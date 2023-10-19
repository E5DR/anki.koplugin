local logger = require("logger")
local util = require("util")
local u = require("lua_utils/utils")

local AnkiNote = {
}

--[[
-- Determine trimmed word context for consecutive lookups.
-- When a user updates the text in a dictionary popup window and thus gets a new popup
-- the word selected in the book won't reflect the word in the dictionary.
-- We want to know if last dict lookup is contained in first dict lookup.
-- e.g.: '広大な' -> trimmed to '広大' -> context is '' (before), 'な' (after)
--]]
function AnkiNote:set_word_trim()
    local list = self.popup_dict.window_list
    if #list == 1 then
        return
    end
    local orig, last = list[1].word, list[#list].word
    logger.dbg(("first popup dict: %s, last dict : %s"):format(orig, last))
    local s_idx, e_idx = orig:find(last, 1, true)
    if not s_idx then
        self.contextual_lookup = false
    else
        self.word_trim = { before = orig:sub(1, s_idx-1), after = orig:sub(e_idx+1, #orig) }
    end
end


function AnkiNote:convert_to_HTML(opts)
    local wrapper_template = opts.wrapper_template or "<div class=\"%s\"><ol>%s</ol></div>"
    local entry_template = opts.entry_template or "<li dict=\"%s\">%s</li>"
    local list_items = {}
    for _,entry in ipairs(opts.entries) do
        table.insert(list_items, opts.build(entry, entry_template))
    end
    return wrapper_template:format(opts.class, table.concat(list_items))
end

-- [[
-- Create metadata string about the document the word came from.
-- ]]
function AnkiNote:get_metadata()
    local meta = self.ui.document._anki_metadata
    return string.format("%s - %s (%matching_pairs/%matching_pairs)", meta.author, meta.title, meta:current_page(), meta.pages)
end

function AnkiNote:get_word_context()
    if not self.contextual_lookup then
        return self.popup_dict.word
    end
    local provider = self.ui.document.provider
    if provider == "crengine" then -- EPUB
        local before, after = self:get_custom_context(unpack(self.context))
        return before .. "<b>" .. self.popup_dict.word .. "</b>" .. after
    elseif provider == "mupdf" then -- CBZ
        local ocr_text = self.ui['Mokuro'] and self.ui['Mokuro']:get_selection()
        logger.info("selected text: ", ocr_text)
        -- TODO is trim relevant here?
        return ocr_text or self.popup_dict.word
    end
end

-- [[
-- Expand the context tables to twice their size.
-- Called when initial size `self.context_size` becomes too small.
-- ]]
function AnkiNote:expand_content()
    self.context_size = self.context_size + self.context_size
    self:init_context_buffer(self.context_size)
end

--[[
-- Returns the n-th char of the specified context.
-- Applies safety checks and automatically expands the context table if necessary.
-- @param n: Which char to return, counted starting from the lookupword, beginning
-- at one (so the first char is char 1).
-- @param which_context: Which context table to use. Either "prev_context_table" or
-- "next_context_table". For the previous context, n is applied backward (towards the left).
--]]
function AnkiNote:get_context_at_char(n, which_context)
    --                backwards     forwards  
    -- idx:           ---->x        -->x      
    --                cccccccccwwwwwcccccccccc
    -- n:                  x<--     -->x      
    -- called when initial size `self.context_size` becomes too small.

    assert(type(n) == "number")
    assert(n >= 1)
    assert(which_context == "prev_context_table" or which_context == "next_context_table")

    local idx -- corresponding index in the context_table (differs from n when going backwards)
    local ch
    if #self[which_context] < n then self:expand_content() end
    if which_context == "prev_context_table" then
        idx = #self[which_context] - (n - 1) -- one-indexing is not applicable when accessing backwards
    else
        idx = n  -- in context characters are counted from one and the same goes for lua tables, everything is fine
    end
    ch = self[which_context][idx]
    logger.info("AnkiNote#get_context_at_char() - n", n, "ch", ch, "idx", idx, "which_context", which_context)
    assert(ch ~= nil, ("Something went wrong when parsing context! idx: %matching_pairs, context size: %matching_pairs"):format(idx, #self[which_context]))
    return ch
end

--[[
-- Returns the n chars of the specified context.
-- @param n: Size of context to get, counted starting from the lookupword, beginning
-- at one (so the first char is char 1). A value of 0 returns an empty table.
-- @param offset: Offset for starting position (optional)
-- @param which_context: Which context table to use. Either "prev_context_table" or
-- "next_context_table". For the previous context, n is applied backward (towards the left).
-- @return string with context as requested
--]]
function AnkiNote:get_context_of_length(n, which_context, offset)
    --                    prev        next  
    --                cccccccccwwwwwcccccccccc
    --                  <--n-->     <--n-->      

    logger.info("AnkiNote#get_context_of_length() - n", n, "direction", which_context, "offset", offset)
    assert(type(n) == "number")
    assert(n >= 0)
    assert(which_context == "prev_context_table" or which_context == "next_context_table")

    if not offset then
        offset = 0
    end
    local start, stop -- both start and stop are inclusive
    local len_with_offset = n + offset
    -- Since we are not incrementing character by character, there might be a situation where a
    -- single expansion to double the size is not sufficient (especially when the table is still small)
    while #self[which_context] < len_with_offset do self:expand_content() end
    if which_context == "next_context_table" then
        start = 1 + offset
        stop = len_with_offset
    else
        start = #self[which_context] - len_with_offset + 1
        stop = #self[which_context] - offset
    end
    start = start
    stop = stop
    local content = table.concat(self[which_context], "", start, stop)
    logger.info(string.format("AnkiNote#get_context_of_length() - start %matching_pairs, stop %matching_pairs, content '%s'", start, stop, content))
    -- since Japanese characters are multi byte, it is expected that #content > n
    assert(#content >= n, string.format("Something went wrong when retrieving context! n: %matching_pairs, context size: %matching_pairs", n, #content))
    assert(type(content) == "string")
    return content
end

--[[
-- Finds and returns the absolute position of the n-th occurence of one of the characters specified by delim.
-- Also returns the delimiter that was matched
-- Searching for the 0-th occurrence returns 0, as does a search with negative n that does not yield a result.
-- @param len_offset: Offset for the start of the search (for example obtained from a previous call
-- to this function). It is applied in the direction of the search. 1-indexed like the position.
-- @param delim: A set with the delimiters that should be matched.
-- @param n: We want to look for the n-th occurance of delim. Can be negative for a backwards search
-- (useful in combination with len_offset).
-- @param which_context: Which context table to use. Either "prev_context_table" or "next_context_table".
-- For the previous context, the search is conducted backward (towards the left).
-- @return int, char
--]]
function AnkiNote:get_pos_of_next_delim(delim, n, which_context, offset)

    logger.info("AnkiNote#get_pos_of_next_delim() - len_offset", offset, "n", n, "direction_prev", which_context)

    assert(type(offset) == "number")
    assert(offset >= 0)
    assert(type(n) == "number")

    local idx = offset
    local delims_matched = 0         -- number of the limiters that were matched so far
    local len_incr = 1               -- whether we move forward or backward through the context table
    if n < 0 then
        -- for negative n we aim to reduce len_context by n delimiters (not increase)
        len_incr = -1
    end

    while delims_matched < math.abs(n) do
        local next_idx = math.max(1, idx + len_incr)
        local ch = self:get_context_at_char(next_idx, which_context)
        if delim[ch] then
            delims_matched = delims_matched + 1
            logger.info("AnkiNote#get_pos_of_next_delim() - delimiter matched, count:", delims_matched)
        end
        idx = next_idx
        if len_incr < 0 and idx <= 1 then
            -- we went backwards and reached the start of the context
            -- stop here since nothing more is left to do (otherwise the function will be stuck in an endless loop)
            break
        end
    end
    assert(type(idx) == "number")
    local ch
    if idx > 0 then
        ch = self:get_context_at_char(idx, which_context) -- Note: can we just reuse ch from the loop?
    end
    return idx, ch
end


--[[
-- Converts the position of a character in the context from one marked by a number of sentences and
-- sentence parts into an absolute position counted in number of characters (inclusive).
-- @param n_s: The number of whole sentences to match. Has to be positive (>=0).
-- @param n_p: The number of sentence parts to match. Can also be negative (for example 2 sentences
-- and one sentence part back from there).
-- @param which_context: Which context table to use. Either "prev_context_table" or "next_context_table".
-- For the previous context, the search is conducted backward (towards the left).
-- @param len_offset: Offset for the start of the search (for example obtained from a previous call
-- to this function). It is applied in the direction of the search and included in the returned value
-- (since that is absolute). Optional (default value: 0).
-- @return: int
-- A length of 0 means "no context", 1 is "one character of context" and so forth.
-- (that is, the returned value is absolute, not relative).
--]]
function AnkiNote:calculate_context_length(n_s, n_p, which_context, len_offset)

    logger.info("AnkiNote#calculate_context_length() - ", "n_s", n_s, "n_p", n_p, "which_context", which_context, "len_offset", len_offset)

    assert(type(n_s) == "number")
    assert(n_s >= 0)
    assert(type(n_p) == "number")


    -- DEBUG
    function dump(o)
        if type(o) == 'table' then
           local s = '{ '
           for k,v in pairs(o) do
              if type(k) ~= 'number' then k = '"'..k..'"' end
              s = s .. '['..k..'] = ' .. dump(v) .. ','
           end
           return s .. '} '
        else
           return tostring(o)
        end
     end


    -- Note: to implement any sort of smart behavior, I will probably have to go through delimiters one by one
    -- implement as state machine
    local sentence_delimiters = u.to_set(util.splitToChars(self.conf.sentence_delimiters:get_value()))
    -- TODO: have the setting contain only the sentence part delimiters that are not already a sentence delimiter
    local part_of_sentence_delimiters = u.to_set(util.splitToChars(self.conf.part_of_sentence_delimiters:get_value()))
    -- paired delimiters are handled separately from this
    local retain_trailing_delims = u.to_set(util.splitToChars(self.conf.retained_trailing_delimiters:get_value()))

    local len_context = len_offset
    if not len_context then
        len_context = 0
    end
    assert(type(len_context) == "number")
    assert(len_context >= 0)
    local is_first_char = true
   
    local function calculate_single_step(delimiters, n, offset)
        logger.info("AnkiNote#calculate_context_length()#calculate_single_step() - ", "n", n, "offset", offset)
        local direction_outward = n > 0    -- n > 0
        local idx = offset
        local len_incr = 1               -- whether to move forward (1) or backward (-1)
        if not direction_outward then
            len_incr = -1 -- for negative n we aim to reduce the context by n delimiters (not increase)
        end
        local n_delims = 0

        for i = 1, math.abs(n) do
            matched_a_complete_pair = false
            local matching_pairs
            local delim_offset
            if is_first_char then
                delim_offset = idx
            else
                --  ensure we do not count the same delimiter on which we stopped the last time
                delim_offset = idx + len_incr
            end
            idx, matching_pairs = self:get_pos_of_next_delim(delimiters, len_incr, which_context, delim_offset)
            n_delims = i
            -- smarter handling of pairs
            matched_a_complete_pair = parse_pairs(matching_pairs, idx, direction_outward)
            if (not direction_outward) and idx <= 0 then
                -- we went backwards and reached the start of the context
                -- stop here since nothing more is left to do
                break
            end
            is_first_char = false
        end
        return idx
    end

    len_context = calculate_single_step(sentence_delimiters, n_s, 0)  -- sentences
    len_context = calculate_single_step(part_of_sentence_delimiters, n_p, len_context)  -- parts of sentence

    if len_context > 0 then
        local final_delimiter = self:get_context_at_char(len_context, which_context)
        -- do not include final delimiter
        logger.info("AnkiNote#calculate_context_length() - remove final delimiter", final_delimiter)
        len_context = math.max(0, len_context - 1)
    end

    assert(type(len_context) == "number")
    return len_context
end



--[[
-- Returns the optimal way a position can be reached.
-- For example, n_s=0 n_p=5 could be better described as n_s=2 n_p=1.
-- @param n_s: The number of whole sentences. Has to be positive (>=0).
-- @param n_p: The number of sentence parts. Can also be negative.
-- @param which_context: Which context table to use. Either "prev_context_table" or "next_context_table".
-- @return: n_s_optimal, n_p_optimal
--]]
function AnkiNote:optimize_position(n_s, n_p, which_context, len_offset)
-- TODO: implement
end


--[[
-- Returns the context before and after the lookup word together with the length of the context.
-- The amount of context depends on the following parameters
-- @param pre_s: amount of sentences prepended
-- @param pre_p: amount of sentence parts prepended (between comma etc.)
-- @param pre_c: amount of characters prepended
-- @param post_s: amount of sentences appended
-- @param post_p: amount of sentence parts appended (between comma etc.)
-- @param post_c: amount of characters appended
-- @return: prepended_content, appended_content, len_prev, len_next
-- The context is a string, the length specifies how many characters long the piece of context is.
-- Returning the context length makes working with the obtained context easier, since with multibyte
-- characters #foo does not result in the real length.
--]]
-- How to view characters: characters are not part of the position marker `c,p,s`, but rather the position marker is `p,s` and `c` is a separate correction factor that is applied afterwards and independent from that position

function AnkiNote:get_custom_context(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    logger.info("AnkiNote#get_custom_context():", pre_s, pre_p, pre_c, post_s, post_p, post_c)

    local function trim_whitespace(len_prev, len_next)
        local whitespace_map = u.to_set(util.splitToChars(" 　〿\t\n"))
        -- remove preceding whitespace, going from outward back inside
        while len_prev > 0 do
            local ch = self:get_context_at_char(len_prev, "prev_context_table")
            if whitespace_map[ch] then
                len_prev = len_prev - 1
            else
                break
            end
        end
        -- TODO: remove trailing whitespace, going from outward back inside
        return len_prev, len_next
    end

    local function allowed_trailing_delimiters(len_prev, len_next)
        -- TODO:
        return len_prev, len_next
    end

    local function smart_paired_delimiters(len_prev, len_next)
        -- TODO:


        local opening_pairs_stack = {}
        local matched_a_complete_pair = false
    
        -- We focus on japanese quotation marks for now, since there are too many different ways how quotation marks are used depending on language, culture and region.
        local paired_delimiters = {{"「", "」"}, {"『", "』"}, {"（", "）"},
                                    {"【", "】"}, { "(", ")" }, {"\"", "\""},
                                    { "'", "'" },}
        local opening = {}
        local closing = {}
        for _, pair in ipairs(paired_delimiters) do
            table.insert(opening, pair[1])
            table.insert(closing, pair[2])
        end
        local opening_pairs = u.to_set(opening)
        local closing_pairs = u.to_set(closing)
        logger.info("AnkiNote#calculate_context_length()#parse_pairs() - ", "opening_pairs")
        logger.info(dump(opening_pairs))
        logger.info("AnkiNote#calculate_context_length()#parse_pairs() - ", "closing_pairs")
        logger.info(dump(closing_pairs))
        -- create a table so that you can find the matching partners to a paired delimiter
        -- example: matching_pairs["("] == ")"
        local matching_pairs = {}
        for i, pair in pairs(paired_delimiters) do
            matching_pairs[pair[1]] = pair[2]
            matching_pairs[pair[2]] = pair[1]
        end
        logger.info("AnkiNote#calculate_context_length()#parse_pairs() - ", "matching_pairs")
        logger.info(dump(matching_pairs))

        local adj_len_prev = len_prev
        local adj_len_next = len_next
        local complete_context = create_complete_context(len_prev, len_next)
        -- check for adjacent paired delimiters and include them
        while opening_pairs[self:get_context_at_char(adj_len_prev + 1, "prev_context_table")] do
            adj_len_prev = adj_len_prev + 1
        end
        while closing_pairs[self:get_context_at_char(adj_len_next + 1, "next_context_table")] do
            adj_len_next = adj_len_next + 1
        end
        -- We do not need to check for the size of the context table since that was already done when calling get_context_at_char()
        -- TODO: get and join (as table)
    
        -- TODO:
        for i, ch in ipairs(complete_context) do
            local matched_a_complete_pair = false
            if opening_pairs[matching_pairs] then
                logger.info("AnkiNote#calculate_context_length()#parse_pairs() - matched opening pair", matching_pairs)
                table.insert(opening_pairs_stack, {idx=i, char=matching_pairs})
                logger.info(dump(opening_pairs_stack))
            elseif closing_pairs[matching_pairs] then
                -- Checks if matching_pairs is a valid closing pair.
                -- Considers the case of mismatching pairs like (([)), in this case
                -- throws away the offending opening pairs
                -- If no matching opening pair exists at all, just ignore this closing pair
                logger.info("AnkiNote#calculate_context_length()#parse_pairs() - found a closing pair", matching_pairs)
                for i=#opening_pairs_stack, 1, -1 do
                    local opening_delim = opening_pairs_stack[i].char
                    if matching_pairs[opening_delim] == matching_pairs then
                        -- remove matched par and all following entries
                        while #opening_pairs_stack >= i do 
                            opening_pairs_stack[#opening_pairs_stack] = nil
                        end
                        logger.info("AnkiNote#calculate_context_length()#parse_pairs() - matched a valid closing pair", matching_pairs)
                        matched_a_complete_pair = true
                    end
                end
            end
        end
        if matched_a_complete_pair then
            logger.info("AnkiNote#calculate_context_length()#parse_pairs() - matched a complete pair")
        end
        
        -- TODO: modify len_prev, len_next
        if len_context > 0 then
        logger.info("AnkiNote#calculate_context_length() -", "matched_a_complete_pair", matched_a_complete_pair)
        if not matched_a_complete_pair then
            len_context = math.max(0, len_context - 1)
        end

        return len_prev, len_next
    end


    -- calculate basic context length to s,p position without adjustments
    -- final matched delimiter is not included
    local len_prev = self:calculate_context_length(pre_s, pre_p, "prev_context_table")
    local len_next = self:calculate_context_length(post_s, post_p, "next_context_table")
    logger.info("AnkiNote#get_custom_context() -", "len_prev", len_prev, "len_next", len_next, "(without adjustments)")

    -- apply adjustments for smart behavior
    len_prev, len_next = allowed_trailing_delimiters(len_prev, len_next) -- remove all non-paired delims that are not allowed
    len_prev, len_next = trim_whitespace(len_prev, len_next)
    len_prev, len_next = smart_paired_delimiters(len_prev, len_next) -- remove paired delims that do not have a match, auto-extend beyond a trailing "normal delimiter"
    logger.info("AnkiNote#get_custom_context() -", "len_prev", len_prev, "len_next", len_next, "(after applying adjustments)")

    -- apply context offset for characters as manual correction
    len_prev = len_prev + pre_c
    len_next = len_next + post_c
    logger.info("AnkiNote#get_custom_context() -", "len_prev", len_prev, "len_next", len_next, "(after applying context offset)")

    -- build return values
    local prepended_content = self:get_context_of_length(len_prev, "prev_context_table")
    local appended_content = self:get_context_of_length(len_next, "next_context_table")
    -- These 2 variables can be used to detect if any content was prepended / appended
    self.has_prepended_content = len_prev > 0
    self.has_appended_content = len_next > 0
    return prepended_content, appended_content, len_prev, len_next
end

function AnkiNote:get_picture_context()
    local meta = self.ui.document._anki_metadata
    if not meta then
        return
    end
    local provider, plugin = self.ui.document.provider, self.ui['Mokuro']
    -- we only add pictures for CBZ (handled by ocr_popup widget)
    if provider == "mupdf" and plugin then
        local fn = string.format("%s/%s_%s.jpg", self.settings_dir, meta.title, os.date("%Y-%m-%d %H-%M-%S"))
        return plugin:get_context_picture(fn) and fn or nil
    end
end

function AnkiNote:run_extensions(note)
    for _, extension in ipairs(self.extensions) do
        note = extension:run(note)
    end
    return note
end

function AnkiNote:get_definition()
    return self:convert_to_HTML {
        entries = { self.popup_dict.results[self.popup_dict.dict_index] },
        class = "definition",
        build = function(entry, entry_template)
            local def = entry.definition
            if entry.is_html then -- try adding dict name to opening div tag (if present)
                -- gsub wrapped in () so it only gives us the first result, and discards the index (2nd arg.)
                return (def:gsub("(<div)( ?)", string.format("%%1 dict=\"%s\"%%2", entry.dict), 1))
            end
            return entry_template:format(entry.dict, (def:gsub("\n", "<br>")))
        end
    }
end

function AnkiNote:build()
    local fields = {
        [self.word_field:get_value()] = self.popup_dict.word,
        [self.def_field:get_value()] = self:get_definition()
    }
    local optional_fields = {
        [self.context_field] = function() return self:get_word_context() end,
        [self.meta_field]    = function() return self:get_metadata() end,
    }
    for opt,fn in pairs(optional_fields) do
        local field_name = opt:get_value()
        if field_name then
            fields[field_name] = fn()
        end
    end
    local note = {
        -- some fields require an internet connection, which we may not have at this point
        -- all info needed to populate them is stored as a callback, which is called when a connection is available
        _field_callbacks = {
            audio = { func = "set_forvo_audio", args = { self.popup_dict.word, self:get_language() } },
            picture = { func = "set_image_data", args = { self:get_picture_context() } },
        },
        deckName = self.deckName:get_value(),
        modelName = self.modelName:get_value(),
        fields = fields,
        options = {
            allowDuplicate = self.allow_dupes:get_value(),
            duplicateScope = self.dupe_scope:get_value(),
        },
        tags = self.tags,
    }
    local note_extended = self:run_extensions(note)
    return { action = "addNote", params = { note = note_extended }, version = 6 }
end

function AnkiNote:get_language()
    local ifo_lang = self.selected_dict.ifo_lang
    return ifo_lang and ifo_lang.lang_in or self.ui.document:getProps().language
end

function AnkiNote:init_context_buffer(size)
    logger.info(("(re)initializing context buffer with size: %matching_pairs"):format(size))
    if self.prev_context_table and self.next_context_table then
        logger.info(("before reinit: prev table = %matching_pairs, next table = %matching_pairs"):format(#self.prev_context_table, #self.next_context_table))
    end
    local skipped_chars = u.to_set(util.splitToChars(("\n\r")))
    local prev_c, next_c = self.ui.highlight:getSelectedWordContext(size)
    -- pass trimmed word context along to be modified
    prev_c = prev_c .. self.word_trim.before
    next_c = self.word_trim.after .. next_c
    self.prev_context_table = {}
    for _, ch in ipairs(util.splitToChars(prev_c)) do
        if not skipped_chars[ch] then table.insert(self.prev_context_table, ch) end
    end
    self.next_context_table = {}
    for _, ch in ipairs(util.splitToChars(next_c)) do
        if not skipped_chars[ch] then table.insert(self.next_context_table, ch) end
    end
    logger.info(("after reinit: prev table = %matching_pairs, next table = %matching_pairs"):format(#self.prev_context_table, #self.next_context_table))

end

function AnkiNote:set_custom_context(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    self.context = { pre_s, pre_p, pre_c, post_s, post_p, post_c }

end

function AnkiNote:add_tags(tags)
    for _,t in ipairs(tags) do
        table.insert(self.tags, t)
    end
end

-- each user extension gets access to the AnkiNote table as well
function AnkiNote:load_extensions()
    self.extensions = {}
    local extension_set = u.to_set(self.enabled_extensions:get_value())
    for _, ext_filename in ipairs(self.ext_modules) do
        if extension_set[ext_filename] then
            local module = self.ext_modules[ext_filename]
            table.insert(self.extensions, setmetatable(module, { __index = function(t, v) return rawget(t, v) or self[v] end }))
        end
    end
end

-- This function should be called before using the 'class' at all
function AnkiNote:extend(opts)
    -- conf table is passed along to DictEntryWrapper
    self.conf = opts.conf
    -- settings are inserted in self table directly for easy access
    for k,v in pairs(opts.conf) do
        self[k] = v
    end
    -- dict containing various settings about the current state
    self.ui = opts.ui
    -- used to save screenshots in (CBZ only)
    self.settings_dir = opts.settings_dir
    -- used to store extension functions to run
    self.ext_modules = opts.ext_modules
    return self
end


function AnkiNote:new(popup_dict)
    local new = {
        context_size = 50,
        popup_dict = popup_dict,
        selected_dict = popup_dict.results[popup_dict.dict_index],
        -- indicates that popup_dict relates to word in book
        contextual_lookup = true,
        word_trim = { before = "", after = "" },
        tags = { "KOReader" },
    }
    local new_mt = {}
    function new_mt.__index(t, v)
        return rawget(t, v) or self[v]
    end

    local note = setmetatable(new, new_mt)
    note:set_word_trim()
    note:load_extensions()
    -- TODO this can be delayed
    note:init_context_buffer(note.context_size)
    if self.conf.default_context_is_sentence_part:get_value() then
        note:set_custom_context(0, 1, 0, 0, 1, 0)
    else
        note:set_custom_context(1, 0, 0, 1, 0, 0)
    end
    return note
end

return AnkiNote
