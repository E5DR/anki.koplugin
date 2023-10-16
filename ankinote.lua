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
    return string.format("%s - %s (%d/%d)", meta.author, meta.title, meta:current_page(), meta.pages)
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
    local idx -- corresponding index in the context_table (differs from n when going backwards)
    local ch
    if #self[which_context] < n then self:expand_content() end
    if which_context == "prev_context_table" then
        idx = #self[which_context] - (n - 1) -- one-indexing is not applicable when accessing backwards
    else
        idx = n  -- in context characters are counted from one and the same goes for lua tables, everything is fine
    end
    ch = self[which_context][idx]
    logger.info("AnkiNote#get_context_at_char() - n", n, "ch", ch, "idx", idx)
    assert(ch ~= nil, ("Something went wrong when parsing context! idx: %d, context size: %d"):format(idx, #self[which_context]))
    return ch
end

--[[
-- Returns the n chars of the specified context.
-- @param n: Size of context to get, counted starting from the lookupword, beginning
-- at one (so the first char is char 1). A value of 0 returns an empty table.
-- @param offset: Offset for starting position (optional)
-- @param which_context: Which context table to use. Either "prev_context_table" or
-- "next_context_table". For the previous context, n is applied backward (towards the left).
--]]
function AnkiNote:get_context_of_length(n, which_context, offset)
    --                backwards     forwards  
    -- idx:           ---->x        -->x      
    --                cccccccccwwwwwcccccccccc
    -- n:                  x<--     -->x      

    logger.info("AnkiNote#get_context_of_length() - n", n, "direction", which_context, "offset", offset)
    assert(type(n) == "number")
    assert(n >= 0)
    local offset_
    if not offset then
        offset_ = 0
    end
    local start, stop -- both start and stop are inclusive
    if #self[which_context] < n + offset_ then self:expand_content() end
    if which_context == "next_context_table" then
        start = 1
        stop = n
    else
        start = #self[which_context] - (n - 1)
        stop = #self[which_context]
    end
    start = start + offset_
    stop = stop + offset_
    local content = table.concat(self[which_context], "", start, stop)
    logger.info(string.format("AnkiNote#get_context_of_length() - start %d, stop %d, content '%s'", start, stop, content))
    -- since Japanese characters are multi byte, it is expected that #content > n
    -- TODO: fill the table in a way that every index contains exactly one character so #content == n
    -- TODO: decide: is this function supposed to return a table of characters or a string?
    assert(#content >= n, string.format("Something went wrong when retrieving context! n: %d, context size: %d", n, #content))
    return content
end

--[[
-- Returns the length of the context until the n-th delimiter.
-- A length of 0 means "no context", 1 is "one character of context" and so forth.
-- @param len_offset: Offset for the start of the search (for example obtained from a previous call
-- to this function). It is applied in the direction of the search and included in the returned value
-- (that is, the returned value is absolute, not relative). Like the return value, this is counted from 1
-- @param delim: A set with the delimiters that should be searched for
-- @param n: We want to look for the n-th occurance of delim. Can be negative (useful in combination
-- with len_offset)
-- @param which_context: Which context table to use. Either "prev_context_table" or
-- "next_context_table". For the previous context, the search is conducted backward (towards the left).
--]]
function AnkiNote:calculate_context_length(len_offset, delim, n, which_context)

    logger.info("AnkiNote#calculate_context_length() - len_offset", len_offset, "n", n, "direction_prev", which_context)

    assert(type(len_offset) == "number")
    assert(len_offset >= 0)
    assert(type(n) == "number")
    
    local len_context = len_offset  -- length of context that we looked at so far (absolute)
    local delims_matched = 0         -- number of the limiters that were matched so far
    local len_incr = 1               -- whether we move forward or backward through the context table
    if n < 0 then
        -- for negative n we aim to reduce len_context by n delimiters (not increase)
        len_incr = -1
    end
    local is_first_char = true

    while delims_matched < math.abs(n) do
        local next_index = math.max(1, len_context + len_incr)
        local last_ch
        local ch = self:get_context_at_char(next_index, which_context)
        if delim[ch] then
            if (len_offset > 1 and is_first_char) then
                --  ensure we do not count the same delimiter on which we stopped during a previous call
                --  Note that since the matched word might be the first or last one in a sentence / -part,
                --  we still want to count a delimiter if it is the very first character
                logger.info("AnkiNote#calculate_context_length() - delimiter matched", delims_matched, "ignoring since we matched that last time")
            else
                delims_matched = delims_matched + 1
                logger.info("AnkiNote#calculate_context_length() - delimiter matched, count:", delims_matched)
                -- if which_context == "next_context_table" and retain_trailing_delims[ch] then
                --    break -- do not count the delimiter
                -- end
            end
        end
        len_context = next_index
        is_first_char = false
        last_ch = ch
        if len_incr < 0 and len_context <= 1 then
            -- we went backwards and reached the start of the context
            -- stop here since nothing more is left to do (otherwise the function will be stuck in an endless loop)
            break
        end
    end
    assert(type(len_context) == "number")
    return len_context
end

--[[
-- Returns the context before and after the lookup word, the amount of context depends on the following parameters
-- @param pre_s: amount of sentences prepended
-- @param pre_p: amount of sentence parts prepended (between comma etc.)
-- @param pre_c: amount of characters prepended
-- @param post_s: amount of sentences appended
-- @param post_p: amount of sentence parts appended (between comma etc.)
-- @param post_c: amount of characters appended
--]]
function AnkiNote:get_custom_context(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    logger.info("AnkiNote#get_custom_context():", pre_s, pre_p, pre_c, post_s, post_p, post_c)

    local delims_map_sentence = u.to_set(util.splitToChars(self.conf.sentence_delimiters:get_value()))
    local delims_map_part_of_sentence = u.to_set(util.splitToChars(self.conf.part_of_sentence_delimiters:get_value()))
    local whitespace_map = u.to_set(util.splitToChars(" 　〿\t\n"))
    local retain_trailing_delims = u.to_set(util.splitToChars(self.conf.retained_trailing_delimiters:get_value()))

    -- calculate the length of the context that should be prepended to the lookupword
    local len_ctx_prev = 0
    len_ctx_prev = self:calculate_context_length(len_ctx_prev, delims_map_sentence, pre_s, "prev_context_table")  -- sentences
    len_ctx_prev = self:calculate_context_length(len_ctx_prev, delims_map_part_of_sentence, pre_p, "prev_context_table")  -- parts of sentence
    logger.info("AnkiNote#get_custom_context() - len_ctx_prev", len_ctx_prev)
    if len_ctx_prev > 0 then
        -- do not include the leading delimiter we matched on
        len_ctx_prev = len_ctx_prev - 1
    end
    while len_ctx_prev > 0 do
        -- remove preceding whitespace, going from outward back inside
        -- Note: extract to function, for example trim_whitespace() (both pre and next)
        local ch = self:get_context_at_char(len_ctx_prev, "prev_context_table")
        if whitespace_map[ch] then
            len_ctx_prev = len_ctx_prev - 1
        else
            break
        end
    end
    logger.info("AnkiNote#get_custom_context() - len_ctx_prev", len_ctx_prev)
    len_ctx_prev = len_ctx_prev + pre_c   -- apply context offset for characters
    logger.info("AnkiNote#get_custom_context() - len_ctx_prev", len_ctx_prev)
    local prepended_content = self:get_context_of_length(len_ctx_prev, "prev_context_table")

    -- calculate the length of the context that should be appended to the lookupword
    local len_ctx_next = 0
    len_ctx_next = self:calculate_context_length(len_ctx_next, delims_map_sentence, post_s, "next_context_table")  -- sentences
    len_ctx_next = self:calculate_context_length(len_ctx_next, delims_map_part_of_sentence, post_p, "next_context_table")  -- parts of sentence
    logger.info("AnkiNote#get_custom_context() - len_ctx_next", len_ctx_next)
    if len_ctx_next > 0 then
        -- TODO: long term integrate into get_context_at_char
        -- do not include trailing delimiter, except it is one of these
        local ch = self:get_context_at_char(len_ctx_next, "next_context_table")
        if not retain_trailing_delims[ch] then
            len_ctx_next = len_ctx_next - 1
        end
    end
    logger.info("AnkiNote#get_custom_context() - len_ctx_next", len_ctx_next)
    len_ctx_next = len_ctx_next + post_c -- apply context offset for characters
    logger.info("AnkiNote#get_custom_context() - len_ctx_next", len_ctx_next)
    local appended_content = self:get_context_of_length(len_ctx_next, "next_context_table")

    -- These 2 variables can be used to detect if any content was prepended / appended
    self.has_prepended_content = len_ctx_prev > 0
    self.has_appended_content = len_ctx_next > 0
    return prepended_content, appended_content
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
    logger.info(("(re)initializing context buffer with size: %d"):format(size))
    if self.prev_context_table and self.next_context_table then
        logger.info(("before reinit: prev table = %d, next table = %d"):format(#self.prev_context_table, #self.next_context_table))
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
    logger.info(("after reinit: prev table = %d, next table = %d"):format(#self.prev_context_table, #self.next_context_table))

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
