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
-- @param which_context: Which context table to use. Either "pre" or "post".
-- For the previous context, n is applied backward (towards the left).
--]]
function AnkiNote:get_context_at_char(n, which_context)
    --                   pre          post  
    -- idx:           ---->x        -->x      
    --                cccccccccwwwwwcccccccccc
    -- n:                  x<--     -->x      
    -- called when initial size `self.context_size` becomes too small.

    assert(type(n) == "number")
    assert(n >= 1)
    assert(which_context == "pre" or which_context == "post")

    local idx -- corresponding index in the context_table (differs from n when going backwards)
    local ch
    if #self.context_table[which_context] < n then self:expand_content() end
    if which_context == "pre" then
        idx = #self.context_table[which_context] - (n - 1) -- one-indexing is not applicable when accessing backwards
    else
        idx = n  -- in context characters are counted from one and the same goes for lua tables, everything is fine
    end
    ch = self.context_table[which_context][idx]
    logger.info("AnkiNote#get_context_at_char() - n", n, "ch", ch, "idx", idx, which_context)
    assert(ch ~= nil, ("Something went wrong when parsing context! idx: %d, context size: %d"):format(idx, #self.context_table[which_context]))
    return ch
end

--[[
-- Returns the n chars of the specified context.
-- @param n: Size of context to get, counted starting from the lookupword, beginning
-- at one (so the first char is char 1).  A value of 0 returns an empty table.
-- @param offset: Offset for starting position (optional)
-- @param which_context: Which context table to use.  Either "pre" or "post".
-- For the previous context, n is applied backward (towards the left).
-- @return: string with context as requested
--]]
function AnkiNote:get_context_of_length(n, which_context, offset)
    --                    pre         post  
    --                cccccccccwwwwwcccccccccc
    --                  <--n-->     <--n-->      

    logger.info("AnkiNote#get_context_of_length() - n", n, "direction", which_context, "offset", offset)
    assert(type(n) == "number")
    assert(n >= 0)
    assert(which_context == "pre" or which_context == "post")

    if not offset then
        offset = 0
    end
    local start, stop -- both start and stop are inclusive
    local len_with_offset = n + offset
    -- Since we are not incrementing character by character, there might be a situation where a
    -- single expansion to double the size is not sufficient (especially when the table is still small)
    while #self.context_table[which_context] < len_with_offset do self:expand_content() end
    if which_context == "post" then
        start = 1 + offset
        stop = len_with_offset
    else
        start = #self.context_table[which_context] - len_with_offset + 1
        stop = #self.context_table[which_context] - offset
    end
    start = start
    stop = stop
    local content = table.concat(self.context_table[which_context], "", start, stop)
    logger.info(string.format("AnkiNote#get_context_of_length() - start %d, stop %d, content '%s'", start, stop, content))
    -- since Japanese characters are multi byte, it is expected that #content > n
    assert(#content >= n, string.format("Something went wrong when retrieving context! n: %d, context size: %d", n, #content))
    assert(type(content) == "string")
    return content
end



--[[
-- Looks up the delimiter marked by the position n_s, n_p in the delimiter table.
-- Note: The function will return and value of zero if the given position lies
-- outside of the delimiter table.  Call init_delimiter_table beforehand if you
-- want to ensure a succesfull lookup.
-- @param n_s: The number of whole sentences. Has to be positive (>=0).
-- @param n_p: The number of sentence parts.  Can also be negative, for any
-- position that would result in a negative length no context is assumed.
-- @param which_context: Which delimiter table to use.  Either "pre" or "post".
-- @return: Index to the table entry containing the delimiter information.
-- An index of 0 means that no delimiter was selected (position s=p=0, or a
-- negative position).
-- An index of -1 means that the delimiter table is not large enough and the
-- position lies outside.
--]]
function AnkiNote:find_delim_from_position(n_s, n_p, which_context)

    -- logger.info("AnkiNote#find_delim_from_position() -", "n_s", n_s, "n_p", n_p, "which_context", which_context)

    assert(which_context == "pre" or which_context == "post")
    assert(type(n_s) == "number")
    assert(n_s >= 0)
    assert(type(n_p) == "number")

    local delim_idx = -1  -- invalid position
    -- determine sentence position
    -- needs to match since even with a negative n_p, we need to know where to count backwards from
    local sentence_count = 0
    if n_s <= 0 then
        delim_idx = 0 -- delimiter index 0 means "no context" / the position points to the edge of the matched word
    else
        for i, delim in ipairs(self.delimiter_table[which_context]) do
            if delim.category == "sentence" then
                sentence_count = sentence_count + 1
                if sentence_count >= n_s then
                    delim_idx = i
                    break
                end
            end
        end
        if sentence_count < n_s then
            -- the delimiter table does not contain n_s sentence delimiters
            delim_idx = -1
        end
    end

    -- determine part-of-sentence position
    if delim_idx >= 0 then
        local part_count = 0
        local next_idx = delim_idx
        local part_incr = 1
        if n_p < 0 then
            part_incr = -1
        end
        while part_count < math.abs(n_p) do
            delim_idx = delim_idx + part_incr
            local delim = self.delimiter_table[which_context][delim_idx]
            if delim_idx <= 0 then
                -- we reached a "no context" position
                delim_idx = 0
                break
            elseif delim_idx > #self.delimiter_table[which_context] then
                -- the delimiter table does not contain n_p part delimiters behind the sentence position
                delim_idx = -1
                break
            end
            if delim.category == "sentence_part" or delim.category == "sentence" then
                part_count = part_count + 1
            end
        end
    end

    return delim_idx
end

--[[
-- Initializes or extends the delimiter table to guarantee it covers the
-- position specified by the parameters n_s, n_p, nc.  Always contains at
-- least one element.
--
-- @param which_context: Which delimiter table to use. Either "pre" or "post".
-- @param n_s: The number of whole sentences. Has to be positive (>=0).
-- @param n_p: The number of sentence parts. Can also be negative, though only
-- positive values are really relevant.
-- @param n_c: Extra character offset from position n_s, n_p (might be large
-- enough to cover one or more additional delimiters). Can also be negative,
-- though the resulting table will always be large enough so that it covers
-- the position n_s, n_p.
--]]
function AnkiNote:init_delimiter_table(n_s, n_p, n_c, which_context)

    -- The delimiter table saves the location of all delimiters up to a certain
    -- position.  Multiple consecutive delimiting characters are treated like a
    -- single delimiter.  For example while "foo),bar" has two delimiting
    -- characters, they really only break the text into two pieces "foo" and
    -- "bar" in one single position in the middle.
    -- Which of these potentially multiple delimiting characters for a single
    -- entry should be included in the context and which not (leading / trailing
    -- characters) is decided by a later step.
    --
    -- The format of the delimiter table is:
    -- `{delimiter, delimiter, ...}`
    -- with a single delimiter being a table `{start, stop, category}`.
    --
    -- * Both `start` and `stop` are inclusive and mark the absolute position
    -- of the first / last delimiting character in that delimiter (as could
    -- be given to get_context_at_char()).
    -- * `category` marks a delimiter as one of
    --   - "whitespace"
    --   - "sentence"
    --   - "sentence_part"


    logger.info("AnkiNote#init_delimiter_table() -", "n_s", n_s, "n_p", n_p, "n_c", n_c, "which_context", which_context)

    assert(which_context == "pre" or which_context == "post", "unknown context '" .. which_context .. "'!")
    assert(type(n_s) == "number")
    assert(type(n_p) == "number")
    assert(type(n_c) == "number")

    local sentence_delimiters = u.to_set(util.splitToChars(self.conf.sentence_delimiters:get_value()))
    local part_of_sentence_delimiters = u.to_set(util.splitToChars(self.conf.part_of_sentence_delimiters:get_value()))
    local whitespace_character = u.to_set(util.splitToChars(" 　〿\t"))

    -- initialize delimiter table
    if not self.delimiter_table then
        self.delimiter_table = {}
    end
    if not self.delimiter_table[which_context] then
        logger.info("AnkiNote#init_delimiter_table() -", "initializing delimiter table")
        self.delimiter_table[which_context] = {}
    end
    local delimiters = self.delimiter_table[which_context]

    -- figure out how much context the current delimiter table already covers
    -- Note: Actually, there might be more text following behind the final delimiter in our table
    -- which we do not know about since we only save the delimiters
    local idx
    if #delimiters == 0 then
        idx = 1
    else
        idx = delimiters[#delimiters].stop + 1
    end
    logger.info("AnkiNote#init_delimiter_table() -", "existing delimiter table covers", idx-1, "chars")

    local function need_to_expand_table(n_s, n_p, n_c, which_context)
        -- ensure the table always has at least one entry
        if #delimiters < 1 then return true end
        -- ensure the table is large enough to cover the position n_s, n_p, n_c
        local delim_idx = self:find_delim_from_position(n_s, n_p, which_context)
        local delim_covered = delim_idx >= 0
        local total_position_covered = delimiters[math.max(1,delim_idx)].stop + n_c <= delimiters[#delimiters].stop
        return not (delim_covered and total_position_covered)
    end

    local current_delimiter

    -- extend delimiter table until it reaches the specified position
    while need_to_expand_table(n_s, n_p, n_c, which_context) do
        local ch = self:get_context_at_char(idx, which_context)
        if part_of_sentence_delimiters[ch] or sentence_delimiters[ch] or whitespace_character[ch] then
            -- we matched a delimiter
            logger.info("AnkiNote#init_delimiter_table() -", "We matched a delimiter:", ch, "at idx", idx)
            if not current_delimiter then
                current_delimiter = {}
                current_delimiter.start = idx
                current_delimiter.stop = idx
                if part_of_sentence_delimiters[ch] then
                    logger.info("AnkiNote#init_delimiter_table() -", "We matched a delimiter:", "new part delimiter mark")
                    current_delimiter.category = "sentence_part"
                elseif sentence_delimiters[ch] then
                    logger.info("AnkiNote#init_delimiter_table() -", "We matched a delimiter:", "new sentence delimiter mark")
                    current_delimiter.category = "sentence"
                elseif whitespace_character[ch] then
                    logger.info("AnkiNote#init_delimiter_table() -", "We matched a delimiter:", "new whitespace mark")
                    current_delimiter.category = "whitespace"
                end
            else
                -- in case several delimiters appear directly next to each other, treat them like a
                -- single delimiter while saving the position of the last one
                current_delimiter.stop = idx
                logger.info("AnkiNote#init_delimiter_table() -", "We matched a delimiter:", "belongs to ", current_delimiter.start, "-", current_delimiter.stop)
                -- whitespace
                if part_of_sentence_delimiters[ch] and current_delimiter.category == "whitespace" then
                    current_delimiter.category = "sentence_part"
                end
                -- In case of something like "foo bar,.", treat it like a sentence delimiter.
                -- A possible case where you might think about doing something else could be if
                -- paired delimiters are involved, for example "'Hello World', he said"
                if sentence_delimiters[ch] and
                (current_delimiter.category == "sentence_part" or current_delimiter.category == "whitespace")
                then
                    current_delimiter.category = "sentence"
                end
            end
        else
            if current_delimiter then
                -- save delimiter to delimiter table
                logger.info("AnkiNote#init_delimiter_table() -", "Continuing with normal text, saving last delimiter mark")
                table.insert(delimiters, current_delimiter)
                current_delimiter = nil
            end
        end
        idx = idx + 1
    end

    assert(#delimiters >= 1)
    logger.info("AnkiNote#init_delimiter_table() -", u.dump(delimiters))
    logger.info("AnkiNote#init_delimiter_table() -", "Finished. New delim table", which_context, "covers", delimiters[#delimiters].stop, "characters")
end

--[[
-- Iterator that iterates through all delimiter characters between the two given
-- delimiters.
-- @param idx_delim_pre: Index of the delimiter on which to start.
-- Assumed to mark a position in the "pre" context.
-- @param idx_delim_post: Index of the delimiter on which to end (inclusive).
-- Assumed to mark a position in the "post" context.
-- @return pos, ch for each delimiter char, nil when finished or no valid entries.
-- * pos: The position of the current entry.  A table with following entries:
--    * which_context      which context (pre / post)
--    * ctx_len            ch is the n-th char in which_context
--    * delim_idx          index of the delimiter in the delimiter table
--    * delim_char_offset  for cases where a delimiter encompasses multiple chars.
--                         A value of 0 = no offset = first char of the delimiter
-- * ch:  The current delimiter character
--]]

function AnkiNote:all_delimiter_chars(idx_delim_pre, idx_delim_post)
    -- check limits of delim table
    assert(idx_delim_pre <= #self.delimiter_table.pre and idx_delim_pre >= 0, "index_delim_pre out of range: " .. idx_delim_pre)
    assert(idx_delim_post <= #self.context_table.post and idx_delim_post >= 0, "idx_delim_post out of range: " .. idx_delim_post)

    -- start with outermost delimiting character of the previous context (= on the very left)
    local direction_inward = true
    local delim_idx = idx_delim_pre
    local delim_char_offset = 0
    if idx_delim_pre < 1 then
        -- start with trailing context
        direction_inward = false
        delim_idx = 1
    end

    local function iterate()
        local which_context
        local incr
        if direction_inward then
            which_context = "pre"
            incr = -1
        else
            which_context = "post"
            incr = 1
        end
        -- logger.info("AnkiNote#determine_exact_context_length()#all_delimiter_chars()#iterator() - ", "CALL", which_context, "delim_char_offset", delim_char_offset, "delim_idx", delim_idx)
        -- check if we are finished
        if (not direction_inward) and delim_idx > idx_delim_post then
            return nil
        end
        -- get current character info
        local ctx_len
        if direction_inward then
            ctx_len = self.delimiter_table[which_context][delim_idx].stop + delim_char_offset
        else
            ctx_len = self.delimiter_table[which_context][delim_idx].start + delim_char_offset
        end
        local ch = self:get_context_at_char(ctx_len, which_context)
        local pos = {}
        pos.which_context = which_context
        pos.ctx_len = ctx_len
        pos.delim_idx = delim_idx
        pos.delim_char_offset = delim_char_offset
        -- setup position of next delimiter character
        -- logger.info("AnkiNote#determine_exact_context_length()#all_delimiter_chars()#iterator() - ", which_context, "delim_char_offset", delim_char_offset, "delim_idx", delim_idx, "ctx_len", ctx_len, "ch", ch)
        delim_char_offset = delim_char_offset + incr -- move to next char
        if not (ctx_len + incr >= self.delimiter_table[which_context][delim_idx].start and
                ctx_len + incr <= self.delimiter_table[which_context][delim_idx].stop) then
            -- move to next delimiter
            delim_idx = delim_idx + incr
            delim_char_offset = 0
        end
        if direction_inward and delim_idx < 1 then
            -- continue with trailing context (the matched word itself does not contain any delimiters)
            direction_inward = false
            delim_idx = 1
        end
        return pos, ch
    end
    return iterate
end


--[[
-- Determines the exact pre / post context length given a pair of delimiters.
-- This is necessary since a delimiter marks a range, not a specific
-- position.
-- Applies smart adjustments to trim or include preceding / trailing characters
-- as needed to get a natural feeling result.
-- This includes a natural handling of quotes depending on whether there is a
-- matching pair or not, and ensuring no undesired characters are included
-- (especially leading delimiters).
-- @param delim_pre: Index of the delimiter marking the start of the position
-- ("pre" side). May be zero to indicate that no context is selected.
-- @param delim_post: Index of the delimiter marking the end of the position
-- ("pre" side). May be zero to indicate that no context is selected. 
-- @return: len_pre, len_post specifying the length of pre / post context in
-- characters.
--]]
function AnkiNote:determine_exact_context_length(delim_pre, delim_post)
    assert(delim_pre >= 0)
    assert(delim_post >= 0)
    logger.info("AnkiNote#determine_exact_context_length():", "pre", delim_pre, "post", delim_post)


    --[[
    -- Adjusts the context outwards to ensure it includes trailing punctuation.
    -- @param idx_delim_pre: Index of the delimiter for the previous context.
    -- @param idx_delim_post: Index of the delimiter for the next context.
    -- @param len_pre: Length of previous context before this adjustment.
    -- @param len_post: Length of post context before this adjustment.
    -- @return Adjusted values len_pre, len_post
    --]]
    local function include_trailing_punctuation(idx_delim_pre, idx_delim_post, len_pre, len_post)
        logger.info("AnkiNote#determine_exact_context_length()#include_trailing_punctuation() - ", idx_delim_pre, idx_delim_post, len_pre, len_post)
        local retain_trailing_delims = u.to_set(util.splitToChars(self.conf.retained_trailing_delimiters:get_value()))
        -- include trailing punctuation that is specified in the setting
        if idx_delim_post > 0 then
            while len_post + 1 <= self.delimiter_table.post[idx_delim_post].stop and
                retain_trailing_delims[self:get_context_at_char(len_post + 1, "post")] do
                len_post = len_post + 1
            end
        end
        return len_pre, len_post
    end

    --[[
    -- Adjusts the context outwards to ensure it includes all delimiting
    -- characters up to the first / last matching paired delimiter character, if
    -- there are any in the specified delimiters.  Has no effect if the context
    -- is already larger than that.
    -- This results in a natural feel when selecting anything with quotes or
    -- parenthesis.
    -- @param idx_delim_pre: Index of the delimiter for the previous context.
    -- @param idx_delim_post: Index of the delimiter for the post context.
    -- @param len_pre: Length of previous context before this adjustment.
    -- @param len_post: Length of post context before this adjustment.
    -- @return Adjusted values len_pre, len_post
    --]]
    local function smart_paired_delimiters(idx_delim_pre, idx_delim_post, len_pre, len_post)
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", idx_delim_pre, idx_delim_post, len_pre, len_post)
   
        -- build paired delimiter information required for parsing
        -- We focus on japanese quotation marks for now, since there are too many different ways how quotation marks are used depending on language, culture and region.
        local paired_delimiters = {"「」", "『』", "（）", "【】", "()", "\"\"", "''"}
        -- build set of opening / closing paired characters
        local opening_pairs = {}
        local closing_pairs = {}
        for _, pair in ipairs(paired_delimiters) do
            -- how do I reference these utils best?
            local chars = require("frontend/util").splitToChars(pair)
            opening_pairs[chars[1]] = true
            closing_pairs[chars[2]] = true
        end
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "opening_pairs")
        logger.info(u.dump(opening_pairs))
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "closing_pairs")
        logger.info(u.dump(closing_pairs))
        -- create a table so that you can find the matching partners to a paired delimiter
        -- example: matching_pairs["("] == ")"
        local matching_pairs = {}
        for i, pair in pairs(paired_delimiters) do
            local chars = require("frontend/util").splitToChars(pair)
            matching_pairs[chars[1]] = chars[2]
            matching_pairs[chars[2]] = chars[1]
        end
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "matching_pairs")
        logger.info(u.dump(matching_pairs))

        -- state
        local opening_pairs_stack = {}
        local pre_first_valid_open
        local post_last_valid_closing
    
        -- parse paired delimiters and save outermost ones that have a proper match
        for pos, ch in self:all_delimiter_chars(idx_delim_pre, idx_delim_post) do
            if opening_pairs[ch] then
                logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - matched opening pair", ch)
                table.insert(opening_pairs_stack, {pos=pos, ch=ch})
                logger.info(u.dump(opening_pairs_stack))
            elseif closing_pairs[ch] then
                -- Checks if matching_pairs is a valid closing pair.
                -- Considers the case of mismatching pairs like (([)), in this case
                -- throws away the offending opening pairs
                -- If no matching opening pair exists at all, just ignore this closing pair
                logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - found a closing pair", ch)
                for i=#opening_pairs_stack, 1, -1 do
                    local opening_delim = opening_pairs_stack[i].ch
                    if matching_pairs[opening_delim] == ch then
                        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - matched a valid closing pair", ch)
                        -- update position of outermost matching pair characters
                        if (not pre_first_valid_open) or
                           (opening_pairs_stack[i].pos.which_context == "pre" and
                           opening_pairs_stack[i].pos.ctx_len > pre_first_valid_open.pos.ctx_len) then
                            pre_first_valid_open = opening_pairs_stack[i]
                        end
                        if (not post_last_valid_closing) or
                           (pos.which_context == "post" and
                           pos.ctx_len > post_last_valid_closing.pos.ctx_len) then
                            post_last_valid_closing = {pos=pos, ch=ch}
                        end
                        -- remove matched opening pair and all following entries
                        while #opening_pairs_stack >= i do
                            opening_pairs_stack[#opening_pairs_stack] = nil
                        end
                    end
                end
            end
        end

        -- apply adjustments
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "first matching opening pair", u.dump(pre_first_valid_open))
        logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "last matching closed pair", u.dump(post_last_valid_closing))
        -- logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "first matching opening pair", pre_first_valid_open.ch, "at ctx_len", pre_first_valid_open.pos.ctx_len, "in", pre_first_valid_open.pos.which_context)
        -- logger.info("AnkiNote#determine_exact_context_length()#parse_pairs() - ", "last matching closing pair", post_last_valid_closing.ch, "at ctx_len", post_last_valid_closing.pos.ctx_len, "in", post_last_valid_closing.pos.which_context)
        -- adjust context outwards to include all delimiting characters up to the first / last matching paired delimiter
        if pre_first_valid_open and 
            pre_first_valid_open.pos.which_context == "pre" then
            len_pre = math.max(len_pre, pre_first_valid_open.pos.ctx_len)
        end
        if post_last_valid_closing and 
            post_last_valid_closing.pos.which_context == "post" then
            len_post = math.max(len_post, post_last_valid_closing.pos.ctx_len)
        end
        return len_pre, len_post
    end

    -- start with no delimiting characters included on either side
    -- note: this implicitely strips any whitespace since any leading / trailing whitespace characters are included as part of the delimiter
    local len_pre = 0
    local len_post = 0
    if delim_pre > 0 then
        len_pre = self.delimiter_table.pre[delim_pre].start - 1
    end
    if delim_post > 0 then
        len_post = self.delimiter_table.post[delim_post].start - 1
    end
    -- apply adjustments for smart behavior
    len_pre, len_post = smart_paired_delimiters(delim_pre, delim_post, len_pre, len_post)
    len_pre, len_post = include_trailing_punctuation(delim_pre, delim_post, len_pre, len_post)
    return len_pre, len_post
end        

--[[
-- Turns an absolute number-of-characters position into a s,p,c position.
-- For example, n_s=0 n_p=5 could be better described as n_s=2 n_p=1.
-- @param n_s: The number of whole sentences. Has to be positive (>=0).
-- @param n_p: The number of sentence parts. Can also be negative.
-- @param which_context: Which context table to use. Either "pre" or "post".
-- @return: pre_s_, pre_p_, pre_c_, post_s_, post_p_, post_c_
-- The returned values are guareed to be positive.
--]]
function AnkiNote:find_optimal_position_mark(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    local ctx_len = {}
    _, _, ctx_len.pre, ctx_len.post = self:get_custom_context(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    logger.info("AnkiNote#find_optimal_position_mark() -", pre_s, pre_p, pre_c, post_s, post_p, post_c)
    logger.info("AnkiNote#find_optimal_position_mark():", pre_s, pre_p, pre_c, post_s, post_p, post_c)

    local function calculate_position(ctx_len, n_ch, which_context)
        logger.info("AnkiNote#find_optimal_position_mark()do_single_context():", which_context)
        -- start at current_position s, p, c
        logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "ctx_len", u.dump(ctx_len))
        -- find s sentence delims to left/right starting at current position
        local sentences = 0
        local idx = 1
        local next_delim = self.delimiter_table[which_context][idx]
        -- the call to get_custom_context ensures the delimiter table is large enough to cover the position s,p,c
        while next_delim and next_delim.start - 1 <= ctx_len[which_context] do
            local delim = next_delim
            if delim.category == "sentence" then
                sentences = sentences + 1
            end
            logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "matching sentences:", "idx", idx, "s", sentences, delim.start, delim.stop, which_context)
            idx = idx + 1
            next_delim = self.delimiter_table[which_context][idx]
            if next_delim then
                logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "next delim:", "start", next_delim.start, "stop", next_delim.stop)
            else
                logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "no next delim")
            end
        end
        -- we are now on the sentence delimiter before current_pos[which_context] (if any)
        -- find p part delims from there (since we decide p it will always be positive)
        local parts = 0
        idx = self:find_delim_from_position(sentences, 1, which_context)
        logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "first part at:", "idx", idx, "p", parts, which_context)
        if idx >= 1 then
            while idx <= #self.delimiter_table[which_context] and
                self.delimiter_table[which_context][idx].start - 1 <= ctx_len[which_context]
            do
                local delim = self.delimiter_table[which_context][idx]
                if delim.category == "sentence_part" then
                    -- check for special case: all delimiters got removed by smart adjustments (last delimiter)
                    if ctx_len[which_context] == delim.start - 1 then
                        if n_ch >= 0 then
                            parts = parts + 1
                        end
                    else
                        -- normal case
                        parts = parts + 1
                    end
                    logger.info("AnkiNote#find_optimal_position_mark()do_single_context() -", "matching parts:", "idx", idx, "p", parts, delim.start, delim.stop, which_context)
                end
                idx = idx + 1
            end
        end
        return sentences, parts
    end

    local pre_s_, pre_p_ = calculate_position(ctx_len, pre_c, "pre")
    local post_s_, post_p_ = calculate_position(ctx_len, post_c, "post")
    -- find char offset adjustments based on calculated position s,p
    local pre_c_, post_c_= 0, 0
    local delim_pre = self:find_delim_from_position(pre_s_, pre_p_, "pre")
    local delim_post = self:find_delim_from_position(post_s_, post_p_, "post")
    local base_len_pre, base_len_post = self:determine_exact_context_length(delim_pre, delim_post)
    logger.info("AnkiNote#find_optimal_position_mark() -", "base len", base_len_pre, base_len_post)
    pre_c_ = ctx_len.pre - base_len_pre
    post_c_ = ctx_len.post - base_len_post
    logger.info("AnkiNote#find_optimal_position_mark() -", "new positions:", pre_s_, pre_p_, pre_c_, post_s_, post_p_, post_c_)
    return pre_s_, pre_p_, pre_c_, post_s_, post_p_, post_c_
end


--[[
-- Returns a new set of s, p, c positions after moving by s_inc, p_inc, c_inc from current_position.
-- @param current_position: The current position (context length) including all adjustments and offsets.
-- @param s_inc: The number of whole sentences to move .
-- @param p_inc: The number of sentence parts.
-- @param c_inc: The number of characters for manual adjustment.
-- @param which_context: Which context table to use. Either "pre" or "post".
-- @return: new_s, new_p, new_c
--]]
-- Note: This might completely make obsolete the need to save a position as s, p, c?
-- just save current_position and get returned a new current_position whenever you apply some adjustments
-- default position is current_position + adjustments for the initial position
function AnkiNote:smart_increment(pos, inc, which_context)
    local pos = self:find_optimal_position_mark(pos, which_context)
    -- TODO: apply increment
    return new_pos
end


--[[
-- Returns the context before and after the lookup word together with the length
-- of the context.  The amount of context depends on the following parameters:
-- @param pre_s: amount of sentences prepended
-- @param pre_p: amount of sentence parts prepended (between comma etc.)
-- @param pre_c: amount of characters prepended
-- @param post_s: amount of sentences appended
-- @param post_p: amount of sentence parts appended (between comma etc.)
-- @param post_c: amount of characters appended
-- @return: prepended_content, appended_content, len_pre, len_post
-- The context is a string, the length specifies how many characters long the
-- piece of context is.  Returning the context length makes working with the
-- obtained context easier, since with multibyte characters #foo does not result
-- in the real length.
--]]
-- Note: How to view characters: characters are not part of the position marker
-- `c,p,s`, but rather the position marker is `p,s` and `c` is a separate
-- correction factor that is applied afterwards and independent from that
-- position
function AnkiNote:get_custom_context(pre_s, pre_p, pre_c, post_s, post_p, post_c)
    logger.info("AnkiNote#get_custom_context():", pre_s, pre_p, pre_c, post_s, post_p, post_c)

    -- find delimiters for specified position
    self:init_delimiter_table(pre_s, pre_p, pre_c, "pre")
    self:init_delimiter_table(post_s, post_p, post_c, "post")
    local delim_pre = self:find_delim_from_position(pre_s, pre_p, "pre")
    local delim_post = self:find_delim_from_position(post_s, post_p, "post")
    logger.info("AnkiNote#get_custom_context() -", "delim_pre", delim_pre, "delim_post", delim_post)

    -- determine context length from delimiters
    local len_pre, len_post = self:determine_exact_context_length(delim_pre, delim_post)
    logger.info("AnkiNote#get_custom_context() -", "len_pre", len_pre, "len_post", len_post, "(after applying adjustments)")

    -- apply context offset for characters as manual correction
    len_pre = len_pre + pre_c
    len_post = len_post + post_c
    logger.info("AnkiNote#get_custom_context() -", "len_pre", len_pre, "len_post", len_post, "(after applying context offset)")

    -- build return values
    local prepended_content = self:get_context_of_length(len_pre, "pre")
    local appended_content = self:get_context_of_length(len_post, "post")
    -- These variables can be used to detect if any content was prepended / appended
    self.has_prepended_content = len_pre > 0
    self.has_appended_content = len_post > 0
    return prepended_content, appended_content, len_pre, len_post
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
    if not self.context_table then
        self.context_table = {}
    end
    if self.context_table.pre and self.context_table.post then
        logger.info(("before reinit: pre table = %d, post table = %d"):format(#self.context_table.pre, #self.context_table.post))
    end
    local skipped_chars = u.to_set(util.splitToChars(("\n\r")))
    local prev_c, next_c = self.ui.highlight:getSelectedWordContext(size)
    -- pass trimmed word context along to be modified
    prev_c = prev_c .. self.word_trim.before
    next_c = self.word_trim.after .. next_c
    self.context_table.pre = {}
    for _, ch in ipairs(util.splitToChars(prev_c)) do
        if not skipped_chars[ch] then table.insert(self.context_table.pre, ch) end
    end
    self.context_table.post = {}
    for _, ch in ipairs(util.splitToChars(next_c)) do
        if not skipped_chars[ch] then table.insert(self.context_table.post, ch) end
    end
    logger.info(("after reinit: pre table = %d, post table = %d"):format(#self.context_table.pre, #self.context_table.post))

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
