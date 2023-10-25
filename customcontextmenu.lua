local Blitbuffer = require("ffi/blitbuffer")
local Button = require("ui/widget/button")
local Device = require("device")
local FrameContainer = require("ui/widget/container/framecontainer")
local FocusManager = require("ui/widget/focusmanager")
local Geom = require("ui/geometry")
local GestureRange = require("ui/gesturerange")
local HorizontalGroup = require("ui/widget/horizontalgroup")
local MovableContainer = require("ui/widget/container/movablecontainer")
local ScrollHtmlWidget = require("ui/widget/scrollhtmlwidget")
local Size = require("ui/size")
local UIManager = require("ui/uimanager")
local VerticalGroup = require("ui/widget/verticalgroup")
local VerticalSpan = require("ui/widget/verticalspan")
local WidgetContainer = require("ui/widget/container/widgetcontainer")
local Screen = Device.screen

local CustomContextMenu = FocusManager:extend{}

local function make_button(text, width, cb, enabled_func)
    enabled_func = enabled_func or function() return true end
    return Button:new{
        text = text,
        radius = 0,
        margin = 2,
        enabled_func = enabled_func,
        width = width,
        show_parent = CustomContextMenu,
        callback = cb,
    }
end

function CustomContextMenu:init()
    self:reset() -- first call is just for initializing
    self.font_size = 19
    local screen_width = Screen:getWidth()
    local screen_height = Screen:getHeight()

    if Device:hasKeys() then
        self.key_events.Close = { { Device.input.group.Back } }
    end
    if Device:isTouchDevice() then
        self.ges_events.Tap = {
            GestureRange:new{
                ges = "tap",
                range = Geom:new{
                    x = 0, y = 0,
                    w = screen_width,
                    h = screen_height,
                }
            },
        }
    end

    local row_span = VerticalSpan:new{ width = Size.padding.fullscreen }
    local frame_width = math.floor(math.min(screen_width, screen_height) * 0.85)
    local frame_height = math.floor(math.min(screen_width, screen_height) * 0.50)
    local frame_border_size = Size.border.window
    local frame_padding = Size.padding.fullscreen
    local inner_width = frame_width - 2 * (frame_border_size + frame_padding)
    local inner_height = frame_height - 2 * (frame_border_size + frame_padding)

    self.scroll_widget = ScrollHtmlWidget:new{
        default_font_size = Screen:scaleBySize(self.font_size),
        width = inner_width,
        height = inner_height,
        dialog = self,
    }
    local button_span_unit_width = Size.span.horizontal_small
    local larger_span_units = 3 -- 3 x small span width
    local nb_span_units = 2 + 2*larger_span_units
    local btn_width = math.floor( ((inner_width+frame_padding) - nb_span_units * button_span_unit_width) * (1/6))

    -- create some helper functions
    local update = function(opts)
        self.pos.pre_c = opts.pre_c or self.pos.pre_c
        self.pos.pre_p = opts.pre_p or self.pos.pre_p
        self.pos.pre_s = opts.pre_s or self.pos.pre_s
        self.pos.post_c = opts.post_c or self.pos.post_c
        self.pos.post_p = opts.post_p or self.pos.post_p
        self.pos.post_s = opts.post_s or self.pos.post_s
        local pos_optimized = table.pack(self.note:find_optimal_position_mark(self.pos.pre_s, self.pos.pre_p, self.pos.pre_c, self.pos.post_s, self.pos.post_p, self.pos.post_c))
        self.pos.pre_s, self.pos.pre_p, self.pos.pre_c = table.unpack(pos_optimized, 1, 3)
        self.pos.post_s, self.pos.post_p, self.pos.post_c = table.unpack(pos_optimized, 4, 6)
        self:update_context()
    end
    local can_prepend = function() return self.note.has_prepended_content end
    local can_append = function() return self.note.has_appended_content end
    local pre_c_inc = function(inc) update({pre_c = self.pos.pre_c + inc}) end
    local post_c_inc = function(inc) update({post_c = self.pos.post_c + inc}) end
    -- char counter is reset to 0 when sentence part count is changed
    local pre_p_inc = function(inc)
        local reset_is_sufficient = (inc > 0 and self.pos.pre_c < 0) or (inc < 0 and self.pos.pre_c > 0)
        if reset_is_sufficient then
            -- just reset to post part-of-sentence if there is a character offset in the opposite direction 
            update({ pre_c = 0})
        else
            update({ pre_c = 0, pre_p = self.pos.pre_p + inc})
        end
    end
    local post_p_inc = function(inc)
        local reset_is_sufficient = (inc > 0 and self.pos.post_c < 0) or (inc < 0 and self.pos.post_c > 0)
        if reset_is_sufficient then
            -- just reset to post part-of-sentence if there is a character offset in the opposite direction
            update({ post_c = 0})
        else
            update({ post_c = 0, post_p = self.pos.post_p + inc})
        end
    end
    
    local pre_s_inc = function(inc)
        if inc == 0 then
            return
        end
        -- Note: Due to position optimisation after each update, position values will be positive (Exception: Fine adjustments that remove one or more delimiting characters compared to the automatic adjustments. Example: "「だから" (0,1,0) -> "だから" (0,1,-1) -> "から" (0,0,2))
        if (inc < 0 and (self.pos.pre_p > 0 or self.pos.pre_c > 0)) or
           (inc > 0 and (self.pos.pre_p == 0 and self.pos.pre_c < 0))
        then
            -- reset to the beginning of the current sentence
            update({ pre_c = 0, pre_p = 0})
        else
            -- move to the next sentence
            update({ pre_c = 0, pre_p = 0, pre_s = math.max(0, self.pos.pre_s + inc)})
        end
    end

    local post_s_inc = function(inc)
        if inc == 0 then
            return
        end
        -- Note: Due to position optimisation after each update, position values will be positive (Exception: Fine adjustments that remove one or more delimiting characters compared to the automatic adjustments. Example: "なんて」" (0,1,0) -> "なんて" (0,1,-1) -> "なん" (0,0,2))
        if (inc < 0 and (self.pos.post_p > 0 or self.pos.post_c > 0)) or
           (inc > 0 and (self.pos.post_p == 0 and self.pos.post_c < 0))
        then
            -- reset to the beginning of the current sentence
            update({ post_c = 0, post_p = 0})
        else
            -- move to the next sentence
            update({ post_c = 0, post_p = 0, post_s = math.max(0, self.pos.post_s + inc)})
        end
    end

    -- buttons for editing previous context
    local remove_pre_sentence_part = make_button("⏩", btn_width, function() pre_p_inc(-1) end , can_prepend)
    local remove_pre_char =          make_button("1-", btn_width, function() pre_c_inc(-1) end, can_prepend)
    local append_pre_char =          make_button("+1", btn_width, function() pre_c_inc(1) end)
    local append_pre_sentence_part = make_button("⏪", btn_width, function() pre_p_inc(1) end)
    local reset_pre =                make_button("Reset", btn_width*2, function() self:reset_pre(); return self:update_context() end)

    -- buttons for editing following context
    local remove_post_sentence_part = make_button("⏪", btn_width, function() post_p_inc(-1) end, can_append)
    local remove_post_char =          make_button("-1", btn_width, function() post_c_inc(-1) end, can_append)
    local append_post_char =          make_button("1+", btn_width, function() post_c_inc(1) end)
    local append_post_sentence_part = make_button("⏩", btn_width, function() post_p_inc(1) end)
    local reset_post =                make_button("Reset", btn_width*2, function() self:reset_post(); self:update_context() end)

    -- holding the ±1 buttons allows to jump back and forth in larger increments 
    remove_pre_char.hold_callback = function() pre_c_inc(- self.note.conf.custom_context_jump_size:get_value()) end
    append_pre_char.hold_callback = function() pre_c_inc(  self.note.conf.custom_context_jump_size:get_value()) end
    remove_post_char.hold_callback = function() post_c_inc(- self.note.conf.custom_context_jump_size:get_value()) end
    append_post_char.hold_callback = function() post_c_inc(  self.note.conf.custom_context_jump_size:get_value()) end

    -- holding the << / >> buttons makes them apply sentences instead of parts of sentences
    remove_pre_sentence_part.hold_callback = function() pre_s_inc(-1) end
    append_pre_sentence_part.hold_callback = function() pre_s_inc( 1) end
    remove_post_sentence_part.hold_callback = function() post_s_inc(-1) end
    append_post_sentence_part.hold_callback = function() post_s_inc( 1) end

    self.top_row = HorizontalGroup:new{
        align = "center",
    append_pre_sentence_part,
    append_pre_char,
    reset_pre,
    remove_pre_char,
    remove_pre_sentence_part,
    }

    self.bottom_row = HorizontalGroup:new{
        align = "center",
    remove_post_sentence_part,
    remove_post_char,
    reset_post,
    append_post_char,
    append_post_sentence_part,
    }

    local save_btn = make_button("Save with custom context", btn_width*4, self.on_save_cb)
    save_btn.hold_callback = function()
        self.ui.AnkiWidget:show_config_widget() -- TODO: reference correct AnkiWidget
    end
    self.confirm_row = HorizontalGroup:new{
        align = "center",
        make_button("Cancel", btn_width*2, function() self:onClose() end),
        save_btn,
    }

    self.context_menu = FrameContainer:new{
        margin = 0,
        bordersize = frame_border_size,
        padding = Size.padding.default,
        radius = Size.radius.window,
        background = Blitbuffer.COLOR_WHITE,
        VerticalGroup:new{
            align = "center",
            self.top_row,
            self.scroll_widget,
            row_span,
            self.bottom_row,
            self.confirm_row,
        }
    }
    self.movable = MovableContainer:new{
        self.context_menu,
    }
    self[1] = WidgetContainer:new{
        align = "center",
        dimen = Geom:new{
            x = 0, y = 0,
            w = screen_width,
            h = screen_height,
        },
        self.movable,
        self.confirm_row,
    }
    self:update_context()
end

function CustomContextMenu:onClose()
    UIManager:close(self)
end

-- used to assure we do a repaint when the menu is closed
function CustomContextMenu:onCloseWidget()
    UIManager:setDirty(nil, function()
        return "flashui", self.context_menu.dimen
    end)
end

-- used to repaint widget when the text is changed
function CustomContextMenu:onShow()
    UIManager:setDirty(self, function()
        return "ui", self.movable.dimen
    end)
end

function CustomContextMenu:onTap(_, ges_ev)
    if not ges_ev.pos:intersectWith(self.context_menu.dimen) then
        self:onClose()
    end
end

function CustomContextMenu:reset()
    self:reset_pre()
    self:reset_post()
end

function CustomContextMenu:reset_pre()
    if not self.pos then
        self.pos = {}
    end
    if self.note.conf.default_context_is_sentence_part:get_value() then
        self.pos.pre_s = 0
        self.pos.pre_p = 1
    else
        self.pos.pre_s = 1
        self.pos.pre_p = 0
    end
    self.pos.pre_c = 0
end

function CustomContextMenu:reset_post()
    if not self.pos then
        self.pos = {}
    end
    if self.note.conf.default_context_is_sentence_part:get_value() then
        self.pos.post_s = 0
        self.pos.post_p = 1
    else
        self.pos.post_s = 1
        self.pos.post_p = 0
    end
    self.pos.post_c = 0
end

function CustomContextMenu:update_context()
    local pre, post, ctx_pre_len, ctx_post_len = self.note:get_custom_context(self.pos.pre_s, self.pos.pre_p, self.pos.pre_c, self.pos.post_s, self.pos.post_p, self.pos.post_c)
    local peek_length = self.note.conf.custom_context_peek_length:get_value()
    local peek_pre = self.note:get_context_of_length(peek_length, "pre", ctx_pre_len)
    local peek_post = self.note:get_context_of_length(peek_length, "post", ctx_post_len)
    local css = [[
        h2 {
            display: inline;
        }
        .lookupword {
            display: inline;
            font-weight: bold;
            background-color: red;
            text-align: center;
        }
        .peek {
            color: #A9A9A9;
        }
        @page {
            margin: 0;
            font-family: 'Noto Sans CJK';
        }
    ]]
    local context_fmt = '<div lang="ja"><p><span class="peek">…%s</span>%s<h2 class="lookupword">%s</h2>%s<span class="peek">%s…</span></p></div>'
    local context = context_fmt:format(peek_pre, pre, self.note.popup_dict.word, post, peek_post)

    self[1]:free()
    self.scroll_widget.htmlbox_widget:setContent(context, css, Screen:scaleBySize(self.font_size))
    self.scroll_widget:resetScroll()
    self:onShow()
end

return CustomContextMenu
