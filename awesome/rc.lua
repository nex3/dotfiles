-- awesome 3 configuration file

-- Include awesome library, with lots of useful function!
require("awful")
require("tabulous")
require("beautiful")

-- {{{ Variable definitions
-- This is a file path to a theme file which will defines colors.
theme_path = "/usr/local/share/awesome/themes/default"

-- This is used later as the default terminal to run.
terminal = "x-terminal-emulator"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod3"
modkey2 = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    "tile",
    "tileleft",
    "tilebottom",
    "tiletop",
    "fairh",
    "fairv",
    "spiral",
    "dwindle",
    "floating",
    "max",
}

-- Table of clients that should be set floating. The index may be either
-- the application class or instance. The instance is useful when running
-- a console app in a terminal like (Music on Console)
--    xterm -name mocp -e mocp
floatapps =
{
    -- by class
    ["MPlayer"] = true,
    ["pinentry"] = true,
    ["gimp"] = true,
    -- by instance
    ["mocp"] = true
}

-- Applications to be moved to a pre-defined tag by class or instance.
-- Use the screen and tags indices.
apptags =
{
    -- ["Firefox"] = { screen = 1, tag = 2 },
    -- ["mocp"] = { screen = 2, tag = 4 },
}

-- Names of tags
tagnames =
{
    "check",
    "prog",
    "hw",
    "im",
    "music",
}

-- Define if we want to use titlebar on all applications.
use_titlebar = false
-- }}}

-- {{{ Initialization
-- Initialize theme (colors).
beautiful.init(theme_path)

-- Register theme in awful.
-- This allows to not pass plenty of arguments to each function
-- to inform it about colors we want it to draw.
awful.beautiful.register(beautiful)

-- Uncomment this to activate autotabbing
-- tabulous.autotab_start()
-- }}}

-- {{{ Tags
-- Define tags table.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = {}
    -- Create 9 tags per screen.
    for tagnumber = 1, 9 do
        tags[s][tagnumber] = tag({ name = tagnames[tagnumber] or tagnumber, layout = layouts[1] })
        -- Add tags to screen one by one
        tags[s][tagnumber].screen = s
    end
    -- I'm sure you want to see at least one tag.
    tags[s][1].selected = true
end

namedtags = {}
for s = 1, screen.count() do
    namedtags[s] = {}
    for i, n in ipairs(tagnames) do
        namedtags[s][n] = tags[s][i]
    end
end
-- }}}

-- {{{ Statusbar
-- Create a taglist widget
mytaglist = widget({ type = "taglist", name = "mytaglist" })
mytaglist:buttons({
    button({ }, 1, function (object, tag) awful.tag.viewonly(tag) end),
    button({ modkey }, 1, function (object, tag) awful.client.movetotag(tag) end),
    button({ }, 3, function (object, tag) tag.selected = not tag.selected end),
    button({ modkey }, 3, function (object, tag) awful.client.toggletag(tag) end),
    button({ }, 4, awful.tag.viewnext),
    button({ }, 5, awful.tag.viewprev)
})
mytaglist.label = awful.widget.taglist.label.all

-- Create a tasklist widget
mytasklist = widget({ type = "tasklist", name = "mytasklist" })
mytasklist:buttons({
    button({ }, 1, function (object, c) client.focus = c; c:raise() end),
    button({ }, 4, function () awful.client.focusbyidx(1) end),
    button({ }, 5, function () awful.client.focusbyidx(-1) end)
})
mytasklist.label = awful.widget.tasklist.label.currenttags

-- Create a textbox widget
mytextbox = widget({ type = "textbox", name = "mytextbox", align = "right" })
-- Set the default text in textbox
mytextbox.text = "<b><small> awesome " .. AWESOME_VERSION .. " </small></b>"
mypromptbox = widget({ type = "textbox", name = "mypromptbox", align = "left" })

-- Create a launcher widget
mylauncher = awful.widget.launcher({ name = "mylauncher",
                                     image = "/usr/local/share/awesome/icons/awesome16.png",
                                     command = terminal .. " -e man awesome"})

-- Create a systray
mysystray = widget({ type = "systray", name = "mysystray", align = "right" })

-- Create an iconbox widget which will contains an icon indicating which layout we're using.
-- We need one layoutbox per screen.
mylayoutbox = {}
for s = 1, screen.count() do
    mylayoutbox[s] = widget({ type = "imagebox", name = "mylayoutbox", align = "right" })
    mylayoutbox[s]:buttons({
        button({ }, 1, function () awful.layout.inc(layouts, 1) end),
        button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        button({ }, 4, function () awful.layout.inc(layouts, 1) end),
        button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    })
    mylayoutbox[s].image = image("/usr/local/share/awesome/icons/layouts/tilew.png")
end

-- Create a statusbar for each screen and add it
mystatusbar = {}
for s = 1, screen.count() do
    mystatusbar[s] = wibox({ position = "top", name = "mystatusbar" .. s,
                             fg = beautiful.fg_normal, bg = beautiful.bg_normal })
    -- Add widgets to the statusbar - order matters
    mystatusbar[s]:widgets({
        mytaglist,
        mytasklist,
        myiconbox,
        mypromptbox,
        mytextbox,
        mylayoutbox[s],
        s == 1 and mysystray or nil
    })
    mystatusbar[s].screen = s
end
-- }}}

-- {{{ Mouse bindings
awesome.buttons({
    button({ }, 3, function () awful.spawn(terminal) end),
    button({ }, 4, awful.tag.viewnext),
    button({ }, 5, awful.tag.viewprev)
})
-- }}}

-- {{{ Key bindings

-- Bind keyboard digits
-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

function tagkey(mod, char, tagfn)
    keybinding({ mod }, char,
                   function ()
                       local screen = mouse.screen
                       if tagfn(screen) then
                           awful.tag.viewonly(tagfn(screen))
                       end
                   end):add()
    keybinding({ mod, "Control" }, char,
                   function ()
                       local screen = mouse.screen
                       if tagfn(screen) then
                           tagfn(screen).selected = not tagfn(screen).selected
                       end
                   end):add()
    keybinding({ mod, "Shift" }, char,
                   function ()
                       if client.focus then
                           if tagfn(client.focus.screen) then
                               awful.client.movetotag(tagfn(client.focus.screen))
                           end
                       end
                   end):add()
    keybinding({ mod, "Control", "Shift" }, char,
                   function ()
                       if client.focus then
                           if tagfn(client.focus.screen) then
                               awful.client.toggletag(tagfn(client.focus.screen))
                           end
                       end
                   end):add()
end

for i = 1, keynumber do
    tagkey(modkey, i, function(screen) return tags[screen][i] end)
end

for i, n in ipairs(tagnames) do
    tagkey(modkey2, n:sub(1, 1), function(screen) return namedtags[screen][n] end)
end

keybinding({ modkey }, "Left", awful.tag.viewprev):add()
keybinding({ modkey }, "Right", awful.tag.viewnext):add()
keybinding({ modkey }, "Escape", awful.tag.history.restore):add()

-- Standard program
keybinding({ modkey }, "t", function () awful.spawn(terminal) end):add()

keybinding({ modkey, "Control" }, "r", awesome.restart):add()
keybinding({ modkey, "Shift" }, "q", awesome.quit):add()

-- Client manipulation
keybinding({ modkey }, "m", awful.client.maximize):add()
keybinding({ modkey, "Shift" }, "c", function () client.focus:kill() end):add()
keybinding({ modkey }, "k", function () awful.client.focusbyidx(1); client.focus:raise() end):add()
keybinding({ modkey }, "l", function () awful.client.focusbyidx(-1);  client.focus:raise() end):add()
keybinding({ modkey, "Shift" }, "k", function () awful.client.swap(1) end):add()
keybinding({ modkey, "Shift" }, "l", function () awful.client.swap(-1) end):add()
keybinding({ modkey, "Control" }, "k", function () awful.screen.focus(1) end):add()
keybinding({ modkey, "Control" }, "l", function () awful.screen.focus(-1) end):add()
keybinding({ modkey, "Control" }, "space", awful.client.togglefloating):add()
keybinding({ modkey, "Control" }, "Return", function () client.focus:swap(awful.client.master()) end):add()
keybinding({ modkey }, "o", awful.client.movetoscreen):add()
keybinding({ modkey }, "Tab", awful.client.focus.history.previous):add()
keybinding({ modkey }, "u", awful.client.urgent.jumpto):add()
keybinding({ modkey, "Shift" }, "r", function () client.focus:redraw() end):add()

-- Layout manipulation
keybinding({ modkey }, ";", function () awful.tag.incmwfact(0.05) end):add()
keybinding({ modkey }, "j", function () awful.tag.incmwfact(-0.05) end):add()
keybinding({ modkey, "Shift" }, "j", function () awful.tag.incnmaster(1) end):add()
keybinding({ modkey, "Shift" }, ";", function () awful.tag.incnmaster(-1) end):add()
keybinding({ modkey, "Control" }, "j", function () awful.tag.incncol(1) end):add()
keybinding({ modkey, "Control" }, ";", function () awful.tag.incncol(-1) end):add()
keybinding({ modkey }, "space", function () awful.layout.inc(layouts, 1) end):add()
keybinding({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end):add()

-- Prompt
keybinding({ modkey }, "Return", function ()
                                 awful.prompt.run({ prompt = "Run: " }, mypromptbox, awful.spawn, awful.completion.bash,
os.getenv("HOME") .. "/.cache/awesome/history") end):add()
keybinding({ modkey }, "F4", function ()
                                 awful.prompt.run({ prompt = "Run Lua code: " }, mypromptbox, awful.eval, awful.prompt.bash,
os.getenv("HOME") .. "/.cache/awesome/history_eval") end):add()
keybinding({ modkey, "Ctrl" }, "i", function ()
                                        if mypromptbox.text then
                                            mypromptbox.text = nil
                                        else
                                            mypromptbox.text = nil
                                            if client.focus.class then
                                                mypromptbox.text = "Class: " .. client.focus.class .. " "
                                            end
                                            if client.focus.instance then
                                                mypromptbox.text = mypromptbox.text .. "Instance: ".. client.focus.instance .. " "
                                            end
                                            if client.focus.role then
                                                mypromptbox.text = mypromptbox.text .. "Role: ".. client.focus.role
                                            end
                                        end
                                    end):add()

--- Tabulous, tab manipulation
keybinding({ modkey, "Control" }, "y", function ()
    local tabbedview = tabulous.tabindex_get()
    local nextclient = awful.client.next(1)

    if not tabbedview then
        tabbedview = tabulous.tabindex_get(nextclient)

        if not tabbedview then
            tabbedview = tabulous.tab_create()
            tabulous.tab(tabbedview, nextclient)
        else
            tabulous.tab(tabbedview, client.focus)
        end
    else
        tabulous.tab(tabbedview, nextclient)
    end
end):add()

keybinding({ modkey, "Shift" }, "y", tabulous.untab):add()

keybinding({ modkey }, "y", function ()
   local tabbedview = tabulous.tabindex_get()

   if tabbedview then
       local n = tabulous.next(tabbedview)
       tabulous.display(tabbedview, n)
   end
end):add()

-- Client awful tagging: this is useful to tag some clients and then do stuff like move to tag on them
keybinding({ modkey, "Control" }, "t", awful.client.togglemarked):add()
keybinding({ modkey, 'Shift' }, "t", function ()
    local tabbedview = tabulous.tabindex_get()
    local clients = awful.client.getmarked()

    if not tabbedview then
        tabbedview = tabulous.tab_create(clients[1])
        table.remove(clients, 1)
    end

    for k,c in pairs(clients) do
        tabulous.tab(tabbedview, c)
    end

end):add()

for i = 1, keynumber do
    keybinding({ modkey, "Shift" }, "F" .. i,
                   function ()
                       local screen = mouse.screen
                       if tags[screen][i] then
                           for k, c in pairs(awful.client.getmarked()) do
                               awful.client.movetotag(tags[screen][i], c)
                           end
                       end
                   end):add()
end
-- }}}

-- {{{ Hooks
-- Hook function to execute when focusing a client.
awful.hooks.focus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_focus
    end
end)

-- Hook function to execute when unfocusing a client.
awful.hooks.unfocus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_normal
    end
end)

-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
    c.border_color = beautiful.border_marked
end)

-- Hook function to execute when unmarking a client
awful.hooks.unmarked.register(function (c)
    c.border_color = beautiful.border_focus
end)

-- Hook function to execute when the mouse is over a client.
awful.hooks.mouse_over.register(function (c)
    -- Sloppy focus, but disabled for magnifier layout
    if awful.layout.get(c.screen) ~= "magnifier"
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- Hook function to execute when a new client appears.
awful.hooks.manage.register(function (c)
    if use_titlebar then
        -- Add a titlebar
        awful.titlebar.add(c, { modkey = modkey })
    end
    -- Add mouse bindings
    c:buttons({
        button({ }, 1, function (c) client.focus = c; c:raise() end),
        button({ modkey }, 1, function (c) c:mouse_move() end),
        button({ modkey }, 3, function (c) c:mouse_resize() end)
    })
    -- New client may not receive focus
    -- if they're not focusable, so set border anyway.
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_normal
    client.focus = c

    -- Check if the application should be floating.
    local cls = c.class
    local inst = c.instance
    if floatapps[cls] then
        c.floating = floatapps[cls]
    elseif floatapps[inst] then
        c.floating = floatapps[inst]
    end

    -- Check application->screen/tag mappings.
    local target
    if apptags[cls] then
        target = apptags[cls]
    elseif apptags[inst] then
        target = apptags[inst]
    end
    if target then
        c.screen = target.screen
        awful.client.movetotag(tags[target.screen][target.tag], c)
    end

    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Honor size hints: if you want to drop the gaps between windows, set this to false.
    -- c.honorsizehints = false
end)

-- Hook function to execute when arranging the screen
-- (tag switch, new client, etc)
awful.hooks.arrange.register(function (screen)
    local layout = awful.layout.get(screen)
    if layout then
        mylayoutbox[screen].image = image("/usr/local/share/awesome/icons/layouts/" .. layout .. "w.png")
    else
        mylayoutbox[screen].image = nil
    end

    -- Give focus to the latest client in history if no window has focus
    -- or if the current window is a desktop or a dock one.
    if not client.focus then
        local c = awful.client.focus.history.get(screen, 0)
        if c then client.focus = c end
    end

    -- Uncomment if you want mouse warping
    --[[
    if client.focus then
        local c_c = client.focus:coords()
        local m_c = mouse.coords()

        if m_c.x < c_c.x or m_c.x >= c_c.x + c_c.width or
            m_c.y < c_c.y or m_c.y >= c_c.y + c_c.height then
            if table.maxn(m_c.buttons) == 0 then
                mouse.coords({ x = c_c.x + 5, y = c_c.y + 5})
            end
        end
    end
    ]]
end)

-- Hook called every second
awful.hooks.timer.register(1, function ()
    mytextbox.text = os.date(" <span weight='bold' color='#fff'>%l:%M</span> %a %b %d ")
end)
-- }}}
