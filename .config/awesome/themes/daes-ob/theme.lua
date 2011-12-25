-------------------------------
--  "Desert" awesome theme  --
--    By Daes DP. (2011)   --
--    edited by oblique    --
-------------------------------


-- {{{ Main
theme = {}
theme.wallpaper_cmd = { "awsetbg -l" }
-- }}}

-- {{{ Styles
theme.font      = "Terminus 8"

-- {{{ Colors
theme.fg_normal = "#517b95"
theme.fg_focus  = "#80ff80"
theme.fg_urgent = "#d21d4c"
theme.bg_normal = "#151515"
theme.bg_focus  = "#151515"
theme.bg_urgent = "#151515"
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_width_panel  = "1"
theme.border_panel = "#050505"
theme.border_normal = "#151515"
theme.border_wnormal = "#303030"
theme.border_focus  = "#303030"
theme.border_marked = "#CC9393"
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "15"
theme.menu_width  = "100"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = awful.util.getdir("config") .. "/themes/daes-ob/taglist/squarefw.png"
theme.taglist_squares_unsel = awful.util.getdir("config") .. "/themes/daes-ob/taglist/squarew.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
-- theme.awesome_icon           = "/usr/share/awesome/themes/zenburn/awesome-icon.png"
theme.menu_submenu_icon      = awful.util.getdir("config") .. "/themes/daes-ob/icons/submenu.png"
-- theme.tasklist_floating_icon = "/usr/share/awesome/themes/default/tasklist/floatingw.png"

-- {{{ Layout
theme.layout_tile       = awful.util.getdir("config") .. "/themes/daes-ob/layouts/tile.png"
theme.layout_tileleft   = awful.util.getdir("config") .. "/themes/daes-ob/layouts/tileleft.png"
theme.layout_tilebottom = awful.util.getdir("config") .. "/themes/daes-ob/layouts/tilebottom.png"
theme.layout_tiletop    = awful.util.getdir("config") .. "/themes/daes-ob/layouts/tiletop.png"
theme.layout_fairv      = awful.util.getdir("config") .. "/themes/daes-ob/layouts/fairv.png"
theme.layout_fairh      = awful.util.getdir("config") .. "/themes/daes-ob/layouts/fairh.png"
theme.layout_spiral     = awful.util.getdir("config") .. "/themes/daes-ob/layouts/spiral.png"
theme.layout_dwindle    = awful.util.getdir("config") .. "/themes/daes-ob/layouts/dwindle.png"
theme.layout_max        = awful.util.getdir("config") .. "/themes/daes-ob/layouts/max.png"
theme.layout_fullscreen = awful.util.getdir("config") .. "/themes/daes-ob/layouts/fullscreen.png"
theme.layout_magnifier  = awful.util.getdir("config") .. "/themes/daes-ob/layouts/magnifier.png"
theme.layout_floating   = awful.util.getdir("config") .. "/themes/daes-ob/layouts/floating.png"
-- }}}


-- {{{ Titlebar
theme.titlebar_close_button_focus  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/close_focus.png"
theme.titlebar_close_button_normal = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = awful.util.getdir("config") .. "/themes/daes-ob/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
