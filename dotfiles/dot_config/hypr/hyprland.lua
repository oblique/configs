-- Submodule loads (was: source = ./hyprland/foo.conf)
require("hyprland.permissions")
require("hyprland.keybindings")
require("hyprland.monitors")
require("hyprland.windowrules")
require("hyprland.theme")

-- Compositor settings (merges with the sections set in theme.lua)
hl.config({
  general = {
    layout = "dwindle",
    resize_on_border = true,
    snap = { enabled = true },
  },
  input = {
    kb_layout = "us,gr",
    touchpad = {
      natural_scroll = false,
      tap_to_click = false,
      clickfinger_behavior = true,
      scroll_factor = 1.5,
    },
    touchdevice = { enabled = false },
    special_fallthrough = true,
  },
  dwindle = { force_split = 2 },
  cursor = { no_hardware_cursors = true },
  xwayland = { force_zero_scaling = true },
  group = {
    groupbar = {
      enabled = true,
      gradients = true,
      render_titles = true,
      font_size = 12,
    },
  },
})

-- Call this at the end
require("hyprland.startup")
