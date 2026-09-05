local monitors = require("hyprland.monitors")
local m = "SUPER" -- $mainMod

-- Launcher
hl.bind(m .. " + D", hl.dsp.exec_cmd("pkill -x rofi || rofi -show run"), { description = "Run" })
hl.bind(m .. " + A", hl.dsp.exec_cmd("pkill -x rofi || rofi -show drun"), { description = "Run .desktop" })
hl.bind(m .. " + W", hl.dsp.exec_cmd("pkill -x rofi || rofi -show window"), { description = "Select window" })
hl.bind(m .. " + Return", hl.dsp.exec_cmd("uwsm app -- alacritty"), { description = "Open terminal" })
hl.bind(m .. " + E", hl.dsp.exec_cmd("uwsm app -- dolphin ~"), { description = "Open file explorer" })
hl.bind(m .. " + B", hl.dsp.exec_cmd("uwsm app -- brave"), { description = "Open web browser" })

-- Window management
hl.bind(m .. " + Q", hl.dsp.window.close(), { description = "Close focused window" })
hl.bind(m .. " + ALT + Q", hl.dsp.window.kill(), { description = "Kill focused window" })
hl.bind(m .. " + Delete", hl.dsp.exec_cmd("uwsm stop"), { description = "Exit hyprland session" })
hl.bind(m .. " + SHIFT + Delete", hl.dsp.exit(), { description = "Exit hyprland session (dirty)" })
hl.bind(m .. " + SHIFT + Space", hl.dsp.window.float({ action = "toggle" }), { description = "Toggle floating" })
hl.bind(m .. " + G", hl.dsp.group.toggle(), { description = "Toggle group" })
hl.bind(m .. " + SHIFT + G", hl.dsp.group.lock_active({ action = "toggle" }), { description = "Lock active group" })
hl.bind(m .. " + SHIFT + X", hl.dsp.exec_cmd("uwsm app -- hyprlock"), { description = "Lock screen" })
hl.bind(m .. " + R", hl.dsp.layout("togglesplit"), { description = "Toggle split" })
hl.bind(m .. " + SHIFT + F", hl.dsp.window.fullscreen({ mode = "fullscreen" }), { description = "Toggle fullscreen" })
hl.bind(m .. " + F", hl.dsp.window.fullscreen({ mode = "maximized" }), { description = "Toggle fullscreen with bars" })
hl.bind(
  m .. " + ALT + F",
  hl.dsp.window.fullscreen_state({ internal = 0, client = 2 }),
  { description = "Toggle fake fullscreen" }
)
hl.bind(m .. " + ALT + P", hl.dsp.window.pseudo(), { description = "Toggle pseudo tiling" })

-- Change focus
hl.bind(m .. " + Left", hl.dsp.focus({ direction = "l" }), { description = "Focus left" })
hl.bind(m .. " + Right", hl.dsp.focus({ direction = "r" }), { description = "Focus right" })
hl.bind(m .. " + Up", hl.dsp.focus({ direction = "u" }), { description = "Focus up" })
hl.bind(m .. " + Down", hl.dsp.focus({ direction = "d" }), { description = "Focus down" })
hl.bind("ALT + Tab", hl.dsp.window.cycle_next(), { description = "Cycle focus" })
hl.bind(m .. " + SHIFT + J", hl.dsp.group.prev(), { description = "Focus left within window group" })
hl.bind(m .. " + SHIFT + L", hl.dsp.group.next(), { description = "Focus right within window group" })

-- Resize windows
hl.bind(
  m .. " + ALT + Right",
  hl.dsp.window.resize({ x = 30, y = 0, relative = true }),
  { description = "Resize window - right", repeating = true }
)
hl.bind(
  m .. " + ALT + Left",
  hl.dsp.window.resize({ x = -30, y = 0, relative = true }),
  { description = "Resize window - left", repeating = true }
)
hl.bind(
  m .. " + ALT + Up",
  hl.dsp.window.resize({ x = 0, y = -30, relative = true }),
  { description = "Resize window - up", repeating = true }
)
hl.bind(
  m .. " + ALT + Down",
  hl.dsp.window.resize({ x = 0, y = 30, relative = true }),
  { description = "Resize window - down", repeating = true }
)
hl.bind(m .. " + mouse:272", hl.dsp.window.drag(), { description = "Hold left-click to move window", mouse = true })
hl.bind(
  m .. " + mouse:273",
  hl.dsp.window.resize(),
  { description = "Hold right-click to resize window", mouse = true }
)

-- Media/Audio
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("pamixer -t"), { description = "Toggle mute output", locked = true })
hl.bind(
  "XF86AudioMicMute",
  hl.dsp.exec_cmd("pamixer --default-source -t"),
  { description = "Toggle mute microphone", locked = true }
)
hl.bind(
  "XF86AudioLowerVolume",
  hl.dsp.exec_cmd("pamixer -d 5"),
  { description = "Decrease volume", repeating = true, locked = true }
)
hl.bind(
  "XF86AudioRaiseVolume",
  hl.dsp.exec_cmd("pamixer -i 5 --allow-boost --set-limit 150"),
  { description = "Increase volume", repeating = true, locked = true }
)
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { description = "Play/pause media", locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { description = "Play/pause media", locked = true })
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { description = "Play next media", locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { description = "Play previous media", locked = true })

-- Brightness
hl.bind(
  "XF86MonBrightnessUp",
  hl.dsp.exec_cmd("brightnessctl s 5%+"),
  { description = "Increase brightness", repeating = true, locked = true }
)
hl.bind(
  "XF86MonBrightnessDown",
  hl.dsp.exec_cmd("brightnessctl s 5%-"),
  { description = "Decrease brightness", repeating = true, locked = true }
)

-- Notifications
hl.bind(m .. " + N", hl.dsp.exec_cmd("swaync-client -t"), { description = "Toggle notification center" })
hl.bind(m .. " + Space", hl.dsp.exec_cmd("swaync-client --close-latest"), { description = "Close latest notification" })

-- Keyboard layout
hl.bind(
  m .. " + K",
  hl.dsp.exec_cmd("hyprctl switchxkblayout all next"),
  { description = "Switch keyboard layout", locked = true }
)

-- Screenshot
hl.bind(
  "Print",
  hl.dsp.exec_cmd("grimblast copysave screen $(date +/tmp/screenshot_%Y-%m-%d_%H-%M-%S.%N.png)"),
  { description = "Screenshot all monitors", locked = true }
)
hl.bind(
  m .. " + Print",
  hl.dsp.exec_cmd("grimblast copysave screen - | satty -f -"),
  { description = "Screenshot all monitors", locked = true }
)
hl.bind(
  m .. " + SHIFT + Print",
  hl.dsp.exec_cmd("grimblast copysave area - | satty -f -"),
  { description = "Screenshot area", locked = true }
)

-- Workspace navigation
hl.bind(m .. " + 1", hl.dsp.focus({ workspace = 1 }))
hl.bind(m .. " + 2", hl.dsp.focus({ workspace = 2 }))
hl.bind(m .. " + 3", hl.dsp.focus({ workspace = 3 }))
hl.bind(m .. " + 4", hl.dsp.focus({ workspace = 4 }))
hl.bind(m .. " + 5", hl.dsp.focus({ workspace = 5 }))
hl.bind(m .. " + 6", hl.dsp.focus({ workspace = 6 }))
hl.bind(m .. " + 7", hl.dsp.focus({ workspace = 7 }))
hl.bind(m .. " + 8", hl.dsp.focus({ workspace = 8 }))
hl.bind(m .. " + 9", hl.dsp.focus({ workspace = 9 }))
hl.bind(m .. " + 0", hl.dsp.focus({ workspace = 10 }))
hl.bind(m .. " + J", hl.dsp.focus({ workspace = "m-1" }), { description = "Navigate to left active workspace" })
hl.bind(m .. " + L", hl.dsp.focus({ workspace = "m+1" }), { description = "Navigate to right active workspace" })
hl.bind(m .. " + Grave", hl.dsp.focus({ workspace = "previous" }), { description = "Navigate to previous workspace" })

-- Move workspace to monitor
hl.bind(
  m .. " + CTRL + Left",
  hl.dsp.workspace.move({ monitor = "l" }),
  { description = "Move workspace to left monitor" }
)
hl.bind(
  m .. " + CTRL + Right",
  hl.dsp.workspace.move({ monitor = "r" }),
  { description = "Move workspace to right monitor" }
)

-- Move window to workspace
hl.bind(m .. " + SHIFT + 1", hl.dsp.window.move({ workspace = 1, follow = false }))
hl.bind(m .. " + SHIFT + 2", hl.dsp.window.move({ workspace = 2, follow = false }))
hl.bind(m .. " + SHIFT + 3", hl.dsp.window.move({ workspace = 3, follow = false }))
hl.bind(m .. " + SHIFT + 4", hl.dsp.window.move({ workspace = 4, follow = false }))
hl.bind(m .. " + SHIFT + 5", hl.dsp.window.move({ workspace = 5, follow = false }))
hl.bind(m .. " + SHIFT + 6", hl.dsp.window.move({ workspace = 6, follow = false }))
hl.bind(m .. " + SHIFT + 7", hl.dsp.window.move({ workspace = 7, follow = false }))
hl.bind(m .. " + SHIFT + 8", hl.dsp.window.move({ workspace = 8, follow = false }))
hl.bind(m .. " + SHIFT + 9", hl.dsp.window.move({ workspace = 9, follow = false }))
hl.bind(m .. " + SHIFT + 0", hl.dsp.window.move({ workspace = 10, follow = false }))

-- Move window (was: movewindoworgroup)
hl.bind(m .. " + SHIFT + Left", hl.dsp.window.move({ direction = "l", group_aware = true }))
hl.bind(m .. " + SHIFT + Right", hl.dsp.window.move({ direction = "r", group_aware = true }))
hl.bind(m .. " + SHIFT + Up", hl.dsp.window.move({ direction = "u", group_aware = true }))
hl.bind(m .. " + SHIFT + Down", hl.dsp.window.move({ direction = "d", group_aware = true }))

-- Monitor submap (auto-resets after any dispatch, was: submap = monitor, reset)
hl.bind(m .. " + M", hl.dsp.submap("monitor"), { description = "Enter monitor submap", locked = true })
hl.define_submap("monitor", "reset", function()
  hl.bind("L", monitors.laptop_only, { description = "Enable only laptop monitor", locked = true })
  hl.bind("E", monitors.external_only, { description = "Enable only external monitor" })
  hl.bind("D", monitors.laptop_and_external, { description = "Enable both monitors and put external on the left" })
  hl.bind("Escape", hl.dsp.submap("reset"), { locked = true })
end)

-- Passthrough
hl.bind(m .. " + SHIFT + P", hl.dsp.submap("passthrough"), { description = "Passthrough" })
hl.define_submap("passthrough", function()
  hl.bind(m .. " + SHIFT + P", hl.dsp.submap("reset"), { description = "submap" })
end)

-- Scratchpad
hl.bind(
  m .. " + SHIFT + Minus",
  hl.dsp.window.move({ workspace = "special", follow = false }),
  { description = "Move to scratchpad" }
)
hl.bind(m .. " + Minus", hl.dsp.workspace.toggle_special(), { description = "Toggle scratchpad" })

-- Utils
hl.bind(
  m .. " + SHIFT + R",
  hl.dsp.dpms({ action = "on" }),
  { description = "Reset DPMS to ON", repeating = true, locked = true }
)
