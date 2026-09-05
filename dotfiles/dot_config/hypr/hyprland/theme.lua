-- Curves (was: bezier = name, x0, y0, x1, y1)
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("md3_standard", { type = "bezier", points = { { 0.2, 0 }, { 0, 1 } } })
hl.curve("md3_decel", { type = "bezier", points = { { 0.05, 0.7 }, { 0.1, 1 } } })
hl.curve("md3_accel", { type = "bezier", points = { { 0.3, 0 }, { 0.8, 0.15 } } })
hl.curve("overshot", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.1 } } })
hl.curve("crazyshot", { type = "bezier", points = { { 0.1, 1.5 }, { 0.76, 0.92 } } })
hl.curve("hyprnostretch", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.0 } } })
hl.curve("fluent_decel", { type = "bezier", points = { { 0.1, 1 }, { 0, 1 } } })
hl.curve("easeInOutCirc", { type = "bezier", points = { { 0.85, 0 }, { 0.15, 1 } } })
hl.curve("easeOutCirc", { type = "bezier", points = { { 0, 0.55 }, { 0.45, 1 } } })
hl.curve("easeOutExpo", { type = "bezier", points = { { 0.16, 1 }, { 0.3, 1 } } })

-- Animations (was: animation = leaf, enabled, speed, curve, style)
hl.animation({ leaf = "windows", enabled = true, speed = 3, bezier = "md3_decel", style = "popin 60%" })
hl.animation({ leaf = "border", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "fade", enabled = true, speed = 2.5, bezier = "md3_decel" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 3.5, bezier = "easeOutExpo", style = "slide" })
hl.animation({ leaf = "specialWorkspace", enabled = true, speed = 3, bezier = "md3_decel", style = "slidevert" })

hl.config({
  animations = { enabled = true },
  general = {
    gaps_in = 1,
    gaps_out = 1,
    border_size = 1,
    col = {
      active_border = "rgb(8786d7)",
      inactive_border = "rgb(303030)",
    },
  },
  decoration = {
    rounding = 4,
    blur = { enabled = false },
    shadow = { enabled = false },
    dim_special = 0.3,
  },
  group = {
    col = {
      border_active = "rgb(8786d7)",
      border_inactive = "rgb(303030)",
      border_locked_active = "rgb(8786d7)",
      border_locked_inactive = "rgb(303030)",
    },
    groupbar = {
      col = {
        inactive = "rgb(270a56)",
        active = "rgb(4c1b9c)",
        locked_active = "rgb(8f0d5d)",
        locked_inactive = "rgb(550436)",
      },
    },
  },
  misc = {
    font_family = "Cantarell",
    background_color = "rgb(101010)",
    disable_hyprland_logo = true,
    disable_splash_rendering = true,
    force_default_wallpaper = 0,
  },
})
