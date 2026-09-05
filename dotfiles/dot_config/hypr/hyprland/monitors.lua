local M = {}

-- `disabled = false` is required so re-applying a rule undoes an earlier disable
local laptop = { output = "eDP-1", mode = "preferred", position = "0x0", scale = "auto", disabled = false }
local fallback = { output = "", mode = "preferred", position = "auto-left", scale = "auto", disabled = false }

-- Default layout, any external monitor goes to the left of the laptop panel
function M.laptop_and_external()
  hl.monitor(laptop)
  hl.monitor(fallback)
end

-- Laptop panel only, the fallback rule keeps anything else disabled
function M.laptop_only()
  -- Enable eDP-1 first so at no point every monitor is disabled
  hl.monitor(laptop)
  hl.monitor({ output = "", disabled = true })
end

-- External only, keeps the laptop panel when no external monitor is active
function M.external_only()
  -- Restore the fallback rule so a connected external monitor comes back up
  hl.monitor(fallback)
  if #hl.get_monitors() > 1 then
    hl.monitor({ output = "eDP-1", disabled = true })
  end
end

M.laptop_and_external()

-- When lid closes then disable eDP-1, unless it is the only active monitor
hl.bind("switch:on:Lid Switch", function()
  if #hl.get_monitors() > 1 then
    hl.monitor({ output = "eDP-1", disabled = true })
  end
end, { locked = true })

-- When lid opens then enable eDP-1
hl.bind("switch:off:Lid Switch", function()
  hl.monitor(laptop)
end, { locked = true })

return M
