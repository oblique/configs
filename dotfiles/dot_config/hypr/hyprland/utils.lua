local M = {}

-- Hyprland only has a toggle dispatcher, so gate it on the current group state
function M.set_grouped(grouped)
  return function()
    local window = hl.get_active_window()
    if window and (window.group ~= nil) ~= grouped then
      hl.dispatch(hl.dsp.group.toggle())
    end
  end
end

-- Windows move one by one because Hyprland cannot merge two workspaces
function M.move_workspace_to(target)
  return function()
    local source = hl.get_active_workspace()
    if not source or source.id == target then
      return
    end
    for _, window in ipairs(hl.get_workspace_windows(source)) do
      hl.dispatch(hl.dsp.window.move({ workspace = target, follow = false, window = window }))
    end
    hl.dispatch(hl.dsp.focus({ workspace = target }))
  end
end

return M
