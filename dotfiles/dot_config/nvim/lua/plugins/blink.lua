return {
  "Saghen/blink.cmp",
  opts = function(_, opts)
    local function remove_from_keymap(key, value)
      local map = opts.keymap and opts.keymap[key]
      if map then
        for i, v in ipairs(map) do
          if v == value then
            table.remove(map, i)
            break
          end
        end
      end
    end

    remove_from_keymap("<Tab>", "snippet_forward")
    remove_from_keymap("<S-Tab>", "snippet_backward")
  end,
}
