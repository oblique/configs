-- Translate every mapping and Ctrl combination to the Greek keyboard layout. Together with
-- `langmap` this keeps normal mode fully usable without switching layouts.

-- The Greek layout has no letter on the Q, W and Shift+W keys, so those are left out.
local latin = "ABCDEFGHIJKLMNOPRSTUVXYZabcdefghijklmnoprstuvwxyz"
local greek = "ΑΒΨΔΕΦΓΗΙΞΚΛΜΝΟΠΡΣΤΘΩΧΥΖαβψδεφγηιξκλμνοπρστθωςχυζ"

---@type LazySpec
return {
  "Wansmer/langmapper.nvim",
  lazy = false,
  -- Load before AstroCore so its mappings get translated too
  priority = 20000,
  init = function()
    -- langmapper reads langmap during setup, so it has to exist before plugins load
    vim.opt.langmap = greek .. ";" .. latin
  end,
  opts = {
    use_layouts = { "gr" },
    layouts = {
      gr = { layout = greek, default_layout = latin },
    },
  },
}
