return {
  "tiagovla/tokyodark.nvim",
  lazy = false,
  opts = {
    custom_highlights = function(highlights, palette)
      -- Disable italics completetly
      for _, opts in pairs(highlights) do
        opts.italic = false
      end

      -- Modify some values
      highlights.Visual.bg = "#1a1a1a"
      highlights.VisualNOS.bg = "#1a1a1a"

      -- Overwrite or add new values
      highlights = vim.tbl_extend("force", highlights, {
        WinBar = { bg = highlights.Normal.bg, fg = highlights.Normal.fg },
        WinBarNC = { bg = highlights.NormalNC.bg, fg = highlights.NormalNC.fg },

        Added = { fg = palette.diff_green },
        Removed = { fg = palette.diff_red },
        Changed = { fg = palette.diff_blue },

        DiagnosticError = { fg = palette.red },
        DiagnosticWarn = { fg = palette.yellow },
        DiagnosticInfo = { fg = palette.cyan },
        DiagnosticHint = { fg = palette.blue },
        DiagnosticOk = { fg = palette.green },
        DiagnosticDeprecated = { fg = palette.red },

        DiagnosticUnderlineError = { fg = palette.red, underline = true },
        DiagnosticUnderlineWarn = { fg = palette.yellow, underline = true },
        DiagnosticUnderlineInfo = { fg = palette.cyan, underline = true },
        DiagnosticUnderlineHint = { fg = palette.blue, underline = true },
        DiagnosticUnderlineOk = { fg = palette.green, underline = true },

        SnacksIndent = { fg = "#202020" },
      })

      return highlights
    end,

    custom_palette = {
      bg0 = "#101010",
      bg1 = "#101010",
      bg2 = "#131313",

      diff_add = "#22322e",
      diff_change = "#1a192e",
      diff_delete = "#281B27",
      diff_text = "#2b2a4e",

      red = "#EE6D85",
      orange = "#F6955B",
      yellow = "#D7A65F",
      green = "#95C561",
      blue = "#7199EE",
      cyan = "#38A89D",
      purple = "#A485DD",
      grey = "#4A5057",
    },
  },
}
