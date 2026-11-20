return {
  "mrjones2014/smart-splits.nvim",
  specs = {
    {
      "AstroNvim/astrocore",
      opts = {
        mappings = {
          n = {
            -- Move between windows and tmux panes
            ["<M-Down>"] = { function() require("smart-splits").move_cursor_down() end },
            ["<M-Up>"] = { function() require("smart-splits").move_cursor_up() end },
            ["<M-Left>"] = { function() require("smart-splits").move_cursor_left() end },
            ["<M-Right>"] = { function() require("smart-splits").move_cursor_right() end },
            ["<C-w>j"] = { function() require("smart-splits").move_cursor_down() end },
            ["<C-w>k"] = { function() require("smart-splits").move_cursor_up() end },
            ["<C-w>h"] = { function() require("smart-splits").move_cursor_left() end },
            ["<C-w>l"] = { function() require("smart-splits").move_cursor_right() end },

            -- Resize windows and tmux panes
            ["<C-Up>"] = false,
            ["<C-Down>"] = false,
            ["<C-Left>"] = false,
            ["<C-Right>"] = false,
            ["<C-S-Up>"] = { function() require("smart-splits").resize_up(2) end, desc = "Resize split up" },
            ["<C-S-Down>"] = { function() require("smart-splits").resize_down(2) end, desc = "Resize split down" },
            ["<C-S-Left>"] = { function() require("smart-splits").resize_left(2) end, desc = "Resize split left" },
            ["<C-S-Right>"] = { function() require("smart-splits").resize_right(2) end, desc = "Resize split right" },
          },
        },
      },
    },
  },
}
