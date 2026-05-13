-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

local function get_path()
  local path = vim.fn.expand "%:p"
  if path == "" then
    return "Empty path"
  else
    return path
  end
end

local function copy_path()
  local path = get_path()
  vim.fn.setreg("+", path)
  vim.fn.setreg("*", path)
end

local function enable_spaces()
  vim.opt.expandtab = true
  vim.opt.softtabstop = 4
  vim.opt.shiftwidth = 4
  vim.opt.tabstop = 4
end

local function enable_spaces2()
  vim.opt.expandtab = true
  vim.opt.softtabstop = 2
  vim.opt.shiftwidth = 2
  vim.opt.tabstop = 2
end

local function enable_tabs()
  vim.opt.expandtab = false
  vim.opt.softtabstop = 0
  vim.opt.shiftwidth = 8
  vim.opt.tabstop = 8
end

local function enable_fmt()
  if vim.b.autoformat ~= nil then vim.b.autoformat = true end
end

local function disable_fmt()
  if vim.b.autoformat ~= nil then vim.b.autoformat = false end
end

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    commands = {
      EnableSpaces = { enable_spaces },
      EnableSpaces2 = { enable_spaces2 },
      EnableTabs = { enable_tabs },
      ShowPath = { function() print(get_path()) end },
      CopyPath = { function() copy_path() end },
      GetKeycode = { function() print(vim.fn.keytrans(vim.fn.getcharstr())) end },
      EnableFmt = { enable_fmt },
      DisableFmt = { disable_fmt },
    },

    autocmds = {
      indentation_spaces = {
        {
          event = { "FileType" },
          pattern = "nix",
          callback = enable_spaces2,
        },
        {
          event = { "FileType" },
          pattern = "lua",
          callback = enable_spaces2,
        },
      },
    },

    -- Configure core features of AstroNvim
    features = {
      diagnostics = false,
    },

    diagnostics = {
      virtual_text = true,
      underline = true,
    },

    -- vim options can be configured here
    options = {
      -- vim.opt.<key>
      opt = {
        number = true,
        relativenumber = false,
        spell = false,
        wrap = true,
        textwidth = 100,

        -- Indentation spaces
        expandtab = true,
        softtabstop = 4,
        shiftwidth = 4,
        tabstop = 4,

        -- Show whitespaces
        list = true,
        listchars = { trail = "~", tab = "> ", nbsp = "␣" },

        -- Use cursor provided by terminal
        guicursor = "",

        -- C fmt
        cindent = true,
        cinoptions = { ":0", "l1", "g0", "t0", "(0" },

        -- use both primary and secondary clipboards
        clipboard = { "unnamed", "unnamedplus" },
      },

      -- vim.g.<key>
      g = {},
    },

    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      -- Normal mode
      n = {
        -- Disable ex mode
        ["Q"] = false,

        -- Disable recording
        ["qq"] = false,

        -- Move between windows
        ["<M-Down>"] = { "<C-w>j" },
        ["<M-Up>"] = { "<C-w>k" },
        ["<M-Left>"] = { "<C-w>h" },
        ["<M-Right>"] = { "<C-w>l" },

        -- Move between buffers
        ["<M-l>"] = { "<Cmd>bnext<CR>" },
        ["<M-j>"] = { "<Cmd>bprevious<CR>" },

        -- Remap splits
        ["\\"] = false,
        ["|"] = false,
        ["<Leader>-"] = { "<Cmd>split<CR>", desc = "Horizontal Split" },
        ["<Leader>\\"] = { "<Cmd>vsplit<CR>", desc = "Vertical Split" },

        -- Remap resizing
        ["<C-Up>"] = false,
        ["<C-Down>"] = false,
        ["<C-Left>"] = false,
        ["<C-Right>"] = false,
        ["<C-S-Up>"] = { "<Cmd>resize -2<CR>", desc = "Resize split up" },
        ["<C-S-Down>"] = { "<Cmd>resize +2<CR>", desc = "Resize split down" },
        ["<C-S-Left>"] = { "<Cmd>vertical resize -2<CR>", desc = "Resize split left" },
        ["<C-S-Right>"] = { "<Cmd>vertical resize +2<CR>", desc = "Resize split right" },

        -- Other stuff
        ["<Leader>bf"] = { "<Cmd>tab split<CR>", desc = "Fullscreen the current buffer" },
        ["<Leader>h"] = { "<Cmd>nohl<CR>", desc = "Reset search hightlight" },
        ["<Leader>i"] = { "mzgg=G`z", desc = "Reindent whole file" },
      },
    },
  },
}
