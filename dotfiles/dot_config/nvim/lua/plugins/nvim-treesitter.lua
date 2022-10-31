local treesitter_config = require('nvim-treesitter.configs')

treesitter_config.setup {
    ensure_installed = { 'c', 'cpp', 'yaml' },
    highlight = {
        enable = true,
        disable = { 'rust', 'toml' },
        additional_vim_regex_highlighting = false,
    },
}

vim.cmd('command SyntaxAttr TSHighlightCapturesUnderCursor')
