local utils = require('utils')
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if not utils.path_exists(install_path) then
    vim.fn.system({
        'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim',
        install_path
    })

    fresh_install = true
end

local packer = require('packer')

packer.startup(function()
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    use 'vim-scripts/SyntaxAttr.vim'

    -- TODO: https://github.com/numToStr/Comment.nvim
    use 'scrooloose/nerdcommenter'

    -- TODO: https://github.com/hoob3rt/lualine.nvim
    -- TODO: https://github.com/Famiu/feline.nvim
    use {
        'vim-airline/vim-airline',
        requires = {
            'vim-airline/vim-airline-themes',
            'tpope/vim-fugitive',
        }
    }

    use 'nvim-lua/plenary.nvim'
    use 'kyazdani42/nvim-web-devicons'
    use 'LionC/nest.nvim'
    use 'kyazdani42/nvim-tree.lua'
    use 'nvim-telescope/telescope.nvim'
    use 'cappyzawa/trim.nvim'
    use 'famiu/bufdelete.nvim'

    -- lsp
    use 'neovim/nvim-lspconfig'
    use 'williamboman/nvim-lsp-installer'
    use 'glepnir/lspsaga.nvim'

    -- completion
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'

    -- syntax
    use 'cespare/vim-toml'
    use 'plasticboy/vim-markdown'
    use 'jceb/vim-orgmode'
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    -- rust
    --
    -- You need to run these commands:
    --   rustup component add rust-src
    use 'rust-lang/rust.vim'
    use 'simrat39/rust-tools.nvim'

    -- tags
    use 'ludovicchabant/vim-gutentags'
    use 'skywind3000/gutentags_plus'
end)

if fresh_install then
    -- Install plugins
    --
    -- TODO: Currently this is async, find a wait to block on it because we get
    -- an error on loading.
    -- Check: https://github.com/wbthomason/packer.nvim/issues/198
    packer.install()
end

require('plugins.vim-airline')
require('plugins.nvim-tree')
require('plugins.vim-markdown')
require('plugins.syntaxattr')
require('plugins.telescope')
require('plugins.nvim-cmp')
require('plugins.lspsaga')
require('plugins.gutentags')
require('plugins.nvim-treesitter')
require('plugins.trim')
