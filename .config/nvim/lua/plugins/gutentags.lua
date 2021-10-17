-- You can regenerate tags with `:GutentagsUpdate`

local nest = require('nest')

vim.g.gutentags_modules = { 'ctags', 'gtags_cscope' }
vim.g.gutentags_cache_dir = vim.env.HOME .. '/.cache/tags'
vim.g.gutentags_plus_switch = 1
vim.g.gutentags_plus_nomap = 1

-- Disable auto-generation
vim.g.gutentags_enabled = 0

-- Find the root directory of a project. If is not under revision system, just
-- create `.root` empty file.
vim.g.gutentags_project_root = { '.git', '.svn', '.hg', '.root' }

-- Keymaps
function _G.apply_gutentags_remaps()
    nest.applyKeymaps {
        {
            'g', buffer = true, {
                { 'd', ':GscopeFind g <c-r><c-w><cr>' },
                { 'r', ':GscopeFind s <c-r><c-w><cr>' },
            }
        }
    }
end

-- Apply remaps on C/C++ buffers
vim.cmd('autocmd FileType c,cpp call v:lua.apply_gutentags_remaps()')
