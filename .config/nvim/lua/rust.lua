local lspconfig = require('lspconfig')
local cmp_nvim_lsp = require('cmp_nvim_lsp')
local rust_tools = require('rust-tools')
local nest = require('nest')
local lsp_installer_servers = require('nvim-lsp-installer.servers')

local ok, rust_analyzer = lsp_installer_servers.get_server('rust_analyzer')
if ok then
    if not rust_analyzer:is_installed() then
        rust_analyzer:install()
    end

    rust_tools.setup {
        tools = {
            inlay_hints = {
                highlight = 'RustToolsInlayHint',
            },
        },
        server = {
            cmd = { rust_analyzer.root_dir .. '/rust-analyzer' },
            capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities()),
            settings = {
                ['rust-analyzer'] = {
                    checkOnSave = {
                        enable = false,
                    },
                }
            }
        },
    }
end

vim.g.rustfmt_autosave = 1
vim.g.rust_fold = 1

function _G.apply_rust_remaps()
    nest.applyKeymaps {
        { '<leader>i', '<cmd>RustFmt<cr>', buffer = true }
    }
end

-- Apply remaps on Rust buffers
vim.cmd('autocmd FileType rust call v:lua.apply_rust_remaps()')

vim.cmd('command EnableRustfmt let g:rustfmt_autosave = 1')
vim.cmd('command DisableRustfmt let g:rustfmt_autosave = 0')
