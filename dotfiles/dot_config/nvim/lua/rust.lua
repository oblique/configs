local lspconfig = require('lspconfig')
local cmp_nvim_lsp = require('cmp_nvim_lsp')
local rust_tools = require('rust-tools')
local nest = require('nest')

rust_tools.setup {
    tools = {
        inlay_hints = {
            highlight = 'RustToolsInlayHint',
        },
    },
    server = {
        capabilities = cmp_nvim_lsp.default_capabilities(),
        on_attach = function(client, bufnr)
            client.server_capabilities.semanticTokensProvider = nil
        end,
        settings = {
            ['rust-analyzer'] = {
                checkOnSave = {
                    enable = false,
                },
            }
        }
    },
}

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
