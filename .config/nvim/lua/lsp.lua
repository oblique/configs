local nest = require('nest')

function is_lsp_diagnostics_enabled(bufnr, client_id)
    local ok, result = pcall(vim.api.nvim_buf_get_var, bufnr, 'lsp_diagnostics_enabled')

    if not ok then
        return true
    end

    return result
end

-- TODO: Use something similar when 0.6.0 is released:
-- https://github.com/BachirC/dotfiles/blob/master/nvim/lua/cstm/lsp/diagnostic/toggle.lua
function toggle_lsp_diagnostics()
    if vim.b.lsp_diagnostics_enabled == nil or vim.b.lsp_diagnostics_enabled == true then
        vim.b.lsp_diagnostics_enabled = false
    else
        vim.b.lsp_diagnostics_enabled = true
    end

    -- Redraw diagnostics
    for _, client in pairs(vim.lsp.get_active_clients()) do
        vim.lsp.diagnostic.redraw(0, client.id)
    end
end

vim.lsp.handlers['textDocument/publishDiagnostics'] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        signs = is_lsp_diagnostics_enabled,
        virtual_text = is_lsp_diagnostics_enabled,
        -- Always disable underlining of diagnostics
        underline = false,
    })

nest.applyKeymaps {
    { '<leader>le', toggle_lsp_diagnostics }
}
