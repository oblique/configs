local lspconfig = require('lspconfig')
local cmp_nvim_lsp = require('cmp_nvim_lsp')

-- If a project uses cmake, you can generate `compile_commands.json` with:
--
-- ```
-- mkdir build
-- cd build
-- cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
-- cd ..
-- ln -s build/compile_commands.json .
-- ```
--
-- If a project uses Makefile, you can generate `compile_commands.json` with:
--
-- ```
-- bear -- make
-- ```
--
-- If you want to index manually a project:
--
-- ```
-- cd /path/to/project_root
-- ccls --index=.
-- ```
lspconfig.ccls.setup {
    capabilities = cmp_nvim_lsp.default_capabilities(),
}
