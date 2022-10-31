vim.g.airline_theme = 'darkness'
vim.g.airline_powerline_fonts = true
vim.g['airline#extensions#tabline#enabled'] = true
vim.g['airline#extensions#nvimlsp#enabled'] = true
-- Show filename and parent directory
vim.g['airline#extensions#tabline#formatter'] = 'short_path'
vim.opt.laststatus = 2
-- we don't need mode since airline show it
vim.opt.showmode = false
