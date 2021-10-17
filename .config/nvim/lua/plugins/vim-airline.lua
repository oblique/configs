vim.g.airline_theme = 'darkness'
vim.g.airline_powerline_fonts = true
vim.g['airline#extensions#tabline#enabled'] = true

-- Show just the filename
vim.g['airline#extensions#tabline#fnamemod'] = ':t'
vim.opt.laststatus = 2
-- we don't need mode since airline show it
vim.opt.showmode = false
