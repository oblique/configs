-- use both primary and secondary clipboards
vim.opt.clipboard = { 'unnamed', 'unnamedplus' }

-- in X11 force xsel as clipboard backend
if vim.env.DISPLAY and not vim.env.WAYLAND_DISPLAY then
    vim.g.clipboard = {
        name = 'xsel',
        copy = {
            ['*'] = 'xsel --nodetach -i -p',
            ['+'] = 'xsel --nodetach -i -b'
        },
        paste = {
            ['*'] = 'xsel -o -p',
            ['+'] = 'xsel -o -b'
        },
        cache_enabled = 1,
    }
end
