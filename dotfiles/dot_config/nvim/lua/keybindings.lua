local nest = require('nest')

nest.applyKeymaps {
    -- Disable ex mode
    { 'Q', '<Nop>' },

    -- Disable recording
    { 'qq', '<Nop>' },

    -- Reset search highlight
    { '<leader>h', '<cmd>nohl<cr>' },

    -- move between windows
    { '<m-down>',   '<c-w>j' },
    { '<m-up>',     '<c-w>k' },
    { '<m-left>',   '<c-w>h' },
    { '<m-right>',  '<c-w>l' },

    -- move between buffers
    { '<m-l>', '<cmd>bnext<cr>' },
    { '<m-j>', '<cmd>bprevious<cr>' },

    {
        '<leader>', {
            { '-',  '<cmd>split<cr>' },
            { '\\', '<cmd>vsplit<cr>' },
            { 'q',  '<cmd>quit<cr>' },
            { 'o',  '<cmd>only<cr>' },
        }
    },

    -- reindent whole file
    { '<leader>i',  'mzgg=G`z' },
}
