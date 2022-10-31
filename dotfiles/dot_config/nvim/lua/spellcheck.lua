-- use ]s and [s to move to mispelled word
-- use z= to get suggestion list
-- use zg to add a word to dictionary
-- use zw to mark a word as incorrect

local nest = require('nest')

vim.opt.spelllang = 'en_us'

local function toggle_spellcheck()
    if vim.opt.spell:get() then
        vim.opt.spell = false
    else
        vim.opt.spell = true
    end
end

nest.applyKeymaps {
    { '<leader>s', toggle_spellcheck }
}
