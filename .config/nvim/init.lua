local utils = require('utils')

require('misc')
require('clipboard')
require('plugins')
require('rust')
require('keybindings')
require('spellcheck')

if utils.path_exists(vim.fn.stdpath('config') .. '/lua/local.lua') then
    require('local')
end
