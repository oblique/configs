local utils = require('utils')

require('misc')
require('clipboard')
require('plugins')
require('keybindings')
require('spellcheck')
require('rust')
require('cxx')

if utils.path_exists(vim.fn.stdpath('config') .. '/lua/local.lua') then
    require('local')
end
