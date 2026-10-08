-- This will run last in the setup process.
-- This is just pure lua so anything that doesn't
-- fit in the normal config locations above can go here

-- Translate built-in and vimscript mappings that bypass the keymap API hooks
require("langmapper").automapping { global = true, buffer = false }
