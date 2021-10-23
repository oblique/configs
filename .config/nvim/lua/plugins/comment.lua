-- With `gcc` you toggle comments in normal mode.
-- With `gc` you toggle comments in virtual mode.

local comment = require('Comment')

comment.setup {
    mappings = {
        extra = false,
    }
}
