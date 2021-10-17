local utils = {}

-- Returns true if path exists
function utils.path_exists(path)
    return vim.fn.empty(vim.fn.glob(path)) == 0
end

return utils
