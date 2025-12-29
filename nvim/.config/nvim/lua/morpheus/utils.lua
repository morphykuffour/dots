-- Utility functions for neovim configuration

local M = {}

-- Call a function and if it errors out print the error
-- It will continue with the script though.
function M.safe_call(func, ...)
  local res, err = pcall(func, ...)
  if not res then
    vim.notify(tostring(err), vim.log.levels.ERROR)
    return nil
  end
  return err
end

-- Check if file exists
function M.file_exists(file)
  local ok, err, code = os.rename(file, file)
  if not ok then
    if code == 13 then
      -- Permission denied, but it exists
      return true
    end
  end
  return ok, err
end

-- Split string by separator
function M.split_string(inputstr, sep)
  if sep == nil then
    sep = "%s"
  end
  local t = {}
  for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
    table.insert(t, str)
  end
  return t
end

-- Pretty print a value
function M.P(v)
  print(vim.inspect(v))
  return v
end

-- Table length
function M.tbl_length(t)
  local count = 0
  for _ in pairs(t) do
    count = count + 1
  end
  return count
end

-- Reload module (requires plenary)
function M.reload(name)
  if pcall(require, "plenary") then
    require("plenary.reload").reload_module(name)
    return require(name)
  end
  return nil
end

-- Async run command
function M.async_run(command, args)
  vim.fn.jobstart(command .. " " .. (args or ""), { detach = true })
end

-- Reload neovim config
function M.reload_config()
  local hls_status = vim.v.hlsearch
  for name, _ in pairs(package.loaded) do
    if name:match("^morpheus") then
      package.loaded[name] = nil
    end
  end
  dofile(vim.env.MYVIMRC)
  if hls_status == 0 then
    vim.opt.hlsearch = false
  end
end

-- Get visual selection text
function M.get_visual_selection()
  local _, csrow, cscol, cerow, cecol
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode == "" then
    _, csrow, cscol, _ = unpack(vim.fn.getpos("."))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("v"))
    if mode == "V" then
      cscol, cecol = 0, 999
    end
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", true)
  else
    _, csrow, cscol, _ = unpack(vim.fn.getpos("'<"))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("'>"))
  end
  if cerow < csrow then
    csrow, cerow = cerow, csrow
  end
  if cecol < cscol then
    cscol, cecol = cecol, cscol
  end
  local lines = vim.fn.getline(csrow, cerow)
  local n = M.tbl_length(lines)
  if n <= 0 then
    return ""
  end
  lines[n] = string.sub(lines[n], 1, cecol)
  lines[1] = string.sub(lines[1], cscol)
  return table.concat(lines, "\n")
end

-- Get last index of a list
function M.get_last_index(list)
  return #list
end

-- Set up global aliases for convenience (optional)
_G.P = M.P
if pcall(require, "plenary") then
  _G.RELOAD = require("plenary.reload").reload_module
  _G.R = M.reload
end

return M
