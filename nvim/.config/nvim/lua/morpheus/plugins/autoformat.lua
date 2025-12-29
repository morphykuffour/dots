-- autoformat.lua
-- Use your language server to automatically format your code on save.

local M = {}

-- Switch for controlling whether you want autoformatting.
local format_is_enabled = true

function M.setup()
  vim.api.nvim_create_user_command('FormatToggle', function()
    format_is_enabled = not format_is_enabled
    vim.notify('Autoformatting: ' .. tostring(format_is_enabled), vim.log.levels.INFO)
  end, { desc = 'Toggle autoformatting on save' })

  -- Create an augroup that is used for managing our formatting autocmds.
  local _augroups = {}
  local get_augroup = function(client)
    if not _augroups[client.id] then
      local group_name = 'lsp-format-' .. client.name
      local id = vim.api.nvim_create_augroup(group_name, { clear = true })
      _augroups[client.id] = id
    end
    return _augroups[client.id]
  end

  -- Whenever an LSP attaches to a buffer, we will run this function.
  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('lsp-attach-format', { clear = true }),
    callback = function(args)
      local client_id = args.data.client_id
      local client = vim.lsp.get_client_by_id(client_id)
      local bufnr = args.buf

      if not client then return end

      -- Only attach to clients that support document formatting
      if not client.server_capabilities.documentFormattingProvider then
        return
      end

      -- Skip tsserver (usually works poorly)
      if client.name == 'tsserver' or client.name == 'ts_ls' then
        return
      end

      -- Create an autocmd that will run *before* we save the buffer.
      vim.api.nvim_create_autocmd('BufWritePre', {
        group = get_augroup(client),
        buffer = bufnr,
        callback = function()
          if not format_is_enabled then
            return
          end

          vim.lsp.buf.format({
            async = false,
            filter = function(c)
              return c.id == client.id
            end,
          })
        end,
      })
    end,
  })
end

-- Run setup immediately when this module is loaded
M.setup()

return M
