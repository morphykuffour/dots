-- [[ Configure Gitsigns ]]
-- See `:help gitsigns.txt`

local function setup_gitsigns()
  local ok, gitsigns = pcall(require, 'gitsigns')
  if not ok then
    vim.notify('Failed to load gitsigns', vim.log.levels.ERROR)
    return
  end

  gitsigns.setup({
    signs = {
      add = { text = '+' },
      change = { text = '~' },
      delete = { text = '_' },
      topdelete = { text = '‾' },
      changedelete = { text = '~' },
    },
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns
      if not gs then
        vim.notify('Gitsigns not loaded properly', vim.log.levels.WARN)
        return
      end
      
      -- Preview hunk
      vim.keymap.set('n', '<leader>hp', function()
        pcall(gs.preview_hunk, { buffer = bufnr })
      end, { buffer = bufnr, desc = 'Preview git hunk' })

      -- Jump to next/previous hunk
      vim.keymap.set({ 'n', 'v' }, ']c', function()
        if vim.wo.diff then return ']c' end
        vim.schedule(function() 
          pcall(gs.next_hunk)
        end)
        return '<Ignore>'
      end, { expr = true, buffer = bufnr, desc = "Jump to next hunk" })
      
      vim.keymap.set({ 'n', 'v' }, '[c', function()
        if vim.wo.diff then return '[c' end
        vim.schedule(function() 
          pcall(gs.prev_hunk)
        end)
        return '<Ignore>'
      end, { expr = true, buffer = bufnr, desc = "Jump to previous hunk" })
    end,
  })
end

-- Set up gitsigns with error handling
setup_gitsigns()

-- Add command to manually reload gitsigns
vim.api.nvim_create_user_command('GitsignsReload', function()
  setup_gitsigns()
  vim.notify('Gitsigns reloaded', vim.log.levels.INFO)
end, { desc = 'Reload gitsigns configuration' })
