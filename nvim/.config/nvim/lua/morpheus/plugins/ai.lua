-- CodeCompanion setup using OpenAI
-- Requires: export OPENAI_API_KEY in your shell env

local ok, codecompanion = pcall(require, 'codecompanion')
if not ok then
  return
end

-- Proactive check: missing key leads to HTTP error payloads without `choices`
if not (vim.env.OPENAI_API_KEY and vim.env.OPENAI_API_KEY ~= '') then
  vim.schedule(function()
    vim.notify('OPENAI_API_KEY is not set; AI features will fail. Ensure secrets bootstrap or export the key.', vim.log.levels.WARN)
  end)
end

codecompanion.setup({
  adapters = {
    http = {
      openai = function()
        return require('codecompanion.adapters').extend('openai', {
          schema = {
            model = {
              default = 'gpt-4o-mini',
            },
          },
          env = {
            -- Explicitly tell CodeCompanion which env var contains the key
            api_key = 'OPENAI_API_KEY',
          },
        })
      end,
      -- opts = { timeout = 30000 }, -- example of new location for opts
    },
  },
  strategies = {
    chat = { adapter = 'openai' },
    inline = { adapter = 'openai' },
  },
})

-- Optional keymaps
vim.keymap.set('n', '<leader>aa', '<cmd>CodeCompanionChat<cr>', { desc = 'AI: Chat' })
vim.keymap.set('v', '<leader>aa', '<cmd>CodeCompanionChat<cr>', { desc = 'AI: Chat (visual)' })
vim.keymap.set('n', '<leader>ai', '<cmd>CodeCompanion<cr>', { desc = 'AI: Actions' })


