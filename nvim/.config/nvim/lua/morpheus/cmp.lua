-- [[ Completion setup (nvim-cmp + LuaSnip) ]]

local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

local cmp = require('cmp')

local function feedkeys(keys)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, true, true), 'n', true)
end

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  }, {
    { name = 'buffer' },
    { name = 'path' },
  }),
  completion = {
    completeopt = 'menu,menuone,noinsert',
  },
  window = {
    documentation = false,
    completion = { scrollbar = false },
  },
  view = {
    entries = { name = 'custom', selection_order = 'near_cursor' }
  },
  experimental = {
    ghost_text = false
  },
  mapping = {
    ['<C-n>'] = function()
      if cmp.visible() then
        cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
      else
        feedkeys('<C-n>')
      end
    end,
    ['<C-p>'] = function()
      if cmp.visible() then
        cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
      else
        feedkeys('<C-p>')
      end
    end,
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
})

-- Disable the default nvim-cmp UI for cmdline; keep Insert completion native
cmp.setup.cmdline('/', { enabled = false })
cmp.setup.cmdline(':', { enabled = false })

-- Update LSP capabilities and export for the LSP setup
local capabilities = require('cmp_nvim_lsp').default_capabilities()
if vim.g.lsp_capabilities == nil then
  vim.g.lsp_capabilities = capabilities
end
