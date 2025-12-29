-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(client, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  -- Enable inlay hints if supported
  if client.supports_method('textDocument/inlayHint') then
    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    nmap('<leader>th', function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr }), { bufnr = bufnr })
    end, '[T]oggle Inlay [H]ints')
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

--  LSP and Completion
local servers = {
  clangd = {
    cmd = {
      "clangd",
      "--background-index",
      "--suggest-missing-includes",
      "--clang-tidy",
      "--header-insertion=iwyu",
    },
    init_options = {
      clangdFileStatus = true,
    },
    filetypes = {
      "c",
    },
  },

  -- nix language server
  nil_ls = {
    settings = {
      ['nil'] = {
        formatting = { command = { "alejandra" } },
        nix = {
          flake = {
            autoArchive = true,
            autoEvalInputs = true,
          },
        },
      },
    },
  },

  -- python language server
  pyright = {
    settings = {
      python = {
        analysis = {
          autoSearchPaths = true,
          useLibraryCodeForTypes = true,
          diagnosticMode = "openFilesOnly",
          typeCheckingMode = "basic",
        },
      },
      pyright = {
        disableOrganizeImports = false,
      },
    },
  },

  -- gopls = {},
  -- rust_analyzer = {},
  -- tsserver = {},
  -- html = { filetypes = { 'html', 'twig', 'hbs'} },

  -- lua language server
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = 'LuaJIT' },
        diagnostics = { globals = { 'vim' } },
        workspace = {
          checkThirdParty = false,
          library = vim.api.nvim_get_runtime_file("", true),
        },
        completion = {
          callSnippet = "Replace",
        },
        hint = {
          enable = true,
          arrayIndex = "Disable",
          setType = true,
          paramName = "All",
          paramType = true,
        },
        telemetry = { enable = false },
      },
    },
  },
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities
-- Check if global capabilities are set by cmp module
local capabilities = vim.g.lsp_capabilities or vim.lsp.protocol.make_client_capabilities()
if not vim.g.lsp_capabilities then
  capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
end

-- Mason is already set up via config function in init.lua
-- Temporarily disable mason-lspconfig to test
-- local mason_lspconfig = require 'mason-lspconfig'
-- mason_lspconfig.setup {
--   ensure_installed = vim.tbl_keys(servers),
-- }

-- Configure and enable each server using the new vim.lsp.config API
for server_name, server_opts in pairs(servers) do
  local config = {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = server_opts.settings or server_opts,
    filetypes = server_opts.filetypes,
    cmd = server_opts.cmd,
    init_options = server_opts.init_options,
  }
  vim.lsp.config(server_name, config)
  vim.lsp.enable(server_name)
end

-- Configure bashls
vim.lsp.config('bashls', {
  capabilities = capabilities,
  on_attach = on_attach,
  filetypes = { 'sh', 'bash' },
})
vim.lsp.enable('bashls')
