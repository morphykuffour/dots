-- Personal settings
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install package manager
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- Bootstrap secrets (decrypt OPENAI_API_KEY via rage if present)
pcall(function()
  require('morpheus.secrets').bootstrap_openai_from_rage()
end)

-- Enable fenced code block highlighting in markdown
vim.g.vim_markdown_fenced_languages = {
  'bash=sh', 'sh=sh', 'zsh=sh',
  'js=javascript', 'ts=typescript', 'tsx=typescriptreact',
  'json=json', 'yaml=yaml', 'toml=toml',
  'lua=lua', 'vim=vim', 'vimscript=vim',
  'go=go', 'rust=rust', 'cpp=cpp', 'c=c', 'python=python',
}

-- Install your plugins.
require('lazy').setup({

  -- Git related plugins
  { 'tpope/vim-fugitive', cmd = { 'Git', 'G', 'Gdiffsplit', 'Gvdiffsplit' } },
  { 'tpope/vim-rhubarb', dependencies = { 'tpope/vim-fugitive' } },
  'tpope/vim-sleuth',

  -- comment
  'tpope/vim-commentary',

  -- quickfix window
  { 'kevinhwang91/nvim-bqf', ft = 'qf' },

  -- Startify
  {
    'mhinz/vim-startify',
    lazy = false,
    priority = 100,
  },

  -- themes (lazy load, only one is used)
  { "miikanissi/modus-themes.nvim", lazy = true },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
  },
  { "Shatur/neovim-ayu", lazy = true },

  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
    },
    config = true
  },

  -- Mason (LSP installer)
  {
    'williamboman/mason.nvim',
    lazy = false,
    config = function()
      require('mason').setup()
    end,
  },
  {
    'williamboman/mason-lspconfig.nvim',
    dependencies = { 'williamboman/mason.nvim' },
  },

  -- LSP Configuration
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    cond = function()
      return not vim.g.__disable_lsp
    end,
    dependencies = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      {
        'j-hui/fidget.nvim',
        opts = {
          notification = {
            window = { winblend = 0 },
          },
        },
      },
      'folke/neodev.nvim',
    },
    config = function()
      require 'morpheus.lsp'
    end,
  },

  -- null-ls for nix formatting
  {
    "nvimtools/none-ls.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local null_ls = require("null-ls")
      null_ls.setup({
        sources = {
          null_ls.builtins.code_actions.statix,
          null_ls.builtins.formatting.alejandra,
          null_ls.builtins.diagnostics.deadnix,
        },
      })
    end,
  },

  -- Autocompletion
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      {
        'L3MON4D3/LuaSnip',
        build = 'make install_jsregexp',
      },
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'rafamadriz/friendly-snippets',
    },
    config = function()
      require 'morpheus.cmp'
    end,
  },

  -- AI assistant
  {
    'olimorris/codecompanion.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
    },
    cmd = { 'CodeCompanion', 'CodeCompanionChat' },
    config = function()
      require('morpheus.plugins.ai')
    end,
  },

  -- Git signs
  {
    'lewis6991/gitsigns.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require('morpheus.gitsigns')
    end,
  },

  -- TODO comments
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      signs = true,
      sign_priority = 8,
      keywords = {
        FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning" },
        WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
        TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
        DONE = { icon = " ", color = "hint", alt = { "COMPLETE", "FINISHED" } },
      },
      highlight = {
        multiline = true,
        before = "",
        keyword = "wide",
        after = "fg",
        comments_only = false,
      },
    },
    keys = {
      { "]t", function() require("todo-comments").jump_next() end, desc = "Next todo comment" },
      { "[t", function() require("todo-comments").jump_prev() end, desc = "Previous todo comment" },
    },
  },

  -- Markdown plugin
  {
    "plasticboy/vim-markdown",
    ft = "markdown",
    config = function()
      vim.g.vim_markdown_folding_disabled = 0
      vim.g.vim_markdown_folding_style = "stacked"
      vim.g.vim_markdown_fold_heading = 1
      vim.g.vim_markdown_fold_fenced_codeblocks = 1
    end
  },

  -- Statusline
  {
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    opts = {
      options = {
        icons_enabled = false,
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  -- Telescope
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    cmd = 'Telescope',
    keys = {
      { '<leader>ff', desc = 'Find files' },
      { '<leader>fg', desc = 'Live grep' },
      { '<leader>bb', desc = 'Buffers' },
    },
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
    config = function()
      require('morpheus.telescope')
    end,
  },

  -- Treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ':TSUpdate',
    config = function()
      require('morpheus.treesitter')
    end,
  },

  -- Debug Adapter Protocol
  {
    'mfussenegger/nvim-dap',
    cmd = { 'DapContinue', 'DapToggleBreakpoint' },
    keys = {
      { '<F5>', function() require('dap').continue() end, desc = 'Debug: Start/Continue' },
      { '<leader>b', function() require('dap').toggle_breakpoint() end, desc = 'Debug: Toggle Breakpoint' },
    },
    dependencies = {
      'rcarriga/nvim-dap-ui',
      'williamboman/mason.nvim',
      'jay-babu/mason-nvim-dap.nvim',
      'leoluz/nvim-dap-go',
      'nvim-neotest/nvim-nio',
    },
    config = function()
      local dap = require 'dap'
      local dapui = require 'dapui'

      require('mason-nvim-dap').setup {
        automatic_installation = true,
        handlers = {},
        ensure_installed = { 'delve' },
      }

      vim.keymap.set('n', '<F1>', dap.step_into, { desc = 'Debug: Step Into' })
      vim.keymap.set('n', '<F2>', dap.step_over, { desc = 'Debug: Step Over' })
      vim.keymap.set('n', '<F3>', dap.step_out, { desc = 'Debug: Step Out' })
      vim.keymap.set('n', '<leader>B', function()
        dap.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
      end, { desc = 'Debug: Set Breakpoint' })

      dapui.setup {
        icons = { expanded = '▾', collapsed = '▸', current_frame = '*' },
        controls = {
          icons = {
            pause = '⏸', play = '▶', step_into = '⏎', step_over = '⏭',
            step_out = '⏮', step_back = 'b', run_last = '▶▶',
            terminate = '⏹', disconnect = '⏏',
          },
        },
      }

      vim.keymap.set('n', '<F7>', dapui.toggle, { desc = 'Debug: See last session result.' })

      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close

      require('dap-go').setup()
    end,
  },

  -- Diffview
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
  },

}, {
  -- Lazy.nvim options
  performance = {
    rtp = {
      disabled_plugins = {
        'gzip',
        'matchit',
        'matchparen',
        'netrwPlugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
})

-- Load configuration modules
require 'morpheus.plugins.autoformat'
require 'morpheus.keymaps'
require 'morpheus.wiki'
require('morpheus.startify').setup()
require('morpheus.build').setup()

-- Set highlight on search
vim.o.hlsearch = false

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Disable swapfiles
vim.o.swapfile = false

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt
vim.o.completeopt = 'menuone,noselect'

-- Enable true colors
vim.o.termguicolors = true

-- Disable space in normal/visual mode (leader key)
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Rawtalk: QMK Layer Switcher Integration
local socket_path = "/tmp/rawtalk.sock"
local uv = vim.uv or vim.loop
local rawtalk_client = nil
local rawtalk_connected = false

local function rawtalk_connect()
  if rawtalk_connected then return true end
  rawtalk_client = uv.new_pipe(false)
  rawtalk_client:connect(socket_path, function(err)
    rawtalk_connected = not err
  end)
  vim.wait(50, function() return rawtalk_connected end, 10)
  return rawtalk_connected
end

local function rawtalk_send_mode(mode)
  if not rawtalk_connected then rawtalk_connect() end
  if rawtalk_connected and rawtalk_client then
    pcall(function() rawtalk_client:write(mode .. "\n") end)
  end
end

local rawtalk_group = vim.api.nvim_create_augroup("Rawtalk", { clear = true })

vim.api.nvim_create_autocmd("ModeChanged", {
  group = rawtalk_group,
  pattern = "*",
  callback = function()
    rawtalk_send_mode(vim.fn.mode())
  end,
})

vim.api.nvim_create_autocmd("VimEnter", {
  group = rawtalk_group,
  callback = function()
    vim.defer_fn(function()
      rawtalk_connect()
      rawtalk_send_mode(vim.fn.mode())
    end, 100)
  end,
})

vim.api.nvim_create_autocmd("VimLeave", {
  group = rawtalk_group,
  callback = function()
    if rawtalk_client then pcall(function() rawtalk_client:close() end) end
  end,
})

-- Nix filetype settings
vim.api.nvim_create_autocmd("FileType", {
  pattern = "nix",
  callback = function()
    vim.bo.commentstring = "# %s"
  end,
})

-- Set colorscheme
vim.cmd.colorscheme 'catppuccin-mocha'

-- Configure diff colors for better conflict visibility
vim.cmd([[
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
]])

-- Copy over SSH with OSC52
if vim.env.SSH_TTY then
  local osc52 = require("vim.ui.clipboard.osc52")

  local function copy_reg(reg)
    local orig = osc52.copy(reg)
    return function(lines, regtype)
      vim.fn.setreg(reg, table.concat(lines, "\n"), regtype)
      orig(lines, regtype)
    end
  end

  vim.g.clipboard = {
    name = "OSC 52 with register sync",
    copy = {
      ["+"] = copy_reg("+"),
      ["*"] = copy_reg("*"),
    },
    paste = {
      ["+"] = function() return vim.fn.getreg('+'), 'v' end,
      ["*"] = function() return vim.fn.getreg('*'), 'v' end,
    },
  }

  vim.o.clipboard = "unnamedplus"
end
