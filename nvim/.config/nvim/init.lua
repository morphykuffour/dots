-- Personal settings
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install package manager
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
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
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  'tpope/vim-sleuth',

  -- comment
  'tpope/vim-commentary',

  -- quickfix window
  'kevinhwang91/nvim-bqf',

  -- Startify
  'mhinz/vim-startify',
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "nvim-telescope/telescope.nvim", -- optional
      "sindrets/diffview.nvim",        -- optional
      "ibhagwan/fzf-lua",              -- optional
    },
    config = true
  },
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    -- Allow disabling LSP for isolated tests
    cond = function()
      return not vim.g.__disable_lsp
    end,
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { 'williamboman/mason.nvim', opts = {} },
      'williamboman/mason-lspconfig.nvim',
      { 'j-hui/fidget.nvim',       version = '*', opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
    config = function()
      require 'morpheus.lsp'
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    opts = function(_, opts)
      local nls = require("null-ls")
      if type(opts.sources) == "table" then
        vim.list_extend(opts.sources, {
          nls.builtins.code_actions.statix,
          nls.builtins.formatting.alejandra,
          nls.builtins.diagnostics.deadnix,
        })
      end
    end,
  },
  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',

      -- Add blink.cmp for blinking completion items
      'Saghen/blink.cmp',
    },
  },

  {
    'olimorris/codecompanion.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'hrsh7th/nvim-cmp',
    },
    event = 'VeryLazy',
    config = function()
      require('morpheus.plugins.ai')
    end,
  },

  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('morpheus.gitsigns')
    end,
    -- Add lazy loading to prevent early initialization issues
    event = { 'BufReadPre', 'BufNewFile' },
  },

  {
    -- Theme inspired by Atom
    'navarasu/onedark.nvim',
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'onedark'
    end,
  },

  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = "VeryLazy", -- Load after startup
    config = function()
      require("todo-comments").setup({
        signs = true, -- show icons in the signs column
        sign_priority = 8, -- sign priority
        -- keywords recognized as todo comments
        keywords = {
          FIX = {
            icon = " ", -- icon used for the sign, and in search results
            color = "error", -- can be a hex color, or a named color (see below)
            alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
          },
          TODO = { icon = " ", color = "info" },
          HACK = { icon = " ", color = "warning" },
          WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
          PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
          NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
          TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
          DONE = { icon = " ", color = "hint", alt = { "COMPLETE", "FINISHED" } },
        },
        merge_keywords = true, -- when true, custom keywords will be merged with the defaults
        -- highlighting of the line containing the todo comment
        highlight = {
          multiline = true, -- enable multine todo comments
          multiline_pattern = "^.", -- lua pattern to match the next multiline from the start of the matched keyword
          multiline_context = 10, -- extra lines that will be re-evaluated when changing a line
          before = "", -- "fg" or "bg" or empty
          keyword = "wide", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty
          after = "fg", -- "fg" or "bg" or empty
          pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlighting (vim regex)
          comments_only = false, -- Set to false to highlight TODOs in markdown and other non-comment text
          max_line_len = 400, -- ignore lines longer than this
          exclude = {}, -- list of file types to exclude highlighting
        },
        -- list of named colors where we try to extract the guifg from the
        -- list of highlight groups or use the hex color if hl not found as a fallback
        colors = {
          error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
          warning = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
          info = { "DiagnosticInfo", "#2563EB" },
          hint = { "DiagnosticHint", "#10B981" },
          default = { "Identifier", "#7C3AED" },
          test = { "Identifier", "#FF00FF" }
        },
        search = {
          command = "rg",
          args = {
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
          },
          pattern = [[\b(KEYWORDS):]], -- ripgrep regex
        },
      })

      -- Add keybindings for todo-comments
      vim.keymap.set("n", "]t", function()
        require("todo-comments").jump_next()
      end, { desc = "Next todo comment" })

      vim.keymap.set("n", "[t", function()
        require("todo-comments").jump_prev()
      end, { desc = "Previous todo comment" })

      -- You can also use telescope integration
      vim.keymap.set("n", "<leader>st", "<cmd>TodoTelescope<cr>", { desc = "[S]earch [T]odos" })
    end
  },

  -- Vim Markdown plugin for proper markdown folding (like linkarzu's config)
  {
    "plasticboy/vim-markdown",
    ft = "markdown",
    config = function()
      -- Enable markdown folding
      vim.g.vim_markdown_folding_disabled = 0
      vim.g.vim_markdown_folding_style = "stacked"
      vim.g.vim_markdown_fold_heading = 1
      vim.g.vim_markdown_fold_fenced_codeblocks = 1
      vim.g.vim_markdown_fold_toc = 1
      vim.g.vim_markdown_fold_toc_fenced = 1
    end
  },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = false,
        theme = 'onedark',
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  --{
  --  -- Add indentation guides even on blank lines
  --  "lukas-reineke/indent-blankline.nvim",
  --  main = "ibl",
  --  ---@module "ibl"
  --  ---@type ibl.config
  --  -- Enable `lukas-reineke/indent-blankline.nvim`
  --  -- See `:help indent_blankline.txt`
  --  opts = {}
  --  -- opts = {
  --  --   char = '┊',
  --  --   show_trailing_blankline_indent = false,
  --  -- },
  --},

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        -- NOTE: If you are having trouble with this installation,
        --       refer to the README for telescope-fzf-native for more instructions.
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ':TSUpdate',
    -- Load at startup so parsers install once, not on first file open
    config = function()
      require('morpheus.treesitter')
    end,
  },

  -- local plugins
  -- {
  --   dir = '~/tmp/lookup.nvim',
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",         -- required
  --   },
  -- },

}, {})

require 'morpheus.plugins.autoformat'
require 'morpheus.plugins.debug'
require 'morpheus.telescope'
require 'morpheus.keymaps'
require 'morpheus.cmp'
require 'morpheus.wiki'

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Disable swapfiles to avoid W325 prompts when reopening files
vim.o.swapfile = false

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })


-- reload vimrc on save
local autocmd = vim.api.nvim_create_autocmd
-- autocmd("BufWritePost", {
-- 	pattern = vim.env.MYVIMRC,
-- 	callback = function()
-- 		dofile(vim.env.MYVIMRC)
-- 	end,
-- })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- TODO: get https://github.com/morphykuffour/rawtalk to work
vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "*",
  callback = function()
    local mode = vim.fn.mode()
    vim.fn.system(string.format("echo '%s' > /tmp/vim_mode", mode))
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "nix",
  callback = function()
    vim.bo.commentstring = "# %s"
  end,
})

-- TODO: explore better colorscheme
-- vim.cmd.colorscheme 'vim'
vim.cmd.colorscheme 'onedark'

-- Configure diff colors for better conflict visibility
vim.cmd([[
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
]])


