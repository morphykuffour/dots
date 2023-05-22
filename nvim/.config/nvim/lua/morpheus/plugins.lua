-- require("morpheus/utils")

local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

vim.cmd([[
augroup packer_user_config
autocmd!
autocmd BufWritePost plugins.lua source <afile> | PackerSync
augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

packer.init({
	-- pop up display
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
	-- fix for nixos
	compile_path = require("packer.util").join_paths(vim.fn.stdpath("data"), "plugin", "packer_compiled.lua"),
})

-- Plugins
return packer.startup(function(use)
	-- utils
	use("wbthomason/packer.nvim")
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")
	use("christoomey/vim-tmux-navigator")
	use("AndrewRadev/bufferize.vim")
	use("francoiscabrol/ranger.vim")
	use("szw/vim-maximizer")
	use("windwp/nvim-autopairs")

	-- writing
	-- use("mzlogin/vim-markdown-toc")
	-- use("SidOfc/mkdx")
	-- use("vim-pandoc/vim-rmarkdown")
	-- use("vim-pandoc/vim-pandoc")
	-- use("vim-pandoc/vim-pandoc-syntax")
	-- use("nvim-orgmode/orgmode")
	-- use("junegunn/goyo.vim")
	-- use("lukas-reineke/headlines.nvim")

	-- UI enchancements
	use("nvim-tree/nvim-web-devicons")
	use("antoinemadec/FixCursorHold.nvim")
	use("kevinhwang91/nvim-bqf")
	use("mhinz/vim-startify")
	use("voldikss/vim-floaterm")
	use({ "akinsho/toggleterm.nvim" })
	use("tjdevries/express_line.nvim")
	use({ "folke/which-key.nvim", opts = {} })
	-- tpope
	use("tpope/vim-sensible")
	use("tpope/vim-fugitive")
	use("tpope/vim-surround")
	use("tpope/vim-repeat")
	use("tpope/vim-eunuch")
	use("tpope/vim-unimpaired")
	-- git
	use({ "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" })
	use({ "sindrets/diffview.nvim", requires = "nvim-lua/plenary.nvim" })
	use({ "akinsho/git-conflict.nvim" })
	use("lewis6991/gitsigns.nvim")
	-- Colorschemes
	use("folke/tokyonight.nvim")
	use("ellisonleao/gruvbox.nvim")
	use("lunarvim/darkplus.nvim")
	use("marko-cerovac/material.nvim")
	-- LSP Configuration
	use({
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "williamboman/mason.nvim", config = true },
			"williamboman/mason-lspconfig.nvim",
			-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
			{ "j-hui/fidget.nvim", opts = {} },
			"folke/neodev.nvim",
		},
	})
      use{ 'williamboman/mason.nvim', config = true }
      use'williamboman/mason-lspconfig.nvim'

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      use{ 'j-hui/fidget.nvim', opts = {} }
	-- Autocompletion and Snippets
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
			"hrsh7th/cmp-nvim-lsp",
			"rafamadriz/friendly-snippets",
		},
	})
	-- cmp plugins
	-- use("hrsh7th/cmp-path")
	-- use("hrsh7th/cmp-cmdline")
	-- use("hrsh7th/cmp-nvim-lua")
	-- use("saadparwaiz1/cmp_luasnip")
	-- use("tjdevries/complextras.nvim")
	-- LSP
	-- use("hrsh7th/cmp-nvim-lsp")
	-- use("onsails/lspkind.nvim")
	-- use("nvim-lua/lsp_extensions.nvim")
	-- use("hrsh7th/cmp-buffer")
	-- use("hrsh7th/nvim-cmp")
	-- use({ "tzachar/cmp-tabnine", run = "./install.sh", requires = "hrsh7th/nvim-cmp" })
	-- use("simrat39/symbols-outline.nvim")
	-- use({
	-- 	"ericpubu/lsp_codelens_extensions.nvim",
	-- 	config = function()
	-- 		require("codelens_extensions").setup()
	-- 	end,
	-- })
	-- code formatting and documentation
	use("milisims/nvim-luaref")
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})
	-- Telescope
	use({ "nvim-telescope/telescope.nvim", branch = "0.1.x", requires = { "nvim-lua/plenary.nvim" } })
	use("nvim-telescope/telescope-fzy-native.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make", cond = vim.fn.executable("make") == 1 })
	use("nvim-telescope/telescope-media-files.nvim")
	-- use("nvim-telescope/telescope-cheat.nvim")
	-- use({ "nvim-telescope/telescope-file-browser.nvim" })
	use({ "dhruvmanila/telescope-bookmarks.nvim" })
	-- Treesitter
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
	use("nvim-treesitter/playground")
	use("nvim-treesitter/nvim-tree-docs")
	use("bryall/contextprint.nvim")
	use("nvim-treesitter/nvim-treesitter-textobjects")
	use("nvim-treesitter/nvim-treesitter-refactor")
	use("p00f/nvim-ts-rainbow")
	-- MISC
	use("LnL7/vim-nix")
	use("tyru/open-browser.vim")
	use("ThePrimeagen/harpoon")
	-- use("ThePrimeagen/vim-be-good")
	-- low level stuff
	-- use("p00f/godbolt.nvim")
	-- use("sakhnik/nvim-gdb") -- TODO configure dap for c and cpp
	-- use("tjdevries/nlua.nvim")
	-- use("nvim-lua/completion-nvim")
	-- use("euclidianAce/BetterLua.vim")
	use({ "wesleimp/stylua.nvim" })
	-- use("~/Dropbox/projects/neovim-plugins/lookup.nvim")
	-- use("~/Dropbox/projects/neovim-plugins/nvim-whid")
	use("jbyuki/one-small-step-for-vimkind")
	use("mfussenegger/nvim-dap")
	use("mfussenegger/nvim-dap-python")
	use("rcarriga/nvim-dap-ui")
	use("theHamsta/nvim-dap-virtual-text")
	use("bfredl/nvim-luadev")
	use({ "ray-x/lsp_signature.nvim" })
	use("onsails/diaglist.nvim")
	use("ii14/nrepl.nvim")
	use("lewis6991/impatient.nvim")
	use({ "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" })
	use("nathom/filetype.nvim")
	-- use("mfussenegger/nvim-treehopper")
	-- use("phaazon/hop.nvim")
	use({
		"phaazon/hop.nvim",
		branch = "v2", -- optional but strongly recommended
	})

	-- sql dev
	use("nanotee/sqls.nvim")
	use("tpope/vim-dadbod")
	use("kristijanhusak/vim-dadbod-ui")
	use("kristijanhusak/vim-dadbod-completion")

	-- use("dhruvasagar/vim-table-mode")
	-- use("preservim/tagbar")
	-- use("norcalli/nvim-colorizer.lua")
	-- use({
	-- 	"andrewferrier/wrapping.nvim",
	-- 	config = function()
	-- 		require("wrapping").setup()
	-- 	end,
	-- })
	-- use("img-paste-devs/img-paste.vim")

	-- use({
	-- 	"klen/nvim-test",
	-- 	config = function()
	-- 		require("nvim-test").setup({
	-- 			commands_create = true, -- create commands (TestFile, TestLast, ...)
	-- 			silent = false, -- less notifications
	-- 			run = true, -- run test commands
	-- 			term = "terminal", -- a terminal to run (terminal|toggleterm)
	-- 			termOpts = {
	-- 				direction = "vertical", -- terminal's direction (horizontal|vertical|float)
	-- 				width = 86, -- terminal's width (for vertical|float)
	-- 				height = 24, -- terminal's height (for horizontal|float)
	-- 				go_back = false, -- return focus to original window after executing
	-- 				stopinsert = false, -- exit from insert mode
	-- 			},
	-- 			runners = { -- setup test runners, only using for JS. Currently, doesn't add value to Rust workflow 17-Feb-2022
	-- 				javascript = "nvim-test.runners.jest",
	-- 				lua = "nvim-test.runners.busted",
	-- 				-- rust = "nvim-test.runners.cargo-test",
	-- 			},
	-- 		})
	-- 	end,
	-- })

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
