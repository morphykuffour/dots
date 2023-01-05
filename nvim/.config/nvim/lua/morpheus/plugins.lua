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
	use("rbgrouleff/bclose.vim")
	use("szw/vim-maximizer")
	use("windwp/nvim-autopairs")
	use({ "gelguy/wilder.nvim" })

	-- writing
	-- use("mzlogin/vim-markdown-toc")
	-- use("SidOfc/mkdx")
	-- use("vim-pandoc/vim-rmarkdown")
	-- use("vim-pandoc/vim-pandoc")
	-- use("vim-pandoc/vim-pandoc-syntax")
	use("nvim-orgmode/orgmode")
	use("lukas-reineke/headlines.nvim")

	-- ui enchancements
	-- use("kyazdani42/nvim-web-devicons")
	use("nvim-tree/nvim-web-devicons")
	use("moll/vim-bbye")
	use("antoinemadec/FixCursorHold.nvim")
	use("kevinhwang91/nvim-bqf")
	-- use("ptzz/lf.vim")
	use("mhinz/vim-startify")
	use("voldikss/vim-floaterm")
	use({ "akinsho/toggleterm.nvim" })
	use("junegunn/goyo.vim")
	use("projekt0n/github-nvim-theme")
	-- use({
	-- 	"nvim-lualine/lualine.nvim",
	-- 	requires = { "kyazdani42/nvim-web-devicons", opt = true },
	-- })
	use("tjdevries/express_line.nvim")

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
	-- use("folke/tokyonight.nvim")
	use("ellisonleao/gruvbox.nvim")
	use("lunarvim/darkplus.nvim")
	use("marko-cerovac/material.nvim")

	-- cmp plugins
	-- use("hrsh7th/cmp-path")
	use("hrsh7th/cmp-cmdline")
	-- use("hrsh7th/cmp-nvim-lua")
	-- use("saadparwaiz1/cmp_luasnip")
	-- use("tjdevries/complextras.nvim")

	-- -- LSP
	-- use({ "neovim/nvim-lspconfig" })
	-- use("hrsh7th/cmp-nvim-lsp")
	-- use("onsails/lspkind.nvim")
	-- use("nvim-lua/lsp_extensions.nvim")
	-- use("hrsh7th/cmp-buffer")
	-- use("hrsh7th/nvim-cmp")
	-- use({ "tzachar/cmp-tabnine", run = "./install.sh", requires = "hrsh7th/nvim-cmp" })
	-- use("simrat39/symbols-outline.nvim")
	use({
		"ericpubu/lsp_codelens_extensions.nvim",
		config = function()
			require("codelens_extensions").setup()
		end,
	})

	use({
		"VonHeikemen/lsp-zero.nvim",
		requires = {
			-- LSP Support
			{ "neovim/nvim-lspconfig" },
			{ "williamboman/mason.nvim" },
			{ "williamboman/mason-lspconfig.nvim" },

			-- Autocompletion
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-nvim-lua" },

			-- Snippets
			{ "L3MON4D3/LuaSnip" },
			{ "rafamadriz/friendly-snippets" },
		},
	})
	-- use("jose-elias-alvarez/null-ls.nvim")
	-- use("williamboman/nvim-lsp-installer") -- simple to use language server installer

	-- snippets
	use("L3MON4D3/LuaSnip")
	use("rafamadriz/friendly-snippets")
	-- use("SirVer/ultisnips")

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
	-- use("nvim-telescope/telescope-cheat.nvim")
	-- use({ "nvim-telescope/telescope-file-browser.nvim" })
	use({ "dhruvmanila/telescope-bookmarks.nvim" })
	use("tyru/open-browser.vim")

  use 'nvim-telescope/telescope-media-files.nvim'
	-- use({
	-- 	"AckslD/nvim-neoclip.lua",
	-- 	requires = {
	-- 		{ "tami5/sqlite.lua", module = "sqlite" },
	-- 		{ "nvim-telescope/telescope.nvim" },
	-- 	},
	-- 	config = function()
	-- 		require("neoclip").setup()
	-- 	end,
	-- })
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
	use("nvim-treesitter/playground")
	use("bryall/contextprint.nvim")
	use("nvim-treesitter/nvim-treesitter-textobjects")
	use("nvim-treesitter/nvim-treesitter-refactor")
	-- use 'nvim-treesitter/nvim-treesitter-context'
	-- end
	use("LnL7/vim-nix")
	-- TJ & ThePrimeagen
	use("ThePrimeagen/harpoon")
	use("ThePrimeagen/vim-be-good")
	-- low level stuff
	use("p00f/godbolt.nvim")
	-- use("sakhnik/nvim-gdb") -- TODO configure dap for c and cpp
	use("tjdevries/nlua.nvim")
	use("nvim-lua/completion-nvim")
	use("euclidianAce/BetterLua.vim")
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
	use("folke/lua-dev.nvim")
	use("ii14/nrepl.nvim")
	use("lewis6991/impatient.nvim")
	use({ "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" })
	use("nvim-treesitter/nvim-tree-docs")
	use("nathom/filetype.nvim")
	-- use("mfussenegger/nvim-treehopper")
	-- use("phaazon/hop.nvim")
	-- use("ziontee113/syntax-tree-surfer")
	use("p00f/nvim-ts-rainbow")
	use("tpope/vim-commentary")
	use("junegunn/fzf")
	use("junegunn/fzf.vim")
	-- use("norcalli/nvim-colorizer.lua")
	-- use("norcalli/nvim_utils")
	-- use("anuvyklack/hydra.nvim")
	-- use("bfredl/nvim-miniyank")
	use("anuvyklack/keymap-layer.nvim")
	-- use("kmonad/kmonad-vim")
	use({ "williamboman/mason.nvim" })
	use("nanotee/sqls.nvim")
	use("tpope/vim-dadbod")
	use("kristijanhusak/vim-dadbod-ui")
	use("kristijanhusak/vim-dadbod-completion")
	-- use({ "ellisonleao/glow.nvim" })
	use({ "williamboman/mason-lspconfig.nvim" })
	use("dhruvasagar/vim-table-mode")
	use("preservim/tagbar")
	use("j-hui/fidget.nvim")
  use 'norcalli/nvim-colorizer.lua'

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
