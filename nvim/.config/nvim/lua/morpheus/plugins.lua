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
	-- use("francoiscabrol/ranger.vim")
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
	use("tpope/vim-commentary")
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
	-- use("ThePrimeagen/vim-be-good")
	-- low level stuff
	-- use("p00f/godbolt.nvim")
	-- use("sakhnik/nvim-gdb") -- TODO configure dap for c and cpp
	-- use("tjdevries/nlua.nvim")
	-- use("nvim-lua/completion-nvim")
	-- use("euclidianAce/BetterLua.vim")
	use({ "wesleimp/stylua.nvim" })
	use("~/tmp/lookup.nvim")
	-- use("~/iCloud/projects/neovim-plugins/nvim-whid")
	use("jbyuki/one-small-step-for-vimkind")
	use("mfussenegger/nvim-dap")
	use("mfussenegger/nvim-dap-python")
	use("rcarriga/nvim-dap-ui")
	use("theHamsta/nvim-dap-virtual-text")
	use("bfredl/nvim-luadev")
	use("onsails/diaglist.nvim")
	use("ii14/nrepl.nvim")
	use({ "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" })
	use("nathom/filetype.nvim")
	-- sql dev
	use("nanotee/sqls.nvim")
	use("tpope/vim-dadbod")
	use("kristijanhusak/vim-dadbod-ui")
	use("kristijanhusak/vim-dadbod-completion")
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
