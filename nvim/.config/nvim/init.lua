-- vimrc
-- Author: Morphy Kuffour
-- Alias:  JediGrandMaster

-- Personal settings
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.snippets = "luasnip"

require("morpheus.utils")
Jcall(require, "morpheus/plugins")
Jcall(require, "morpheus/keymaps")
require("morpheus.globals")
require("morpheus.options")
require("morpheus.lsp")
require("morpheus.completion")
require("morpheus.syntastic")
require("morpheus.telescope")
require("morpheus.treesitter")
require("morpheus.diaglist")
require("morpheus.neogit")
require("morpheus.snippets")
require("morpheus.luasnip")
require("morpheus.headlines")
require("morpheus.gitsigns")
require("morpheus.format")
require("morpheus.godbolt")
-- require("morpheus.hop")
require("morpheus.debug")
require("morpheus.statusline")
-- require("morpheus.filetype")
require("morpheus.todo")
-- require("morpheus.delimters")
-- require("morpheus.wilder") -- TODO move wilder.vim to wilder.lua

-- require("nrepl").config({})
-- require("lsp_signature").setup({})
-- require("nvim-treesitter.configs").setup({
-- 	-- tree_docs = { enable = true },
-- })

-- Themes
-- vim.cmd("colorscheme darkplus")
-- vim.cmd("colorscheme gruvbox")
vim.cmd("colorscheme github_dark_default")

-- Setup LSPs, DAPs, Linters
require("mason").setup({
	ui = {
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
	},
	-- The directory in which to install packages.
	-- install_root_dir = path.concat { vim.fn.stdpath "data", "mason" },
})

vim.g.loaded_python_provider = 0 -- disable python2
vim.g.python3_host_prog = "/usr/bin/python3"

-- reload vimrc on save
local autocmd = vim.api.nvim_create_autocmd
autocmd("BufWritePost", {
	pattern = vim.env.MYVIMRC,
	callback = function()
		dofile(vim.env.MYVIMRC)
	end,
})

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
	command = "source <afile> | PackerCompile",
	group = packer_group,
	pattern = vim.fn.expand("$MYVIMRC"),
})

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
-- local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
-- vim.api.nvim_create_autocmd("TextYankPost", {
-- 	callback = function()
-- 		vim.highlight.on_yank()
-- 	end,
-- 	group = highlight_group,
-- 	pattern = "*",
-- })

-- Do not source the default filetype.vim
-- vim.g.did_load_filetypes = 1

-- source vimfiles
vim.cmd([[runtime! vimfiles/*.vim]])

-- gx => open url in browser
if vim.fn.has("wsl") then
	vim.g.netrw_browsex_viewer = "/usr/bin/wslview"
elseif vim.fn.has("mac") then
	vim.g.netrw_browsex_viewer = "open"
elseif vim.fn.has("linux") then
	-- vim.g.netrw_browsex_viewer = "/usr/bin/xdg-open"
	vim.g.netrw_browsex_viewer = vim.env.BROWSER
end
