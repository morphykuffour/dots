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
-- Jcall(require, "morpheus/hop")
require("morpheus.lsp")
Jcall(require, "morpheus/telescope")

-- Themes
-- vim.cmd("colorscheme darkplus")
vim.cmd("colorscheme gruvbox")
-- vim.cmd("colorscheme github_dark_default")

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

-- Do not source the default filetype.vim
-- vim.g.did_load_filetypes = 1

-- source vimfiles
vim.cmd([[runtime! vimfiles/*.vim]])
