-- Copy settings from 'r.vim'
vim.cmd([[runtime! ftplugin/r.lua]])

-- Manually copy some settings from 'markdown.lua'
vim.cmd([[setlocal spell]])
vim.cmd([[setlocal wrap]])

-- EC.rmd_block = function()
-- 	vim.fn.append(vim.fn.line("."), "```")
-- 	vim.fn.append(vim.fn.line("."), "```{r }")
-- 	vim.fn.cursor(vim.fn.line(".") + 1, 7)
-- 	vim.cmd([[startinsert]])
-- end

vim.keymap.set("i", "<C-b>", function()
	vim.fn.append(vim.fn.line("."), "```")
	vim.fn.append(vim.fn.line("."), "```{r }")
	vim.fn.cursor(vim.fn.line(".") + 1, 7)
	vim.cmd([[startinsert]])
end, { desc = "rmd_code_block" })
