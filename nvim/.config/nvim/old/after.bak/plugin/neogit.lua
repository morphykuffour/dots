-- local neogit = require("neogit")

-- neogit.setup({
-- 	disable_commit_confirmation = true,
-- 	disable_insert_on_commit = false,
-- 	integrations = {
-- 		diffview = true,
-- 	},
-- 	-- S
-- })

-- -- Insert mode in NEOGIT_COMMIT_EDIT_EDITMSG
-- local git_group = vim.api.nvim_create_augroup("git_group", { clear = true })
-- local autocmd = vim.api.nvim_create_autocmd
-- autocmd({ "BufRead", "BufWinEnter" }, {
-- 	group = git_group,
-- 	pattern = "NEOGIT_COMMIT_EDIT_EDITMSG",
-- 	command = "startinsert | 1",
-- })
