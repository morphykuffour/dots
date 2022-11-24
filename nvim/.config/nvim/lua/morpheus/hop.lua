require("hop").setup({
	keys = "etovxqpdygfblzhckisuran",
})

-- testing out Hop.nvim with vim.schedule

local opts = { noremap = true, silent = true }
local function jump_back_to_original_buffer(original_buffer) --{{{
	local current_buffer = vim.api.nvim_get_current_buf()

	if current_buffer ~= original_buffer then
		-- jump back to the original buffer
		vim.cmd([[normal! ]])
	else
		-- jump back to the original line
		vim.cmd([[normal! ]])
	end
end --}}}

-- SECTION: Hyper Yank
-- NOTE: Hyper Yank with Treesitter Node Select
vim.keymap.set("n", "yx", function()
	local original_buffer = vim.api.nvim_get_current_buf()

	vim.cmd([[:HopLineStartMW]]) --> jump to line
	vim.schedule(function()
		require("syntax-tree-surfer").select()
		vim.cmd([[normal! V]]) --> go to visual selection mode -> optional
		vim.cmd([[normal! y]]) --> yank
		jump_back_to_original_buffer(original_buffer)
	end)
end, opts)

-- NOTE: Hyper Yank a line
vim.keymap.set("n", "yl", function()
	local original_buffer = vim.api.nvim_get_current_buf()

	vim.cmd([[:HopLineStartMW]]) --> jump to line
	vim.schedule(function()
		vim.cmd([[normal! yy]]) --> yank the line
		jump_back_to_original_buffer(original_buffer)
	end)
end, opts)

-- NOTE: Hyper Yank Treesitter Code Block
vim.keymap.set("n", "yc", function()
	local original_buffer = vim.api.nvim_get_current_buf()
	vim.cmd([[:HopLineStartMW]])
	vim.schedule(function()
		require("tsht").nodes()
		vim.schedule(function()
			vim.cmd([[normal! V]]) --> go to visual selection mode -> optional
			vim.cmd([[normal! y]]) --> yank
			jump_back_to_original_buffer(original_buffer)
		end)
	end)
end, opts)

-- NOTE: Using nvim-treehopper to yank
vim.keymap.set("n", "ym", function()
	require("tsht").nodes()
	vim.schedule(function()
		vim.cmd([[normal! V]]) --> go to visual selection mode
		vim.cmd([[normal! y]]) --> yank
	end)
end, opts)

--SECTION: Hyper Paste

vim.keymap.set("n", "vp", function()
	vim.cmd([[:HopLineStartMW]])
	vim.schedule(function()
		vim.cmd([[normal! p]]) --> paste
	end)
end, opts)
vim.keymap.set("n", "<Leader>vp", function()
	vim.cmd([[:HopLineStartMW]])
	vim.schedule(function()
		vim.cmd([[normal! o]]) --> make new line below target
		vim.cmd([[normal! o]]) --> make another new line below target
		vim.cmd([[normal! p]]) --> paste
	end)
end, opts)

vim.keymap.set("n", "vP", function()
	vim.cmd([[:HopLineStartMW]])
	vim.schedule(function()
		vim.cmd([[normal! P]]) --> paste
	end)
end, opts)
vim.keymap.set("n", "<Leader>vP", function()
	vim.cmd([[:HopLineStartMW]])
	vim.schedule(function()
		vim.cmd([[normal! O]]) --> make another new line below target
		vim.cmd([[normal! P]]) --> paste
	end)
end, opts)

--------------------
--------------------
--------------------
--------------------
--------------------
--------------------
--------------------
--------------------
--------------------
-- VIDEO 1: NOTE: Hop to create new lines

vim.keymap.set("n", "vo", function()
	vim.cmd([[:HopLineStart]])
	vim.schedule(function()
		vim.cmd([[normal! o]])
		vim.cmd([[startinsert]])
	end)
end, opts)
vim.keymap.set("n", "<Leader>vo", function()
	vim.cmd([[:HopLineStart]])
	vim.schedule(function()
		vim.cmd([[normal! o]])
		vim.cmd([[normal! o]])
		vim.cmd([[startinsert]])
	end)
end, opts)

vim.keymap.set("n", "vO", function()
	vim.cmd([[:HopLineStart]])
	vim.schedule(function()
		vim.cmd([[normal! O]])
		vim.cmd([[normal! O]])
		vim.cmd([[startinsert]])
	end)
end, opts)
vim.keymap.set("n", "<Leader>vO", function()
	vim.cmd([[:HopLineStart]])
	vim.schedule(function()
		vim.cmd([[normal! O]])
		vim.cmd([[normal! O]])
		vim.cmd([[startinsert]])
	end)
end, opts)

--  NOTE: Hop with Macros

vim.keymap.set("n", "vY", function()
	vim.cmd([[:HopLineStart]])
	vim.schedule(function()
		vim.cmd([[:normal @f]])
	end)
end, opts)

--  NOTE: Hop can even trigger LuaSnip Snippets (complicated)

vim.keymap.set("n", "your_keymap", function()
	vim.cmd([[:HopLineStart]]) --> Best thing ever in the history of mankind
	vim.schedule(function()
		vim.cmd([[:normal ojja ]]) --> POGG
		vim.cmd("startinsert")
	end)
end, opts)

--  NOTE: TSHT (Treesitter hint textobject) --> mfussenegger/nvim-treehopper

vim.keymap.set("n", "vy", function()
	require("tsht").nodes()
	vim.schedule(function()
		vim.cmd([[normal! c]])
		vim.cmd([[startinsert]])
	end)
end, opts)

-- hop.lua
vim.api.nvim_set_keymap(
	"n",
	"f",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"F",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"o",
	"f",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"o",
	"F",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"",
	"t",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"",
	"T",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>e",
	"<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"v",
	"<leader>e",
	"<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"o",
	"<leader>e",
	"<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<cr>",
	{}
)
