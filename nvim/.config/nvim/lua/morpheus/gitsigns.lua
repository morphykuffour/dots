function diffThisBranch()
	local branch = vim.fn.input("Branch: ", "")
	require("gitsigns").diffthis(branch)
end

require("gitsigns").setup({
	current_line_blame = true,
	on_attach = function(bufnr)
		-- Navigation
		vim.keymap.set("n", "]c", "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", { expr = true })
		vim.keymap.set("n", "[c", "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", { expr = true })

		-- Actions
		vim.keymap.set("n", "<leader>hs", ":Gitsigns stage_hunk<CR>")
		vim.keymap.set("v", "<leader>hs", ":Gitsigns stage_hunk<CR>")
		vim.keymap.set("n", "<leader>hr", ":Gitsigns reset_hunk<CR>")
		vim.keymap.set("v", "<leader>hr", ":Gitsigns reset_hunk<CR>")
		vim.keymap.set("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>")
		vim.keymap.set("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>")
		vim.keymap.set("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>")
		vim.keymap.set("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>")
		vim.keymap.set("n", "<leader>hb", function()
			require("gitsigns").blame_line({ full = true })
		end)
		vim.keymap.set("n", "<leader>tb", "<cmd>Gitsigns toggle_current_line_blame<CR>")
		vim.keymap.set("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>")
		vim.keymap.set("n", "<leader>hD", function()
			require("gitsigns").diffthis("~")
		end)
		vim.keymap.set("n", "<leader>hm", function()
			require("gitsigns").diffthis("main")
		end)
		vim.keymap.set("n", "<leader>hM", diffThisBranch)
		vim.keymap.set("n", "<leader>td", "<cmd>Gitsigns toggle_deleted<CR>")

		-- Text object
		vim.keymap.set("o", "ih", ":<C-U>Gitsigns select_hunk<CR>")
		vim.keymap.set("x", "ih", ":<C-U>Gitsigns select_hunk<CR>")
	end,
})
