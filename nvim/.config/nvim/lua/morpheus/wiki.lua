local wiki = {}

wiki.make_diary_entry = function()
	vim.cmd(string.format(":85vnew ~/iCloud/notes/note-%s.md", os.date("%y_%m_%d")))
end

wiki.make_todo = function()
	local bufnr = vim.fn.bufnr(vim.fn.expand("~/iCloud/todo/todo.md"), true)

	for _, win_id in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
		local open_bufnr = vim.api.nvim_win_get_buf(win_id)
		if open_bufnr == bufnr then
			return vim.api.nvim_set_current_win(win_id)
		end
	end

	vim.api.nvim_win_set_buf(0, bufnr)
end

return wiki
