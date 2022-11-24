local buffhandle = vim.api.nvim_create_buf("My buffer", 0)
vim.api.nvim_buf_set_lines(buffhandle, 5, -1, false, {
	"hello",
	"world",
})
