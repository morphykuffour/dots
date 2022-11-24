local autocmd = vim.api.nvim_create_autocmd
-- Autoformatters
autocmd("BufWritePost", { pattern = "*.lua", command = "lua require('stylua').format()" })
autocmd("BufWritePost", { pattern = "*.py", command = "AutoFormatBuffer yapf" })
-- autocmd("BufWritePost", { pattern = "*.go", command = "AutoFormatBuffer gofmt" })
-- autocmd("BufWritePost", { pattern = "*.rs", command = "AutoFormatBuffer rustfmt" })
-- autocmd("BufWritePost", { pattern = "*.c", command = "AutoFormatBuffer clang-format" })
-- autocmd("BufWritePost", { pattern = "*.cpp", command = "AutoFormatBuffer clang-format" })
