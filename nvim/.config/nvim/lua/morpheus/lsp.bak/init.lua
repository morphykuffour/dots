local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
	return
end

require("morpheus.lsp.configs")
require("morpheus.lsp.handlers").setup()
require("morpheus.lsp.null-ls")
