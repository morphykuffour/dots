--debugging
local dap, dapui = require("dap"), require("dapui")

vim.fn.sign_define("DapBreakpoint", { text = "ß", texthl = "", linehl = "", numhl = "" })
vim.fn.sign_define("DapBreakpointCondition", { text = "ü", texthl = "", linehl = "", numhl = "" })
vim.fn.sign_define("DapStopped", { text = "ඞ", texthl = "Error" })

require("nvim-dap-virtual-text").setup({
	enabled = true,
	enabled_commands = false,
	highlight_changed_variables = true,
	highlight_new_as_changed = true,
	commented = false,
	show_stop_reason = true,
	virt_text_pos = "eol",
	all_frames = false,
})

dap.adapters.nlua = function(callback, config)
	callback({ type = "server", host = config.host, port = config.port })
end

dap.configurations.lua = {
	{
		type = "nlua",
		request = "attach",
		name = "Attach to running Neovim instance",
		host = function()
			return "127.0.0.1"
		end,
		port = function()
			-- local val = tonumber(vim.fn.input('Port: '))
			-- assert(val, "Please provide a port number")
			local val = 54231
			return val
		end,
	},
}

require("dapui").setup()

-- python setup
local debugpy = vim.fn.expand("$HOME/miniconda3/envs/debugpy/bin/python")
require("dap-python").setup(debugpy)
require("dap-python").test_runner = "pytest"

dap.configurations.python = {
	{
		-- The first three options are required by nvim-dap
		type = "python", -- the type here established the link to the adapter definition: `dap.adapters.python`
		request = "launch",
		name = "Launch file",

		-- Options below are for debugpy, see https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings for supported options

		program = "${file}", -- This configuration will launch the current file if used.
		pythonPath = function()
			-- debugpy supports launching an application with a different interpreter then the one used to launch debugpy itself.
			-- The code below looks for a `venv` or `.venv` folder in the current directly and uses the python within.
			-- You could adapt this - to for example use the `VIRTUAL_ENV` environment variable.

			local cwd_only = vim.fn.fnamemodify(vim.fn.getcwd(), ":t:h")
			local debug_env = "$HOME/miniconda3/envs/" .. cwd_only .. "/bin/python"

			if vim.fn.executable(debug_env) == 1 then
				return debug_env
			else
				return vim.fn.expand("$HOME/miniconda3/bin/python")
			end
		end,
	},
}

-- /home/morp/miniconda3/envs/bookmark-zettelkasten/bin/python
-- /home/morp/miniconda3/bin/python

-- dap.configurations.python = {
-- 	{
-- 		type = "python",
-- 		request = "launch",
-- 		name = "Build api",
-- 		program = "${file}",
-- 		args = { "--target", "api" },
-- 		console = "integratedTerminal",
-- 	},
-- }

dap.listeners.after.event_initialized["dapui_config"] = function()
	dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
	dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
	dapui.close()
end
