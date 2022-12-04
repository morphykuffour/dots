--vimrc
-- Author: Morphy Kuffour
-- Alias:  JediGrandMaster

-- TODO fix lspconfig https://www.vikasraj.dev/blog/lsp-neovim-retrospective
-- TODO fix Goyo mode remove extra statusbars nvim
-- TODO TSContextDisable for all lua files
-- Personal settings
vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("morpheus.gitsigns")
require("morpheus.todo")

-- sql
-- TODO: https://www.reddit.com/r/neovim/comments/x8708y/how_to_use_vim_or_neovim_for_sql/
require("lspconfig").sqls.setup({
	on_attach = function(client, bufnr)
		require("sqls").on_attach(client, bufnr)
	end,
})
vim.g.snippets = "luasnip"
vim.g.loaded_matchparen = 1

require("morpheus.utils")
Jcall(require, "morpheus/plugins")

-- reload vimrc on save
-- local autocmd = vim.api.nvim_create_autocmd
-- autocmd("BufWritePost", {
-- 	pattern = vim.env.MYVIMRC,
-- 	callback = function()
-- 		dofile(vim.env.MYVIMRC)
-- 	end,
-- })

-- options
local opt = vim.opt
opt.wildignore = "__pycache__"
opt.wildignore = opt.wildignore + { "*.o", "*~", "*.pyc", "*pycache*" }
opt.pumblend = 17
opt.wildmode = "longest:full"
opt.wildoptions = "pum"
vim.o.lazyredraw = false
opt.termguicolors = true
opt.showmode = false
opt.showcmd = true
opt.cmdheight = 1
opt.incsearch = true
opt.showmatch = true
opt.relativenumber = true
opt.number = true
opt.ignorecase = true
opt.smartcase = true
opt.hidden = true
opt.equalalways = false
opt.splitright = true
opt.splitbelow = true
opt.updatetime = 1000
opt.hlsearch = true
opt.scrolloff = 10
opt.undofile = true
opt.autoindent = true
opt.cindent = true
opt.wrap = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true
opt.breakindent = true
opt.showbreak = string.rep(" ", 3) -- Make it so that long lines wrap smartly
opt.linebreak = true
-- opt.foldmethod = "marker"
-- opt.foldlevel = 0
opt.modelines = 1
opt.belloff = "all" -- Just turn the dang bell off
opt.clipboard = "unnamedplus"
opt.inccommand = "split"
opt.swapfile = false -- Living on the edge
opt.shada = { "!", "'1000", "<50", "s10", "h" }
opt.mouse = "a"
opt.mouse = ""
opt.formatoptions = opt.formatoptions
	- "a" -- Auto formatting is BAD.
	- "t" -- Don't auto format my code. I got linters for that.
	+ "c" -- In general, I like it when comments respect textwidth
	+ "q" -- Allow formatting comments w/ gq
	- "o" -- O and o, don't continue comments
	+ "r" -- But do continue when pressing enter.
	+ "n" -- Indent past the formatlistpat, not underneath it.
	+ "j" -- Auto-remove comments if possible.
	- "2" -- I'm not in gradeschool anymore
opt.joinspaces = false -- Two spaces and grade school, we're done
-- set fillchars=eob:~
opt.fillchars = { eob = "~" }
opt.diffopt = { "internal", "filler", "closeoff", "hiddenoff", "algorithm:minimal" }
opt.cursorline = true -- Highlight the current line
local group = vim.api.nvim_create_augroup("CursorLineControl", { clear = true })
local set_cursorline = function(event, value, pattern)
	vim.api.nvim_create_autocmd(event, {
		group = group,
		pattern = pattern,
		callback = function()
			vim.opt_local.cursorline = value
		end,
	})
end

set_cursorline("WinLeave", false)
set_cursorline("WinEnter", true)
set_cursorline("FileType", false, "TelescopePrompt")

-- Themes
-- vim.cmd("colorscheme github_dark_default")
vim.cmd("colorscheme gruvbox")

-- Setup LSPs, DAPs, Linters
require("mason").setup({
	ui = {
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
	},
	-- The directory in which to install packages.
	-- install_root_dir = path.concat { vim.fn.stdpath "data", "mason" },
})

require("mason-lspconfig").setup({
	ensure_installed = { "sumneko_lua", "rust_analyzer" },
	-- automatic_installation = false,
	-- Configured servers list: rust_analyzer, clojure_lsp, pyright, rnix, sumneko_lua
})

-- require("lualine").setup({
-- 	options = {
-- 		theme = "github_dark", -- or you can assign github_* themes individually.
-- 		-- ... your lualine config
-- 	},
--   tabline = {
--     lualine_a = {
--       {
--         'buffers',
--         show_filename_only = false,
--         mode = 2,
--         -- max_length = vim.o.columns * 1 / 2,
--       }
--     },
--   }
-- })

vim.opt.statusline = " <b>[%N]  %< %{FugitiveHead()}  %F %m %r %w %= %y %{&fileencoding?&fileencoding:&encoding} [%{&fileformat}]  Ln %l, Col %c "

vim.g.loaded_python_provider = 0 -- disable python2


if os.getenv("NIX_PATH") ~= nil or os.getenv("NIX_PATH") ~= "" then
	vim.g.python3_host_prog = "/home/morp/.nix-profile/bin/python3"
end
-- vim.g.python3_host_prog = "/usr/bin/python3" -- nix takes care of this

-- reload vimrc on save
local autocmd = vim.api.nvim_create_autocmd
-- autocmd("BufWritePost", {
-- 	pattern = vim.env.MYVIMRC,
-- 	callback = function()
-- 		dofile(vim.env.MYVIMRC)
-- 	end,
-- })
--
-- Automatically source and re-compile packer whenever you save this init.lua
-- local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
-- vim.api.nvim_create_autocmd("BufWritePost", {
-- 	command = "source <afile> | PackerCompile",
-- 	group = packer_group,
-- 	pattern = vim.fn.expand("$MYVIMRC"),
-- })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = "*",
})

-- source vimfiles
vim.cmd([[runtime! vimfiles/*.vim]])
-- nvim treesister NixOs

-- gx => open url in browser
if vim.fn.has("wsl") then
	vim.g.netrw_browsex_viewer = "/usr/bin/wslview"
elseif vim.fn.has("mac") then
	vim.g.netrw_browsex_viewer = "open"
elseif vim.fn.has("linux") then
	vim.g.netrw_browsex_viewer = "/usr/bin/xdg-open"
end

-- completion.lua
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.shortmess:append("c")

local cmp = require("cmp")
local source_mapping = {
	buffer = "[buffer]",
	nvim_lsp = "[LSP]",
	nvim_lua = "[api]",
	cmp_tabnine = "[tn]",
	luasnip = "[snip]",
	path = "[path]",
	dadbod_cmp = "[DB]",
}

local ok, lspkind = pcall(require, "lspkind")
if not ok then
	return
end

lspkind.init()

cmp.setup({
	mapping = {
		["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-u>"] = cmp.mapping.scroll_docs(-4),
		["<C-d>"] = cmp.mapping.scroll_docs(4),
		["<c-space>"] = cmp.mapping.complete(),
		["<c-y>"] = cmp.mapping(
			cmp.mapping.confirm({
				behavior = cmp.ConfirmBehavior.Insert,
				select = true,
			}),
			{ "i", "c" }
		),
		["<tab>"] = cmp.config.disable,
	},
	sources = {
		{ name = "cmp_tabnine" },
		{ name = "nvim_lsp" },
		{ name = "buffer", keyword_length = 3 },
		{ name = "nvim_lua" },
		{ name = "path" },
		{ name = "luasnip" },
		{ name = "vim-dadbod-completion" },
	},

	sorting = {
		-- TODO: Would be cool to add stuff like "See variable names before method names" in rust, or something like that.
		comparators = {
			cmp.config.compare.offset,
			cmp.config.compare.exact,
			cmp.config.compare.score,

			-- copied from cmp-under, but I don't think I need the plugin for this.
			-- I might add some more of my own.
			function(entry1, entry2)
				local _, entry1_under = entry1.completion_item.label:find("^_+")
				local _, entry2_under = entry2.completion_item.label:find("^_+")
				entry1_under = entry1_under or 0
				entry2_under = entry2_under or 0
				if entry1_under > entry2_under then
					return false
				elseif entry1_under < entry2_under then
					return true
				end
			end,

			cmp.config.compare.kind,
			cmp.config.compare.sort_text,
			cmp.config.compare.length,
			cmp.config.compare.order,
		},
	},

	-- Youtube: mention that you need a separate snippets plugin
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},

	formatting = {
		format = function(entry, vim_item)
			vim_item.kind = lspkind.presets.default[vim_item.kind]
			local menu = source_mapping[entry.source.name]
			if entry.source.name == "cmp_tabnine" then
				if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
					menu = entry.completion_item.data.detail .. " " .. menu
				end
				vim_item.kind = ""
			end
			vim_item.menu = menu
			return vim_item
		end,
	},

	experimental = {
		native_menu = false,
		ghost_text = false,
	},
})


local tabnine = require("cmp_tabnine.config")
tabnine:setup({
	max_lines = 1000,
	max_num_results = 20,
	sort = true,
	run_on_every_keystroke = true,
	snippet_placeholder = "..",
})

_ = vim.cmd([[
  augroup CmpZsh
    au!
    autocmd Filetype zsh lua require'cmp'.setup.buffer { sources = { { name = "zsh" }, } }
  augroup END
]])

-- LSP settings for nixos
vim.cmd([[ packadd nvim-lspconfig]])

vim.api.nvim_set_keymap("n", "<leader>L", "<cmd>lua vim.diagnostic.setloclist()<cr>", { noremap = true, silent = true })
local keymap = vim.api.nvim_set_keymap
vim.api.nvim_set_keymap(
	"n",
	"<leader>ds",
	"<cmd>lua vim.lsp.buf.document_symbol()<cr>",
	{ noremap = true, silent = true }
)

keymap("i", "<c-h>", "<cmd>lua vim.lsp.buf.signature_help()<cr>", { noremap = true, silent = true })
keymap("n", "<leader>w", "<cmd>lua vim.lsp.buf.workspace_symbol()<cr>", { noremap = true, silent = true })
keymap("n", "<leader>e", "<cmd>lua vim.lsp.buf.rename()<cr>", { noremap = true, silent = true })
keymap("n", "<leader>d.", "<cmd>lua vim.diagnostic.goto_next({ float = true })<cr>", { noremap = true, silent = true })
keymap("n", "<leader>d,", "<cmd>lua vim.diagnostic.goto_prev({ float = true })<cr>", { noremap = true, silent = true })
keymap("n", "]i", "<cmd>lua vim.lsp.buf.implementation()<cr>", { noremap = true, silent = true })
keymap("n", "]t", "<cmd>lua vim.lsp.buf.type_definition()<cr>", { noremap = true, silent = true })
keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { noremap = true, silent = true })
keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<cr>", { noremap = true, silent = true })
keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", { noremap = true, silent = true })
keymap("n", "<C-p>", "<cmd>lua vim.diagnostic.open_float()<cr>", { noremap = true, silent = true })

local nvim_lsp = require("lspconfig")
local on_attach = function(client, bufnr)
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
end

-- TODO replace with mason nvim when production ready
-- local configs = require("lspconfig/configs")
nvim_lsp.util.default_config = vim.tbl_extend("force", nvim_lsp.util.default_config, { on_attach = on_attach })
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" })

nvim_lsp.rust_analyzer.setup({ on_attach = on_attach })
nvim_lsp.clojure_lsp.setup({ on_attach = on_attach })
nvim_lsp.pyright.setup({ on_attach = on_attach })
nvim_lsp.rnix.setup({ on_attach = on_attach })
nvim_lsp.ccls.setup({ on_attach = on_attach })
-- nvim_lsp.gopls.setup({ on_attach = on_attach })

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
nvim_lsp.sumneko_lua.setup({

	settings = {
		Lua = {
			runtime = {
				version = "LuaJIT",
				path = vim.split(package.path, ";"),
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
				},
			},
		},
	},
})

-- debug
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

dap.listeners.after.event_initialized["dapui_config"] = function()
	dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
	dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
	dapui.close()
end

require("diaglist").init({
	-- optional settings
	-- below are defaults
	debug = false,

	-- increase for noisy servers
	debounce_ms = 150,
})

-- In init.lua or filetype.nvim's config file
-- Do not source the default filetype.vim
vim.g.did_load_filetypes = 1
require("filetype").setup({
	overrides = {
		extensions = {
			-- Set the filetype of *.pn files to potion
			pn = "potion",
			markdown = "markdown",
		},
		literal = {
			-- Set the filetype of files named "MyBackupFile" to lua
			MyBackupFile = "lua",
			Vagrantfile = "ruby",
		},
		complex = {
			-- Set the filetype of any full filename matching the regex to gitconfig
			[".*git/config"] = "gitconfig", -- Included in the plugin
			["/tmp/zsh*"] = "bash", -- Included in the plugin
			[".*zsh*"] = "bash", -- include zsh files
		},

		-- The same as the ones above except the keys map to functions
		function_extensions = {
			["cpp"] = function()
				vim.bo.filetype = "cpp"
				-- Remove annoying indent jumping
				vim.bo.cinoptions = vim.bo.cinoptions .. "L0"
			end,
			["pdf"] = function()
				vim.bo.filetype = "pdf"
				if vim.fn.has("wsl") then
					-- Open in MS-Edge PDF viewer
					vim.fn.jobstart("wslview " .. '"' .. vim.fn.expand("%") .. '"')
				elseif vim.fn.has("mac") then
					-- Open in PDF viewer (Skim.app)
					vim.fn.jobstart("open -a skim " .. '"' .. vim.fn.expand("%") .. '"')
					-- Open in zathura PDF viewer
				elseif vim.fn.has("linux") then
					vim.fn.jobstart("zathura " .. '"' .. vim.fn.expand("%") .. '"')
				end
			end,
		},
		function_literal = {
			Brewfile = function()
				vim.cmd("syntax off")
			end,
		},
		function_complex = {
			["*.math_notes/%w+"] = function()
				vim.cmd("iabbrev $ $$")
			end,
		},

		shebang = {
			-- Set the filetype of files with a dash shebang to sh
			dash = "sh",
		},
	},
})

-- nixos
local nix_group = vim.api.nvim_create_augroup("nix", { clear = true })
autocmd("BufWritePost", {
	group = nix_group,
	pattern = "/etc/nixos/configuration.nix",
	callback = function()
		local bufnr = vim.api.nvim_create_buf()
		vim.fn.jobstart({ "sudo", "nixos-rebuild", "switch" }, {
			stdout_buffered = true,
			on_stdout = function(_, data)
				if data then
					vim.api.nvim_buf_set_keymap(bufnr, -1, -1, false, data)
				end
			end,
			on_stderr = function(_, data)
				if data then
					vim.api.nvim_buf_set_keymap(bufnr, -1, -1, false, data)
				end
			end,
		})
	end,
})

autocmd("BufWinEnter", {
	group = nix_group,
	pattern = { "*.nix" },
	command = "TSBufDisable highlight",
})

autocmd("BufWritePost", {
	group = nix_group,
	pattern = vim.fn.expand("$HOME") .. "/nix/nixpkgs/.config/nixpkgs/home.nix",
	callback = function()
		local bufnr = vim.api.nvim_create_buf()
		vim.fn.jobstart({ "home-manager", "switch" }, {
			stdout_buffered = true,
			on_stdout = function(_, data)
				if data then
					vim.api.nvim_buf_set_keymap(bufnr, -1, -1, false, data)
				end
			end,
			on_stderr = function(_, data)
				if data then
					vim.api.nvim_buf_set_keymap(bufnr, -1, -1, false, data)
				end
			end,
		})
	end,
})

-- auto resize
local wr_group = vim.api.nvim_create_augroup("WinResize", { clear = true })

vim.api.nvim_create_autocmd("VimResized", {
	group = wr_group,
	pattern = "*",
	command = "wincmd =",
	desc = "Automatically resize windows when the host window size changes.",
})

vim.api.nvim_create_autocmd("TermOpen", {
	callback = function()
		vim.opt_local.number = false
		vim.opt_local.relativenumber = false
	end,
})

require("godbolt").setup({
	languages = {
		c = { compiler = "cg112", options = {} },
		cpp = { compiler = "g112", options = {} },
		rust = { compiler = "r1590", options = {} },
	},
	quickfix = {
		enable = false,
		auto_open = false,
	},
	url = "https://godbolt.org",
})

---- testing out Hop.nvim with vim.schedule

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

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

--vim.keymap.set("n", "vo", function()
--	vim.cmd([[:HopLineStart]])
--	vim.schedule(function()
--		vim.cmd([[normal! o]])
--		vim.cmd([[startinsert]])
--	end)
--end, opts)
--vim.keymap.set("n", "<Leader>vo", function()
--	vim.cmd([[:HopLineStart]])
--	vim.schedule(function()
--		vim.cmd([[normal! o]])
--		vim.cmd([[normal! o]])
--		vim.cmd([[startinsert]])
--	end)
--end, opts)

--vim.keymap.set("n", "vO", function()
--	vim.cmd([[:HopLineStart]])
--	vim.schedule(function()
--		vim.cmd([[normal! O]])
--		vim.cmd([[normal! O]])
--		vim.cmd([[startinsert]])
--	end)
--end, opts)
--vim.keymap.set("n", "<Leader>vO", function()
--	vim.cmd([[:HopLineStart]])
--	vim.schedule(function()
--		vim.cmd([[normal! O]])
--		vim.cmd([[normal! O]])
--		vim.cmd([[startinsert]])
--	end)
--end, opts)

--vim.keymap.set("n", "vY", function()
--	vim.cmd([[:HopLineStart]])
--	vim.schedule(function()
--		vim.cmd([[:normal @f]])
--	end)
--end, opts)

--vim.keymap.set("n", "your_keymap", function()
--	vim.cmd([[:HopLineStart]]) --> Best thing ever in the history of mankind
--	vim.schedule(function()
--		vim.cmd([[:normal ojja ]]) --> POGG
--		vim.cmd("startinsert")
--	end)
--end, opts)

--vim.keymap.set("n", "vy", function()
--	require("tsht").nodes()
--	vim.schedule(function()
--		vim.cmd([[normal! c]])
--		vim.cmd([[startinsert]])
--	end)
--end, opts)

---- hop.lua
--vim.api.nvim_set_keymap('n', 'f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>", {})
--vim.api.nvim_set_keymap('n', 'F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>", {})
--vim.api.nvim_set_keymap( "o", "f", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<cr>", {})
--vim.api.nvim_set_keymap( "o", "F", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<cr>", {})
--vim.api.nvim_set_keymap( "", "t", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>", {})
--vim.api.nvim_set_keymap( "", "T", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>", {})
--vim.api.nvim_set_keymap( "n", "<leader>e", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>", {})
--vim.api.nvim_set_keymap( "v", "<leader>e", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>", {})
--vim.api.nvim_set_keymap( "o", "<leader>e", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<cr>", {})

keymap("n", "<leader>w", "<cmd>w<cr>", opts)
keymap("n", "<A-q>", ":Ranger<CR>", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Move text up and down
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Switch between last two buffers
keymap("n", "<leader><leader>", "<C-^>", opts)
keymap("n", "Q", "<nop>", opts)

-- "Edit configs
keymap("n", "<leader>vc", "<cmd>e $MYVIMRC<cr>")
keymap("n", "<leader>tc", ":edit $HOME/dotfiles/tmux/.tmux.conf<cr>")
keymap("n", "<leader>zc", ":edit $HOME/dotfiles/zsh/.zshrc<cr>")

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Better terminal navigation
-- move between vim panes
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Harpoon mappings
keymap("n", "<leader>hm", [[:lua require("harpoon.mark").add_file()<cr>]])
keymap("n", "<leader>hv", [[:lua require("harpoon.ui").toggle_quick_menu()<cr>]])

keymap("n", "<leader>st", ":Startify<CR>")
keymap("n", "<leader>so", ":source %<CR>")

keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>")
keymap("n", "<leader>w", "<Cmd>w<CR>")

-- buffer switching
keymap("n", "<leader>,", "<cmd>bprev<cr>")
keymap("n", "<leader>.", "<cmd>bnext<cr>")
-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)
keymap("n", "<leader>bf", ":bfirst<CR>")
keymap("n", "<leader>bl", ":blast<CR>")

-- pane switching
keymap("n", "<c-j>", "<c-w>j")
keymap("n", "<c-k>", "<c-w>k")
keymap("n", "<c-h>", "<c-w>h")
keymap("n", "<c-l>", "<c-w>l")
keymap("c", "<c-j>", "<Down>")
keymap("c", "<c-k>", "<Up>")

-- tab switching
keymap("n", "<leader>tn", ":tabnew<CR>")
keymap("n", "<leader>t,", ":tabprev<CR>")
keymap("n", "<leader>t.", ":tabnext<CR>")
keymap("n", "c,", "<cmd>cprev<CR>zzzv")
keymap("n", "c.", "<cmd>cnext<CR>zzzv")

keymap("n", "di$", "T$dt$")
keymap("n", "ci$", "T$ct$")
keymap("n", "<leader>hn", "<cmd>:setlocal nonumber norelativenumber<CR>")
keymap("n", "<leader>hN", "<cmd>:setlocal number relativenumber<CR>")
keymap("n", "-", "<C-W><")
keymap("n", "_", "<C-W>>")
keymap("n", "=", "<C-W>-")
keymap("n", "+", "<C-W>+")

-- Remap for dealing with word wrap
keymap("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
keymap("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
keymap("n", "<leader>sf", "/\\c")
keymap("n", "<leader>sb", "?\\c")
keymap("n", "<leader>nh", "<cmd>noh<CR>")

-- Telescope keymaps
local telescope = require("telescope")
keymap("n", "<leader>/", function()
	require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
		winblend = 10,
		previewer = false,
	}))
end, { desc = "[/curr] Fuzzily search in current buffer]" })

keymap("n", "<space>do", function()
	require("telescope.builtin").find_files({
		prompt_title = "< dotfiles >",
		cwd = vim.env.DOTFILES,
		hidden = true,
	})
end, { desc = "[/dot] search dotfiles]" })

--[[
_G.search_vimrc = function()
end
--]]

--[[
require("telescope.builtin").find_files({
	prompt_title = "< vimrc >",
	cwd = "~/.config/nvim/",
	hidden = true,
})
--]]

_G.installed_plugins = function()
	require("telescope.builtin").find_files({
		prompt_title = "< searching installed plugins >",
		cwd = vim.fn.stdpath("data") .. "/site/pack/packer/start/",
	})
end

-- keymap("<space>fp", installed_plugins)

_G.search_all_files = function()
	require("telescope.builtin").find_files({
		prompt_title = "< searching all files >",
		find_command = { "rg", "--no-ignore", "--files" },
	})
end

keymap("n", "<A-x>", function()
	require("telescope.builtin").keymaps(require("telescope.themes").get_ivy({
		winblend = 5,
		previewer = false,
	}))
end, { desc = "[/keys] execute keymaps or functions]" })

keymap("n", "<leader>fs", function()
	require("telescope.builtin").grep_string({ search = vim.fn.input("Grep For > ") })
end, { desc = "[/gr] grep string from pwd]" })

keymap("n", "<leader>fb", "<cmd> Telescope file_browser<CR>", { desc = "[/fb] file browser search]" })

keymap("n", "<leader>bb", function()
	require("telescope.builtin").buffers(require("telescope.themes").get_ivy({
		winblend = 5,
		previewer = false,
	}))
end, { desc = "[/buf] search current nvim buffers]" })

keymap("n", "<leader>fo", function()
	require("telescope.builtin").oldfiles(require("telescope.themes").get_ivy({
		winblend = 5,
		previewer = false,
	}))
end, { desc = "[/old] old files search]" })

keymap("n", "<leader>ff", function()
	require("telescope.builtin").find_files(require("telescope.themes").get_ivy({
		winblend = 5,
		previewer = false,
	}))
end, { desc = "[/ff] find files search]" })

local previewers = require("telescope.previewers")
local builtin = require("telescope.builtin")

keymap("n", "<leader>fh", "<cmd> Telescope help_tags<CR>")
keymap("n", "<leader>lg", "<cmd> Telescope live_grep<CR>")
keymap("n", "<leader>lr", "<cmd> Telescope lsp_references<CR>")
keymap("n", "<leader>ft", "<cmd> TodoTelescope<CR>")

-- Extension mappings
keymap("n", "<leader>fm", "<cmd>Telescope bookmarks<cr>")
keymap("n", "<leader>fc", "<cmd>Telescope neoclip<cr>")
keymap("n", "<c-f>", "<cmd>Telescope find_files hidden=true<CR>")

-- Git
local delta_bcommits = previewers.new_termopen_previewer({
	get_command = function(entry)
		return {
			"git",
			"-c",
			"core.pager=delta",
			"-c",
			"delta.side-by-side=false",
			"diff",
			entry.value .. "^!",
			"--",
			entry.current_file,
		}
	end,
})

local delta = previewers.new_termopen_previewer({
	get_command = function(entry)
		return { "git", "-c", "core.pager=delta", "-c", "delta.side-by-side=false", "diff", entry.value .. "^!" }
	end,
})

Delta_git_commits = function(opts)
	opts = opts or {}
	opts.previewer = {
		delta,
		previewers.git_commit_message.new(opts),
		previewers.git_commit_diff_as_was.new(opts),
	}
	builtin.git_commits(opts)
end

Delta_git_bcommits = function(opts)
	opts = opts or {}
	opts.previewer = {
		delta_bcommits,
		previewers.git_commit_message.new(opts),
		previewers.git_commit_diff_as_was.new(opts),
	}
	builtin.git_bcommits(opts)
end
keymap("n", "<leader>dgc", "<cmd>lua Delta_git_commits()<CR>", opts)
keymap("n", "<leader>dgb", "<cmd>lua Delta_git_bcommits()<CR>", opts)

-- Insert mode in NEOGIT_COMMIT_EDIT_EDITMSG
local git_group = vim.api.nvim_create_augroup("git_group", { clear = true })
autocmd({ "BufRead", "BufWinEnter" }, {
	group = git_group,
	pattern = "NEOGIT_COMMIT_EDIT_EDITMSG",
	command = "startinsert | 1",
})

autocmd("FileType", {
	group = git_group,
	pattern = { "gitcommit", "gitrebase" },
	command = "startinsert | 1",
})

require("git-conflict").setup({
	default_mappings = true,
	disable_diagnostics = false,
	highlights = {
		incoming = "DiffText",
		current = "DiffAdd",
	},
})

require("neogit").setup({
	disable_commit_confirmation = true,
	disable_insert_on_commit = false,
	integrations = {
		diffview = true,
	},
})
keymap("n", "<leader>gg", "<cmd>lua require('neogit').open({ kind = 'split' })<cr>")
keymap("n", "<leader>gd", "<cmd> DiffviewOpen<CR>")
keymap("n", "<leader>cls", "<cmd>SymbolsOutline<cr>")

keymap("n", "<leader>sv", "<cmd>lua ReloadConfig()<cr>")
vim.api.nvim_create_user_command("CopyBufferName", function()
	vim.cmd("echo expand('%:p')")
	vim.cmd("let @+ = expand('%:p')")
	vim.cmd('echo "Full path of " . expand(\'%:t\') . " was copied to system clipboard"')
end, {})
keymap("n", "<leader>bn", "<cmd> CopyBufferName()<cr>")
keymap("n", "<leader>sa", "<cmd>Scratch<cr>")

keymap("n", "gx", "<Plug>(openbrowser-smart-search)<cr>")
keymap("v", "gx", "<Plug>(openbrowser-smart-search)<cr>")

-- writing
keymap("n", "<leader>wd", function()
	vim.cmd(string.format(":85vnew ~/Dropbox/notes/note-%s.md", os.date("%y_%m_%d")))
end, { desc = "[/de] notes]", noremap = true })

keymap("n", "<leader>wt", function()
	local bufnr = vim.fn.bufnr(vim.fn.expand("~/Dropbox/todo/todo.md"), true)

	for _, win_id in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
		local open_bufnr = vim.api.nvim_win_get_buf(win_id)
		if open_bufnr == bufnr then
			return vim.api.nvim_set_current_win(win_id)
		end
	end

	vim.api.nvim_win_set_buf(0, bufnr)
end, { desc = "[/todo] todo]", noremap = true })

-- debugging remaps
keymap("n", "<leader>ddd", '<cmd>lua require("osv").launch()<cr>')
keymap("n", "<leader>ddr", '<cmd>lua require("osv").run_this()<cr>')
keymap("n", "<leader>db", '<cmd>lua require("dap").toggle_breakpoint()<cr>')
keymap("n", "<leader>dj", "<cmd>lua require'dap'.step_over()<cr>")
keymap("n", "<leader>dl", "<cmd>lua require'dap'.step_into()<cr>")
keymap("n", "<leader>dk", "<cmd>lua require'dap'.step_out()<cr>")
keymap("n", "<leader>dc", "<cmd>lua require'dap'.continue()<cr>")
keymap("n", "<leader>dr", "<cmd>lua require'dap'.repl.open()<cr>")
keymap("n", "<leader>dB", ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>", opts)
keymap("n", "<leader>lp", ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>", opts)
keymap("n", "<leader>dw", "<cmd>lua require('diaglist').open_all_diagnostics()<cr>")
keymap("n", "<leader>d0", "<cmd>lua require('diaglist').open_buffer_diagnostics()<cr>")
keymap("n", "<leader>m", "<cmd> MaximizerToggle!<CR>")

keymap("n", "<leader>dn", "<cmd> lua require('dap-python').test_method()<CR>", opts)
keymap("n", "<leader>df", "<cmd> lua require('dap-python').test_class()<CR>", opts)
keymap("v", "<leader>ds", "<ESC> <cmd> lua require('dap-python').debug_selection()<CR>", opts)

-- vim.keymap.set("n", "<F5>", ":lua require'dap'.continue()<CR>")
-- vim.keymap.set("n", "<F3>", ":lua require'dap'.step_over()<CR>")
-- vim.keymap.set("n", "<F2>", ":lua require'dap'.step_into()<CR>")
-- vim.keymap.set("n", "<F12>", ":lua require'dap'.step_out()<CR>")
-- vim.keymap.set("n", "<leader>br", ":lua require'dap'.toggle_breakpoint()<CR>")
-- vim.keymap.set("n", "<leader>B", ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
-- vim.keymap.set("n", "<leader>lp", ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>")
-- vim.keymap.set("n", "<leader>dr", ":lua require'dap'.repl.open()<CR>")
require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip").filetype_extend("ruby", { "rails" })
require("luasnip").filetype_extend("lua", { "lua" })
require("luasnip").filetype_extend("js", { "js" })
require("luasnip").filetype_extend("cpp", { "cpp" })
require("luasnip").filetype_extend("python", { "python" })
require("luasnip").filetype_extend("latex", { "latex" })
-- require("luasnip").filetype_extend("markdown", { "cpp" })
-- require("luasnip").filetype_extend("org", { "org" })
require("luasnip").filetype_extend("r", { "r" })
require("luasnip").filetype_extend("shell", { "shell" })

local ls = require("luasnip")
-- some shorthands...
local snip = ls.snippet
local node = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local func = ls.function_node
local choice = ls.choice_node
local dynamicn = ls.dynamic_node

local date = function()
	return { os.date("%Y-%m-%d") }
end

ls.add_snippets(nil, {
	all = {
		snip({
			trig = "date",
			namr = "Date",
			dscr = "Date in the form of YYYY-MM-DD",
		}, {
			func(date, {}),
		}),
	},
})

local actions = require("telescope.actions")
local fb_actions = require("telescope").extensions.file_browser.actions
local fb_utils = require("telescope._extensions.file_browser.utils")
local action_state = require("telescope.actions.state")
local Job = require("plenary.job")

require("telescope").setup({
	defaults = {

		prompt_prefix = " ",
		selection_caret = " ",
		path_display = { "smart" },

		mappings = {
			i = {
				["<C-n>"] = actions.cycle_history_next,
				["<C-p>"] = actions.cycle_history_prev,
				["<C-j>"] = actions.move_selection_next,
				["<C-k>"] = actions.move_selection_previous,
				["<C-c>"] = actions.close,
				["<Down>"] = actions.move_selection_next,
				["<Up>"] = actions.move_selection_previous,
				["<CR>"] = actions.select_default,
				["<C-x>"] = actions.select_horizontal,
				["<C-v>"] = actions.select_vertical,
				["<C-t>"] = actions.select_tab,
				["<C-u>"] = actions.preview_scrolling_up,
				["<C-d>"] = actions.preview_scrolling_down,
				["<PageUp>"] = actions.results_scrolling_up,
				["<PageDown>"] = actions.results_scrolling_down,
				["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
				["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
				["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
				["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
				["<C-l>"] = actions.complete_tag,
				["<C-_>"] = actions.which_key, -- keys from pressing <C-/>
			},

			n = {
				["<esc>"] = actions.close,
				["<CR>"] = actions.select_default,
				["<C-x>"] = actions.select_horizontal,
				["<C-v>"] = actions.select_vertical,
				["<C-t>"] = actions.select_tab,
				["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
				["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
				["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
				["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
				["j"] = actions.move_selection_next,
				["k"] = actions.move_selection_previous,
				["H"] = actions.move_to_top,
				["M"] = actions.move_to_middle,
				["L"] = actions.move_to_bottom,
				["<Down>"] = actions.move_selection_next,
				["<Up>"] = actions.move_selection_previous,
				["gg"] = actions.move_to_top,
				["G"] = actions.move_to_bottom,
				["<C-u>"] = actions.preview_scrolling_up,
				["<C-d>"] = actions.preview_scrolling_down,
				["<PageUp>"] = actions.results_scrolling_up,
				["<PageDown>"] = actions.results_scrolling_down,
			},
		},
	},
	pickers = {
		-- Default configuration for builtin pickers goes here:
		-- picker_name = {
		--   picker_config_key = value,
		--   ...
		-- }
		-- Now the picker_config_key will be applied every time you call this
		-- builtin picker
	},
	extensions = {
		file_browser = {
			theme = "ivy",
			hijack_netrw = true,
			mappings = {
				["i"] = {
					["<C-h>"] = fb_actions.goto_home_dir,

					-- mass renameing with edir
					["<C-r>"] = function(prompt_bufnr)
						-- bulk rename with edir
						-- https://github.com/bulletmark/edir
						-- local quiet = action_state.get_current_picker(prompt_bufnr).finder.quiet
						local selections = fb_utils.get_selected_files(prompt_bufnr, true)
						Job:new({ "edir", selections }):start()
					end,
				},
				["n"] = {
					-- your custom normal mode mappings
					["<C-h>"] = fb_actions.goto_home_dir,
					["."] = fb_actions.toggle_hidden,
					["dd"] = fb_actions.remove,
					["re"] = fb_actions.rename,
					["yy"] = fb_actions.copy,
					["c"] = fb_actions.create,
					["p"] = fb_actions.move,
					["o"] = fb_actions.open,
				},
			},
		},
		fzf = {
			fuzzy = true, -- false will only do exact matching
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
			-- the default case_mode is "smart_case"
		},
		bookmarks = {
			selected_browser = "brave",
			url_open_command = "open",
			url_open_plugin = "open_browser",
			full_path = true,
			firefox_profile_name = nil,
		},
	},
})
require("telescope").load_extension("fzy_native")
-- telescope.load_extension("fzf")
-- telescope.load_extension("neoclip")
telescope.load_extension("bookmarks")
telescope.load_extension("file_browser")

-- https://github.com/nvim-treesitter/nvim-treesitter/wiki/Windows-support#troubleshooting
require 'nvim-treesitter.install'.compilers = { "gcc" }
vim.opt.runtimepath:append("$HOME/.local/share/treesitter")

require("nvim-treesitter.configs").setup({
    -- If you need to change the installation directory of the parsers (see -> Advanced Setup)
    parser_install_dir = "$HOME/.local/share/treesitter",

    -- List of parsers to ignore installing (for "all")
	ensure_installed = { enable = "all" },

    -- ignore_install = { "javascript" },
	tree_docs = { enable = true },
	highlight = {
		enable = true,
		-- disable = {'markdown'},
		-- disable = { "nix" },
	},
	context_commentstring = {
		enable = true,
		enable_autocmd = false,
	},
	indent = {
	       enable = true,
	   },
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "<c-space>",
			node_incremental = "<c-space>",
			scope_incremental = "<c-s>",
			node_decremental = "<c-backspace>",
		},
	},

	rainbow = {
		enable = true,
		disable = { "markdown" },
		extended_mode = true,
		max_file_lines = nil,
	},

	playground = {
		enable = true,
		disable = { "markdwon" },
		updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
		persist_queries = false, -- Whether the query persists across vim sessions
		keybindings = {
			toggle_query_editor = "o",
			toggle_hl_groups = "i",
			toggle_injected_languages = "t",
			toggle_anonymous_nodes = "a",
			toggle_language_display = "I",
			focus_language = "f",
			unfocus_language = "F",
			update = "R",
			goto_node = "<cr>",
			show_help = "?",
		},
	},

	textobjects = {
		select = {
			enable = true,
			lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",
			},
		},
		move = {
			enable = true,
			set_jumps = true, -- whether to set jumps in the jumplist
			goto_next_start = {
				["]m"] = "@function.outer",
				["]]"] = "@class.outer",
			},
			goto_next_end = {
				["]M"] = "@function.outer",
				["]["] = "@class.outer",
			},
			goto_previous_start = {
				["[m"] = "@function.outer",
				["[["] = "@class.outer",
			},
			goto_previous_end = {
				["[M"] = "@function.outer",
				["[]"] = "@class.outer",
			},
		},
		swap = {
			enable = true,
			swap_next = {
				["<leader>a"] = "@parameter.inner",
			},
			swap_previous = {
				["<leader>A"] = "@parameter.inner",
			},
		},
	},
})

