local m = require("morpheus/mapping_utils")

local opts = { noremap = true, silent = true }
local term_opts = { silent = true }
local keymap = vim.api.nvim_set_keymap

vim.keymap.set("n", "<leader>/", function()
	-- You can pass additional configuration to telescope to change theme, layout, etc.
	require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
		winblend = 10,
		previewer = false,
	}))
end, { desc = "[/] Fuzzily search in current buffer]" })

-- move between vim panes
m.nmap("<C-h>", "<C-w>h", opts)
m.nmap("<C-j>", "<C-w>j", opts)
m.nmap("<C-k>", "<C-w>k", opts)
m.nmap("<C-l>", "<C-w>l", opts)

m.nmap("<leader>rf", "<cmd> Ranger<CR>")

-- Resize with arrows
m.nmap("<C-Up>", ":resize -2<CR>", opts)
m.nmap("<C-Down>", ":resize +2<CR>", opts)
m.nmap("<C-Left>", ":vertical resize -2<CR>", opts)
m.nmap("<C-Right>", ":vertical resize +2<CR>", opts)

-- Move text up and down
m.nmap("<A-j>", "<Esc>:m .+1<CR>==gi", opts)
m.nmap("<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Switch between last two buffers
m.nmap("<leader><leader>", "<C-^>", opts)
m.nmap("Q", "<nop>", opts)

-- "Edit configs
m.nmap("<leader>vc", "<cmd>e $MYVIMRC<cr>")
m.nmap("<leader>tc", ":edit $HOME/dotfiles/tmux/.tmux.conf<cr>")
m.nmap("<leader>zc", ":edit $HOME/dotfiles/zsh/.zshrc<cr>")

-- Stay in indent mode
m.vmap("<", "<gv", opts)
m.vmap(">", ">gv", opts)

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
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Harpoon mappings
m.nmap("<leader>hm", [[:lua require("harpoon.mark").add_file()<cr>]])
m.nmap("<leader>hv", [[:lua require("harpoon.ui").toggle_quick_menu()<cr>]])

-- nnoremap <leader><F1> :Startify<CR>
m.nmap("<leader>st", ":Startify<CR>")
m.nmap("<leader>so", ":source %<CR>")

m.nmap("<leader>pp", ":lua require('nabla').popup()<CR>")
m.vmap("<leader>pp", ":lua require('nabla').popup()<CR>")

m.nmap("K", "<Cmd>lua vim.lsp.buf.hover()<CR>")
m.nmap("<leader>w", "<Cmd>w<CR>")
m.nmap("c,", "<cmd>cprev<cr>")
m.nmap("c.", "<cmd>cnext<cr>")

-- buffer switching
m.nmap("<leader>,", "<cmd>bprev<cr>")
m.nmap("<leader>.", "<cmd>bnext<cr>")
-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)
m.nmap("<leader>bf", ":bfirst<CR>")
m.nmap("<leader>bl", ":blast<CR>")

-- pane switching
m.nmap("<c-j>", "<c-w>j")
m.nmap("<c-k>", "<c-w>k")
m.nmap("<c-h>", "<c-w>h")
m.nmap("<c-l>", "<c-w>l")
m.cmap("<c-j>", "<Down>")
m.cmap("<c-k>", "<Up>")

-- tab switching
m.nmap("<leader>tn", ":tabnew<CR>")
m.nmap("<leader>tk", ":tabnext<CR>")
m.nmap("<leader>tj", ":tabprev<CR>")

m.nmap("di$", "T$dt$")
m.nmap("ci$", "T$ct$")
m.nmap("<leader>hn", "<cmd>:setlocal nonumber norelativenumber<CR>")
m.nmap("<leader>hN", "<cmd>:setlocal number relativenumber<CR>")
m.nmap("-", "<C-W><")
m.nmap("_", "<C-W>>")
m.nmap("=", "<C-W>-")
m.nmap("+", "<C-W>+")

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

m.nmap("<leader>sf", "/\\c")
m.nmap("<leader>sb", "?\\c")
m.nmap("<leader>nh", "<cmd>noh<CR>")

if not pcall(require, "telescope") then
	return
end

local sorters = require("telescope.sorters")

TelescopeMapArgs = TelescopeMapArgs or {}

local map_tele = function(key, f, options, buffer)
	local map_key = vim.api.nvim_replace_termcodes(key .. f, true, true, true)

	TelescopeMapArgs[map_key] = options or {}

	local mode = "n"
	local rhs = string.format("<cmd>lua R('morpheus.telescope')['%s'](TelescopeMapArgs['%s'])<CR>", f, map_key)

	local map_options = {
		noremap = true,
		silent = true,
	}

	if not buffer then
		vim.api.nvim_set_keymap(mode, key, rhs, map_options)
	else
		vim.api.nvim_buf_set_keymap(0, mode, key, rhs, map_options)
	end
end

-- Telescope keymaps
m.nmap("<leader>bb", "<cmd> Telescope buffers<CR>")
m.nmap("<leader>fb", "<cmd> Telescope file_browser<CR>")
m.nmap("<leader>fo", "<cmd> Telescope oldfiles<CR>")
m.nmap("<leader>ff", "<cmd> Telescope find_files<CR>")
m.nmap("<leader>fk", "<cmd> Telescope keymaps<CR>")
m.nmap("<leader>fs", "<cmd> lua require('telescope.builtin').grep_string({ search = vim.fn.input('Grep For > ')})<CR>")
m.nmap("<leader>fh", "<cmd> Telescope help_tags<CR>")
m.nmap("<leader>fg", "<cmd> Telescope live_grep<CR>")
m.nmap("<leader>lr", "<cmd> Telescope lsp_references<CR>")
map_tele("<space>fp", "installed_plugins")
map_tele("<space>do", "search_dotfiles")
map_tele("<space>vr", "search_vimrc")
m.nmap("<leader>ft", "<cmd> TodoTelescope<CR>")

-- Extension mappings
m.nmap("<leader>fm", "<cmd>Telescope bookmarks<cr>")
m.nmap("<leader>fc", "<cmd>Telescope neoclip<cr>")
-- m.nmap("<c-f>", "<cmd>Telescope find_files hidden=true<CR>")

-- Git
m.nmap("<leader>gg", ":Neogit <CR>")
m.nmap("<leader>gd", ":DiffviewOpen<CR>")

m.nmap("<leader>cls", "<cmd>SymbolsOutline<cr>")

m.nmap("<leader>sv", "<cmd>lua ReloadConfig()<cr>")
vim.cmd("command! ReloadConfig lua ReloadConfig()")

vim.cmd("command! CopyBufferName lua CopyBufferName()")
m.nmap("<leader>bn", "<cmd>lua CopyBufferName()<cr>")
m.nmap("<leader>sa", "<cmd>Scratch<cr>")

m.nmap("gx", "<Plug>(openbrowser-smart-search)<cr>")
m.vmap("gx", "<Plug>(openbrowser-smart-search)<cr>")

-- writing
m.nmap("<leader>wd", '<cmd>lua R("morpheus.wiki").make_diary_entry()<CR>', { noremap = true })
m.nmap("<leader>wt", '<cmd>lua R("morpheus.wiki").make_todo()<CR>', { noremap = true })
m.nmap("<leader>oc", '<cmd>lua require("orgmode").action("capture.prompt")<CR>', { noremap = true })
m.nmap("<leader>oa", '<cmd>lua require("orgmode").action("agenda.prompt")<CR>', { noremap = true })

-- debugging remaps
m.nmap("<leader>ddd", '<cmd>lua require("osv").launch()<cr>')
m.nmap("<leader>ddr", '<cmd>lua require("osv").run_this()<cr>')
m.nmap("<leader>db", '<cmd>lua require("dap").toggle_breakpoint()<cr>')
m.nmap("<leader>dj", "<cmd>lua require'dap'.step_over()<cr>")
m.nmap("<leader>dl", "<cmd>lua require'dap'.step_into()<cr>")
m.nmap("<leader>dk", "<cmd>lua require'dap'.step_out()<cr>")
m.nmap("<leader>dc", "<cmd>lua require'dap'.continue()<cr>")
m.nmap("<leader>dr", "<cmd>lua require'dap'.repl.open()<cr>")
keymap("n", "<leader>dB", ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>", opts)
keymap("n", "<leader>lp", ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>", opts)
m.nmap("<leader>dw", "<cmd>lua require('diaglist').open_all_diagnostics()<cr>")
m.nmap("<leader>d0", "<cmd>lua require('diaglist').open_buffer_diagnostics()<cr>")
m.nmap("<leader>m", "<cmd> MaximizerToggle!<CR>")

m.nmap("<leader>dn", "<cmd> lua require('dap-python').test_method()<CR>", opts)
m.nmap("<leader>df", "<cmd> lua require('dap-python').test_class()<CR>", opts)
m.vmap("<leader>ds", "<ESC> <cmd> lua require('dap-python').debug_selection()<CR>", opts)

-- vim.keymap.set("n", "<F5>", ":lua require'dap'.continue()<CR>")
-- vim.keymap.set("n", "<F3>", ":lua require'dap'.step_over()<CR>")
-- vim.keymap.set("n", "<F2>", ":lua require'dap'.step_into()<CR>")
-- vim.keymap.set("n", "<F12>", ":lua require'dap'.step_out()<CR>")
-- vim.keymap.set("n", "<leader>br", ":lua require'dap'.toggle_breakpoint()<CR>")
-- vim.keymap.set("n", "<leader>B", ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
-- vim.keymap.set("n", "<leader>lp", ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>")
-- vim.keymap.set("n", "<leader>dr", ":lua require'dap'.repl.open()<CR>")
