local m = require("morpheus/mapping_utils")

local opts = { noremap = true, silent = true }
local term_opts = { silent = true }
local keymap = vim.api.nvim_set_keymap

-- Markdown Folding Functions using vim-markdown plugin (like linkarzu's config)
-- This uses the proven vim-markdown plugin for proper VSCode-like folding

-- Set up markdown folding using vim-markdown plugin
local function setup_markdown_folding()
  if vim.bo.filetype ~= "markdown" then
    return false
  end
  
  -- Use vim-markdown's syntax-based folding (like linkarzu's config)
  vim.opt_local.foldmethod = "syntax"
  vim.opt_local.foldlevel = 99 -- Start with all folds open like VSCode
  vim.opt_local.foldenable = true
  
  return true
end

function markdown_fold_all()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  
  -- Set fold level to 1 to show only top-level headers
  vim.opt_local.foldlevel = 1
  
  vim.notify("Folded all sections to top level (VSCode style)", vim.log.levels.INFO)
end

function markdown_fold_all_except_selected()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  
  -- Get current cursor position
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  
  -- Find the current section header level
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  
  -- Set fold level to show only the current section and its subsections
  vim.opt_local.foldlevel = current_section_level
  
  vim.notify("Folded all sections except current (VSCode style)", vim.log.levels.INFO)
end

function markdown_unfold_all()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  
  -- Unfold everything (like VSCode)
  vim.opt_local.foldlevel = 99
  
  vim.notify("Unfolded all sections (VSCode style)", vim.log.levels.INFO)
end

function markdown_fold_current_section()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  
  -- Get current cursor position
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  
  -- Find the current section header level
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  
  -- Set fold level to hide the current section
  vim.opt_local.foldlevel = current_section_level - 1
  
  vim.notify("Folded current section (VSCode style)", vim.log.levels.INFO)
end

function markdown_unfold_current_section()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  
  -- Get current cursor position
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  
  -- Find the current section header level
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  
  -- Set fold level to show the current section and its subsections
  vim.opt_local.foldlevel = current_section_level + 1
  
  vim.notify("Unfolded current section (VSCode style)", vim.log.levels.INFO)
end

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

-- m.nmap("<leader>pp", ":lua require('nabla').popup()<CR>")
-- m.vmap("<leader>pp", ":lua require('nabla').popup()<CR>")

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

-- paste with <S-Insert>
keymap("n", "<S-Insert>", "p", opts)
keymap("i", "<S-Insert>", "<ESC>p", opts)

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
m.nmap("<leader>ft", "<cmd> TodoTelescope<CR>")
m.nmap("<leader>fm", "<cmd>Telescope bookmarks<cr>")
m.nmap("<leader>fc", "<cmd>Telescope neoclip<cr>")
map_tele("<space>fp", "installed_plugins")
map_tele("<space>do", "search_dotfiles")
map_tele("<space>vr", "search_vimrc")
map_tele("<space>bs", "anime_selector")
-- m.nmap("<c-f>", "<cmd>Telescope find_files hidden=true<CR>")

-- Git
-- m.nmap("<leader>gg", ":Neogit <CR>")
-- m.nmap("<leader>gd", ":DiffviewOpen<CR>")

-- Git conflict resolution keymaps (from blog post)
m.nmap("<leader>gs", ":Git<CR>", { desc = "[G]it [S]tatus" })
m.nmap("<leader>gd", ":Gdiffsplit<CR>", { desc = "[G]it [D]iff split" })
m.nmap("<leader>gc", ":Git commit<CR>", { desc = "[G]it [C]ommit" })
m.nmap("<leader>gb", ":Git blame<CR>", { desc = "[G]it [B]lame" })
m.nmap("<leader>gm", ":Git mergetool<CR>", { desc = "[G]it [M]ergetool" })

-- Quick conflict resolution
m.nmap("<leader>gj", ":diffget //3<CR>", { desc = "Get changes from [R]ight (REMOTE)" })
m.nmap("<leader>gf", ":diffget //2<CR>", { desc = "Get changes from [L]eft (LOCAL)" })

-- Improve diff experience
vim.opt.diffopt:append('algorithm:patience')
vim.opt.diffopt:append('indent-heuristic')

-- Custom functions for conflict resolution
vim.cmd([[
function! ConflictStats()
    let l:conflict_pattern = '^<<<<<<< '
    let l:conflicts = search(l:conflict_pattern, 'n')
    echo "Remaining conflicts: " . l:conflicts
endfunction
]])

m.nmap("<leader>gcs", ":call ConflictStats()<CR>", { desc = "[G]it [C]onflict [S]tats" })

m.nmap("<leader>cls", "<cmd>SymbolsOutline<cr>")

m.nmap("<leader>sv", "<cmd>lua ReloadConfig()<cr>")
vim.cmd("command! ReloadConfig lua ReloadConfig()")


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
--

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

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
-- pane switching
keymap("n", "<c-j>", "<c-w>j")
keymap("n", "<c-k>", "<c-w>k")
keymap("n", "<c-h>", "<c-w>h")
keymap("n", "<c-l>", "<c-w>l")
keymap("c", "<c-j>", "<Down>")
keymap("c", "<c-k>", "<Up>")

-- "Edit configs
keymap("n", "<leader>vc", "<cmd>e $MYVIMRC<cr>")
keymap("n", "<leader>tc", ":edit $HOME/dotfiles/tmux/.tmux.conf<cr>")
keymap("n", "<leader>zc", ":edit $HOME/dotfiles/zsh/.zshrc<cr>")

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

-- keymap("n", "<leader>fb", "<cmd> Telescope file_browser<CR>", { desc = "[/fb] file browser search]" })

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
local autocmd = vim.api.nvim_create_autocmd
autocmd({ "BufRead", "BufWinEnter" }, {
	group = git_group,
	pattern = "NEOGIT_COMMIT_EDIT_EDITMSG",
	command = "startinsert | 1",
})

autocmd({ "BufRead", "BufWinEnter" }, {
	group = git_group,
	pattern = "COMMIT_EDITMSG",
	command = "startinsert | 1",
})

autocmd("FileType", {
	group = git_group,
	pattern = { "gitcommit", "gitrebase" },
	command = "startinsert | 1",
})

-- require("git-conflict").setup({
-- 	default_mappings = true,
-- 	disable_diagnostics = false,
-- 	highlights = {
-- 		incoming = "DiffText",
-- 		current = "DiffAdd",
-- 	},
-- })

-- require("neogit").setup({
-- 	disable_commit_confirmation = true,
-- 	disable_insert_on_commit = false,
-- 	integrations = {
-- 		diffview = true,
-- 	},
-- })
keymap("n", "<leader>gg", "<cmd>lua require('neogit').open({ kind = 'split' })<cr>")
keymap("n", "<leader>gd", "<cmd> DiffviewOpen<CR>")
keymap("n", "<leader>cls", "<cmd>SymbolsOutline<cr>")

keymap("n", "<leader>sv", "<cmd>lua ReloadConfig()<cr>")
keymap("n", "<leader>sv", "<cmd>lua ReloadConfig()<cr>")

vim.api.nvim_create_user_command("CopyBufferName", function()
    vim.cmd("echo expand('%:p')")
    vim.cmd("let @+ = expand('%:p')")
    vim.cmd('echo "Full path of " . expand(\'%:t\') . " was copied to system clipboard"')
end, {})
vim.cmd("command! CopyBufferName lua CopyBufferName()")
keymap("n", "<leader>bn", "<cmd>CopyBufferName<cr>")

m.nmap("<leader>sa", "<cmd>Scratch<cr>")

keymap("n", "gx", "<Plug>(openbrowser-smart-search)<cr>")
keymap("v", "gx", "<Plug>(openbrowser-smart-search)<cr>")

-- Remove all trailing whitespace by pressing F5
keymap("n", "<F12>", ":let _s=@/<Bar>:%s/s+$//e<Bar>:let @/=_s<Bar><CR>")

-- writing
keymap("n", "<leader>wd", function()
	vim.cmd(string.format(":85vnew ~/Org/zettelkasten/notes/note-%s.md", os.date("%Y-%m-%d")))
end, { desc = "[/de] notes]", noremap = true })

-- Markdown Folding Keybindings
vim.keymap.set("n", "<leader>mf", function() markdown_fold_all() end, { desc = "[M]arkdown [F]old All" })
vim.keymap.set("n", "<leader>ms", function() markdown_fold_all_except_selected() end, { desc = "[M]arkdown fold all except [S]elected" })
vim.keymap.set("n", "<leader>mu", function() markdown_unfold_all() end, { desc = "[M]arkdown [U]nfold All" })
vim.keymap.set("n", "<leader>mc", function() markdown_fold_current_section() end, { desc = "[M]arkdown fold [C]urrent section" })
vim.keymap.set("n", "<leader>mo", function() markdown_unfold_current_section() end, { desc = "[M]arkdown unfold current secti[O]n" })

-- Note: Markdown folding is now handled in lua/morpheus/treesitter.lua
-- The vim-markdown plugin provides syntax-based folding for markdown files

-- Custom gf function to handle quoted file paths
local function open_quoted_file()
  local line = vim.api.nvim_get_current_line()
  local col = vim.api.nvim_win_get_cursor(0)[2] + 1 -- Convert to 1-based column
  
  -- Look for quoted strings that might be file paths
  local patterns = {
    -- Double quotes
    '"[^"]*"',
    -- Single quotes  
    "'[^']*'",
    -- Backticks
    '`[^`]*`'
  }
  
  local best_match = nil
  local best_start = 0
  local best_end = 0
  
  for _, pattern in ipairs(patterns) do
    local start = 1
    while true do
      local match_start, match_end = line:find(pattern, start)
      if not match_start then break end
      
      -- Check if cursor is within this match
      if col >= match_start and col <= match_end then
        best_match = line:sub(match_start + 1, match_end - 1) -- Remove quotes
        best_start = match_start
        best_end = match_end
        break
      end
      start = match_end + 1
    end
    if best_match then break end
  end
  
  if best_match then
    -- Try to open the file
    local success = pcall(vim.cmd, "edit " .. vim.fn.fnameescape(best_match))
    if not success then
      vim.notify("Could not open file: " .. best_match, vim.log.levels.ERROR)
    end
  else
    -- Fall back to default gf behavior
    vim.cmd("normal! gf")
  end
end

-- Override gf keybinding to handle quoted file paths
vim.keymap.set("n", "gf", open_quoted_file, { desc = "Open file under cursor (handles quoted paths)" })

-- Forward search from neovim to zathura
function SyncTexForward()
  local linenumber = vim.fn.line(".")
  local colnumber = vim.fn.col(".")
  local filename = vim.fn.expand("%:p")
  local filenamePDF = filename:gsub("%.tex$", ".pdf")
  local execstr = "!zathura --synctex-forward " .. linenumber .. ":" .. colnumber .. ":" .. filename .. " " .. filenamePDF .. " &>/dev/null &"
  vim.cmd(execstr)
end

vim.keymap.set("n", "<leader>lv", SyncTexForward, { desc = "Forward search to zathura" })

-- remove emojis from the current buffer
local function remove_emojis_from_buffer()
  local buf = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  local emoji_pattern = "[\240-\244][\128-\191][\128-\191][\128-\191]"

  for i, line in ipairs(lines) do
    lines[i] = line:gsub(emoji_pattern, "")
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
end

vim.api.nvim_create_user_command("RemoveEmojis", remove_emojis_from_buffer, {})
-- vim.keymap.set("n", "<leader>er", RemoveEmojis, { desc = "removes emojis from the current buffer" })
