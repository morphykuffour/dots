-- Keymaps configuration
-- Consolidated to use vim.keymap.set consistently

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Markdown Folding Functions using vim-markdown plugin
local function setup_markdown_folding()
  if vim.bo.filetype ~= "markdown" then
    return false
  end
  vim.opt_local.foldmethod = "syntax"
  vim.opt_local.foldlevel = 99
  vim.opt_local.foldenable = true
  return true
end

local function markdown_fold_all()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  vim.opt_local.foldlevel = 1
  vim.notify("Folded all sections to top level", vim.log.levels.INFO)
end

local function markdown_fold_all_except_selected()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  vim.opt_local.foldlevel = current_section_level
  vim.notify("Folded all sections except current", vim.log.levels.INFO)
end

local function markdown_unfold_all()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  vim.opt_local.foldlevel = 99
  vim.notify("Unfolded all sections", vim.log.levels.INFO)
end

local function markdown_fold_current_section()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  vim.opt_local.foldlevel = current_section_level - 1
  vim.notify("Folded current section", vim.log.levels.INFO)
end

local function markdown_unfold_current_section()
  if not setup_markdown_folding() then
    vim.notify("This function only works in markdown files", vim.log.levels.WARN)
    return
  end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local current_section_level = 0
  for i = cursor_line, 1, -1 do
    local line = lines[i]
    if line and line:match("^#+ ") then
      current_section_level = #line:match("^(#+)")
      break
    end
  end
  vim.opt_local.foldlevel = current_section_level + 1
  vim.notify("Unfolded current section", vim.log.levels.INFO)
end

-- Markdown folding keymaps
keymap("n", "<leader>mf", markdown_fold_all, { desc = "[M]arkdown [F]old All" })
keymap("n", "<leader>ms", markdown_fold_all_except_selected, { desc = "[M]arkdown fold all except [S]elected" })
keymap("n", "<leader>mu", markdown_unfold_all, { desc = "[M]arkdown [U]nfold All" })
keymap("n", "<leader>mc", markdown_fold_current_section, { desc = "[M]arkdown fold [C]urrent section" })
keymap("n", "<leader>mo", markdown_unfold_current_section, { desc = "[M]arkdown unfold current secti[O]n" })

-- Window/pane navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", { silent = true })
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", { silent = true })
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", { silent = true })
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", { silent = true })

-- Command line navigation
keymap("c", "<C-j>", "<Down>", { noremap = true })
keymap("c", "<C-k>", "<Up>", { noremap = true })

-- Resize windows with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Alternative resize with +/-
keymap("n", "-", "<C-W><", opts)
keymap("n", "_", "<C-W>>", opts)
keymap("n", "=", "<C-W>-", opts)
keymap("n", "+", "<C-W>+", opts)

-- Move text up and down (normal mode)
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Move text up and down (visual mode)
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Move text up and down (visual block mode)
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Buffer navigation
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)
keymap("n", "<leader>,", "<cmd>bprev<cr>", opts)
keymap("n", "<leader>.", "<cmd>bnext<cr>", opts)
keymap("n", "<leader>bf", ":bfirst<CR>", opts)
keymap("n", "<leader>bl", ":blast<CR>", opts)
keymap("n", "<leader><leader>", "<C-^>", opts)

-- Quickfix navigation
keymap("n", "c,", "<cmd>cprev<cr>", opts)
keymap("n", "c.", "<cmd>cnext<cr>", opts)

-- Tab navigation
keymap("n", "<leader>tn", ":tabnew<CR>", opts)
keymap("n", "<leader>tk", ":tabnext<CR>", opts)
keymap("n", "<leader>tj", ":tabprev<CR>", opts)

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Disable Q (ex mode)
keymap("n", "Q", "<nop>", opts)

-- Paste with Shift+Insert
keymap("n", "<S-Insert>", "p", opts)
keymap("i", "<S-Insert>", "<ESC>p", opts)

-- Edit config files
keymap("n", "<leader>vc", "<cmd>e $MYVIMRC<cr>", { desc = "Edit init.lua" })
keymap("n", "<leader>tc", ":edit $HOME/dotfiles/tmux/.tmux.conf<cr>", { desc = "Edit tmux config" })
keymap("n", "<leader>zc", ":edit $HOME/dotfiles/zsh/.zshrc<cr>", { desc = "Edit zshrc" })

-- Useful shortcuts
keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
keymap("n", "<leader>w", "<Cmd>w<CR>", { desc = "Save file" })
keymap("n", "<leader>nh", "<cmd>noh<CR>", { desc = "Clear search highlight" })
keymap("n", "<leader>hn", "<cmd>:setlocal nonumber norelativenumber<CR>", { desc = "Hide line numbers" })
keymap("n", "<leader>hN", "<cmd>:setlocal number relativenumber<CR>", { desc = "Show line numbers" })

-- Search shortcuts
keymap("n", "<leader>sf", "/\\c", { desc = "Search forward (case insensitive)" })
keymap("n", "<leader>sb", "?\\c", { desc = "Search backward (case insensitive)" })

-- Custom text objects
keymap("n", "di$", "T$dt$", opts)
keymap("n", "ci$", "T$ct$", opts)

-- Word wrap navigation
keymap("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
keymap("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Startify
keymap("n", "<leader>st", ":Startify<CR>", { desc = "Open Startify" })
keymap("n", "<leader>so", ":source %<CR>", { desc = "Source current file" })

-- Telescope keymaps (only if telescope is available)
local telescope_ok, _ = pcall(require, "telescope")
if telescope_ok then
  local builtin = require("telescope.builtin")
  local themes = require("telescope.themes")

  keymap("n", "<leader>/", function()
    builtin.current_buffer_fuzzy_find(themes.get_dropdown({
      winblend = 10,
      previewer = false,
    }))
  end, { desc = "[/] Fuzzily search in current buffer" })

  keymap("n", "<leader>bb", function()
    builtin.buffers(themes.get_ivy({ winblend = 5, previewer = false }))
  end, { desc = "Search buffers" })

  keymap("n", "<leader>fo", function()
    builtin.oldfiles(themes.get_ivy({ winblend = 5, previewer = false }))
  end, { desc = "Search old files" })

  keymap("n", "<leader>ff", function()
    builtin.find_files(themes.get_ivy({ winblend = 5, previewer = false }))
  end, { desc = "Find files" })

  keymap("n", "<leader>fg", "<cmd>Telescope live_grep<CR>", { desc = "Live grep" })
  keymap("n", "<leader>fh", "<cmd>Telescope help_tags<CR>", { desc = "Help tags" })
  keymap("n", "<leader>fk", "<cmd>Telescope keymaps<CR>", { desc = "Keymaps" })
  keymap("n", "<leader>ft", "<cmd>TodoTelescope<CR>", { desc = "Search TODOs" })
  keymap("n", "<leader>lr", "<cmd>Telescope lsp_references<CR>", { desc = "LSP references" })
  keymap("n", "<leader>gf", builtin.git_files, { desc = "Search git files" })
  keymap("n", "<leader>sd", builtin.diagnostics, { desc = "Search diagnostics" })
  keymap("n", "<leader>sr", builtin.resume, { desc = "Search resume" })
  keymap("n", "<C-f>", "<cmd>Telescope find_files hidden=true<CR>", { desc = "Find files (hidden)" })

  keymap("n", "<leader>fs", function()
    builtin.grep_string({ search = vim.fn.input("Grep For > ") })
  end, { desc = "Grep string" })

  keymap("n", "<space>do", function()
    builtin.find_files({
      prompt_title = "< dotfiles >",
      cwd = vim.env.DOTFILES or "~/dotfiles",
      hidden = true,
    })
  end, { desc = "Search dotfiles" })

  keymap("n", "<A-x>", function()
    builtin.keymaps(themes.get_ivy({ winblend = 5, previewer = false }))
  end, { desc = "Execute keymaps" })

  keymap("n", "<space>fp", function()
    builtin.find_files({
      prompt_title = "< installed plugins >",
      cwd = vim.fn.stdpath("data") .. "/lazy",
    })
  end, { desc = "Search installed plugins" })

  -- Git with delta previewer
  local previewers = require("telescope.previewers")

  local delta_bcommits = previewers.new_termopen_previewer({
    get_command = function(entry)
      return {
        "git", "-c", "core.pager=delta", "-c", "delta.side-by-side=false",
        "diff", entry.value .. "^!", "--", entry.current_file,
      }
    end,
  })

  local delta = previewers.new_termopen_previewer({
    get_command = function(entry)
      return { "git", "-c", "core.pager=delta", "-c", "delta.side-by-side=false", "diff", entry.value .. "^!" }
    end,
  })

  keymap("n", "<leader>dgc", function()
    builtin.git_commits({
      previewer = { delta, previewers.git_commit_message.new({}), previewers.git_commit_diff_as_was.new({}) },
    })
  end, { desc = "Git commits with delta" })

  keymap("n", "<leader>dgb", function()
    builtin.git_bcommits({
      previewer = { delta_bcommits, previewers.git_commit_message.new({}), previewers.git_commit_diff_as_was.new({}) },
    })
  end, { desc = "Git buffer commits with delta" })
end

-- Git (vim-fugitive)
keymap("n", "<leader>gs", ":Git<CR>", { desc = "Git status" })
keymap("n", "<leader>gd", ":Gdiffsplit<CR>", { desc = "Git diff split" })
keymap("n", "<leader>gc", ":Git commit<CR>", { desc = "Git commit" })
keymap("n", "<leader>gb", ":Git blame<CR>", { desc = "Git blame" })
keymap("n", "<leader>gm", ":Git mergetool<CR>", { desc = "Git mergetool" })
keymap("n", "<leader>gj", ":diffget //3<CR>", { desc = "Get changes from right (REMOTE)" })
keymap("n", "<leader>gk", ":diffget //2<CR>", { desc = "Get changes from left (LOCAL)" })

-- Neogit
keymap("n", "<leader>gg", function()
  local ok, neogit = pcall(require, 'neogit')
  if ok then neogit.open({ kind = 'split' }) end
end, { desc = "Open Neogit" })

-- DiffviewOpen
keymap("n", "<leader>dv", "<cmd>DiffviewOpen<CR>", { desc = "Open Diffview" })

-- Improve diff experience
vim.opt.diffopt:append('algorithm:patience')
vim.opt.diffopt:append('indent-heuristic')

-- DAP (Debug Adapter Protocol) keymaps
local dap_ok, dap = pcall(require, 'dap')
if dap_ok then
  keymap("n", "<leader>db", function() dap.toggle_breakpoint() end, { desc = "Toggle breakpoint" })
  keymap("n", "<leader>dB", function() dap.set_breakpoint(vim.fn.input('Breakpoint condition: ')) end, { desc = "Conditional breakpoint" })
  keymap("n", "<leader>dc", function() dap.continue() end, { desc = "Debug continue" })
  keymap("n", "<leader>dj", function() dap.step_over() end, { desc = "Debug step over" })
  keymap("n", "<leader>dl", function() dap.step_into() end, { desc = "Debug step into" })
  keymap("n", "<leader>dk", function() dap.step_out() end, { desc = "Debug step out" })
  keymap("n", "<leader>dr", function() dap.repl.open() end, { desc = "Debug REPL" })
  keymap("n", "<leader>lp", function() dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end, { desc = "Log point" })
end

-- Writing/Notes
keymap("n", "<leader>wd", function()
  vim.cmd(string.format(":85vnew ~/Org/zettelkasten/notes/note-%s.md", os.date("%Y-%m-%d")))
end, { desc = "Create diary entry" })

keymap("n", "<leader>wt", function()
  local ok, wiki = pcall(require, 'morpheus.wiki')
  if ok then wiki.make_todo() end
end, { desc = "Open todo" })

-- Config reload
_G.ReloadConfig = function()
  local hls_status = vim.v.hlsearch
  for name, _ in pairs(package.loaded) do
    if name:match("^morpheus") then
      package.loaded[name] = nil
    end
  end
  dofile(vim.env.MYVIMRC)
  if hls_status == 0 then
    vim.opt.hlsearch = false
  end
end

keymap("n", "<leader>sv", "<cmd>lua ReloadConfig()<cr>", { desc = "Reload config" })
vim.api.nvim_create_user_command("ReloadConfig", ReloadConfig, {})

-- Copy buffer name to clipboard
vim.api.nvim_create_user_command("CopyBufferName", function()
  local path = vim.fn.expand('%:p')
  vim.fn.setreg('+', path)
  vim.notify("Copied: " .. path, vim.log.levels.INFO)
end, {})
keymap("n", "<leader>bn", "<cmd>CopyBufferName<cr>", { desc = "Copy buffer path" })

-- Remove trailing whitespace
keymap("n", "<F12>", function()
  local save = vim.fn.winsaveview()
  vim.cmd([[%s/\s\+$//e]])
  vim.fn.winrestview(save)
end, { desc = "Remove trailing whitespace" })

-- Custom gf for quoted file paths
local function open_quoted_file()
  local line = vim.api.nvim_get_current_line()
  local col = vim.api.nvim_win_get_cursor(0)[2] + 1
  local patterns = { '"[^"]*"', "'[^']*'", '`[^`]*`' }
  local best_match = nil

  for _, pattern in ipairs(patterns) do
    local start = 1
    while true do
      local match_start, match_end = line:find(pattern, start)
      if not match_start then break end
      if col >= match_start and col <= match_end then
        best_match = line:sub(match_start + 1, match_end - 1)
        break
      end
      start = match_end + 1
    end
    if best_match then break end
  end

  if best_match then
    local success = pcall(vim.cmd, "edit " .. vim.fn.fnameescape(best_match))
    if not success then
      vim.notify("Could not open file: " .. best_match, vim.log.levels.ERROR)
    end
  else
    vim.cmd("normal! gf")
  end
end

keymap("n", "gf", open_quoted_file, { desc = "Open file under cursor (handles quoted paths)" })

-- Forward search from neovim to zathura (LaTeX)
local function sync_tex_forward()
  local linenumber = vim.fn.line(".")
  local colnumber = vim.fn.col(".")
  local filename = vim.fn.expand("%:p")
  local filenamePDF = filename:gsub("%.tex$", ".pdf")
  vim.fn.system("zathura --synctex-forward " .. linenumber .. ":" .. colnumber .. ":" .. filename .. " " .. filenamePDF .. " &")
end

keymap("n", "<leader>lv", sync_tex_forward, { desc = "Forward search to zathura" })

-- Remove emojis from buffer
vim.api.nvim_create_user_command("RemoveEmojis", function()
  local buf = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local emoji_pattern = "[\240-\244][\128-\191][\128-\191][\128-\191]"
  for i, line in ipairs(lines) do
    lines[i] = line:gsub(emoji_pattern, "")
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
end, {})

-- Remove trailing spaces
vim.api.nvim_create_user_command("RemoveTrailingSpaces", function()
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  for i, line in ipairs(lines) do
    lines[i] = line:gsub("%s+$", "")
  end
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
end, {})

-- Git commit autocommands
local git_group = vim.api.nvim_create_augroup("git_group", { clear = true })

vim.api.nvim_create_autocmd({ "BufRead", "BufWinEnter" }, {
  group = git_group,
  pattern = { "NEOGIT_COMMIT_EDITMSG", "COMMIT_EDITMSG" },
  command = "startinsert | 1",
})

vim.api.nvim_create_autocmd("FileType", {
  group = git_group,
  pattern = { "gitcommit", "gitrebase" },
  command = "startinsert | 1",
})
