-- vim-startify configuration
-- Converted from after/ftplugin/startup.vim

local M = {}

function M.setup()
  -- Session directory
  vim.g.startify_session_dir = '$HOME/tmp/nvim_sessions'

  -- Lists to show on startify screen
  vim.g.startify_lists = {
    { type = 'files',     header = { '   MRU Files' } },
    { type = 'dir',       header = { '   Current Directory ' .. vim.fn.getcwd() } },
    { type = 'sessions',  header = { '   Sessions' } },
    { type = 'bookmarks', header = { '   Bookmarks' } },
    { type = 'commands',  header = { '   Commands' } },
  }

  -- Bookmarks
  vim.g.startify_bookmarks = {
    { d = '~/dotfiles' },
    { p = '~/.config/nvim/lua/morpheus/plugins.lua' },
    { v = '$MYVIMRC' },
    { z = '~/dotfiles/zsh/.zshrc' },
    { e = '~/dotfiles/emacs/.emacs.d/init.el' },
    { ts = '~/.config/nvim/vimfiles/telescope.vim' },
    { km = '~/.config/nvim/lua/morpheus/keymaps.lua' },
  }

  -- Custom commands
  vim.g.startify_commands = {
    { o = { 'oldfiles', 'Telescope oldfiles' } },
    { t = { 'terminal', 'FloatermNew' } },
    { f = { 'ranger', 'Ranger' } },
    { g = { 'Git', 'Neogit' } },
  }

  -- Let Startify take care of buffers
  vim.g.startify_session_delete_buffers = 1

  -- Similar to Vim-rooter
  vim.g.startify_change_to_vcs_root = 1

  -- Unicode fortune
  vim.g.startify_fortune_use_unicode = 1

  -- Automatically update sessions
  vim.g.startify_session_persistence = 1

  -- Get rid of empty buffer and quit
  vim.g.startify_enable_special = 1
end

return M
