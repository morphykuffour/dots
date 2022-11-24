" --- For vim-startify --------------------------------------------------"

" Sessions
" :SLoad       load a session
" :SSave[!]    save a session
" :SDelete[!]  delete a session
" :SClose      close a session
"
let g:startify_session_dir = '$HOME/tmp/nvim_sessions'
"
let g:startify_lists = [
          \ { 'type': 'files',     'header': ['   MRU Files']            },
          \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
          \ { 'type': 'sessions',  'header': ['   Sessions']       },
          \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
          \ { 'type': 'commands', 'header': ['   Commands']      },
          \ ]
let g:startify_bookmarks = [ { 'd': '~/dotfiles' }, {'p': '~/.config/nvim/lua/morpheus/plugins.lua'}, {'v': '$MYVIMRC'}, {'z': '~/dotfiles/zsh/.zshrc'}, {'e': '~/dotfiles/emacs/.emacs.d/init.el'}, {'ts': '~/.config/nvim/vimfiles/telescope.vim'}, {'km': '~/.config/nvim/lua/morpheus/keymaps.lua'}]

let g:startify_commands = [
      \ {'o': ['oldfiles', 'Telescope oldfiles']},
      \ {'t': ['terminal', 'FloatermNew']},
      \ {'f': ['ranger', 'Ranger']},
      \ {'g': ['Git', 'Neogit']},
      \ ]

" Let Startify take care of buffers
let g:startify_session_delete_buffers = 1

" Similar to Vim-rooter
let g:startify_change_to_vcs_root = 1

" If you want Unicode
let g:startify_fortune_use_unicode = 1

" Automatically Update Sessions
let g:startify_session_persistence = 1

" Get rid of empty buffer and quit
let g:startify_enable_special = 1

