" Highlight text yanked. avaiable on the Nightly build
" yank.vim ?
augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank {
        \ higroup = "Substitute",
        \ timeout = 150,
        \ on_macro = true
        \ }
augroup END

autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

" make the oldfiles list automatically close after I select an entry
augroup oldfiles
    autocmd!
    autocmd FileType qf if w:quickfix_title =~# 'Oldfiles' | nnoremap <buffer> <CR> <CR>:cclose<CR> | endif
augroup END


" Syntastic recommmended settings
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" Help Buffer
command! -nargs=1 -complete=help H :enew | :set buftype=help | :keepalt h <args>


" Status Bar
" set statusline=
" set statusline+=%#DiffAdd#%{(mode()=='n')?'\ \ NORMAL\ ':''}
" set statusline+=%#DiffChange#%{(mode()=='i')?'\ \ INSERT\ ':''}
" set statusline+=%#DiffDelete#%{(mode()=='r')?'\ \ RPLACE\ ':''}
" set statusline+=%#Cursor#%{(mode()=='v')?'\ \ VISUAL\ ':''}
" set statusline+=\ %n\           " buffer number
" set statusline+=%#Visual#       " colour
" set statusline+=%{&paste?'\ PASTE\ ':''}
" set statusline+=%{&spell?'\ SPELL\ ':''}
" set statusline+=%#CursorIM#     " colour
" set statusline+=%R                        " readonly flag
" set statusline+=%M                        " modified [+] flag
" set statusline+=%#Cursor#               " colour
" set statusline+=%#CursorLine#     " colour
" set statusline+=\ %t\                   " short file name
" set statusline+=\ %F\                   " long file name
" set statusline+=%=                          " right align
" set statusline+=%#CursorLine#   " colour
" set statusline+=\ %Y\                   " file type
" set statusline+=%#CursorIM#     " colour
" set statusline+=\ %3l:%-2c\         " line + column
" " set statusline+=%#Cursor#       " colour
" set statusline+=\ %3p%%\                " percentage


" Floaterm
let g:floaterm_keymap_toggle = '<F1>'
let g:floaterm_keymap_next   = '<F2>'
let g:floaterm_keymap_prev   = '<F3>'
let g:floaterm_keymap_new    = '<F4>'

" Floaterm
let g:floaterm_gitcommit='floaterm'
let g:floaterm_autoinsert=1
let g:floaterm_width=0.8
let g:floaterm_height=0.8
let g:floaterm_wintitle=0
let g:floaterm_autoclose=1

" Better grep
if executable("rg")
    let g:rg_derive_root='true'
endif

" Cursor to yellow on insert mode
" Blue on command/other mode
" Note the use of hex codes (ie 3971ED)
if exists('$TMUX')
    let &t_EI = "\<Esc>Ptmux;\<Esc>\033]Pl3971ED\033\\"
    let &t_SI = "\<Esc>Ptmux;\<Esc>\033]PlFBA922\033\\"
    silent !echo -ne "\<Esc>Ptmux;\<Esc>\033]Pl3971ED\033\\"
    autocmd VimLeave * silent !echo -ne "\<Esc>Ptmux;\<Esc>\033]Pl3971ED\033\\"
else
    let &t_EI = "\033]Pl3971ED\033\\"
    let &t_SI = "\033]PlFBA922\033\\"
    silent !echo -ne "\033]Pl3971ED\033\\"
    autocmd VimLeave * silent !echo -ne "\033]Pl3971ED\033\\"
endif


" let g:fzf_preview_git_status_preview_command =  "[[ $(git diff --cached -- {-1}) != \"\" ]] && git diff --cached --color=always -- {-1} | delta || " .
" \ "[[ $(git diff -- {-1}) != \"\" ]] && git diff --color=always -- {-1} | delta || " .
" \ g:fzf_preview_command
