"-----------------------------"
" copy this file to ~/.vimrc  "
" or load it with :source vim "
"-----------------------------"

colorscheme desert
" my own indentation for C using the coding styles
set cindent
set tabstop=8
set noexpandtab
set smartindent
set cino=:0,+0,(2,J0,{1,}0,>4,)1,m2

" pancake's exposee for vim:
let fs=0
fun Exposee()
if (g:fs == 0)
  res 1000
  vertical res 1000
  let g:fs=1
else
  exe "normal \<C-W>="
  let g:fs=0
endif
endfun
map <F10> :call Exposee()<cr>

"some nice keymappings
map <F1> :vsp<cr>
map <F2> :sp<cr>
map <F3> :sp<cr>:e .<cr>
map <F4> :q<cr>
map <F5> <C-W>=

map <F9> :make<cr>
map <C-F9> :cnext<cr>
map <S-F9> :cprevious<cr>

" fine zooming
map <C-J> 2<C-W>+
map <C-K> 2<C-W>-
map <C-L> 2<C-W>>
map <C-H> 2<C-W><

" fine frame moving
map <C-Y> <C-W>h
map <C-U> <C-W>j
map <C-I> <C-W>k
map <C-O> <C-W>l

au BufNewFile,BufRead *.vala setf cs
au BufNewFile,BufRead *.vapi setf cs
au BufNewFile,BufRead *.gtkaml setf cs
au BufNewFile,BufRead *.gtkon setf cs

filetype indent on
colorscheme pablo
set foldmethod=marker
set hlsearch
set paste
sy on


let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
" Plug 'vim-scripts/lookup.vim'
call plug#end()
 " Don't try to be vi compatible
set nocompatible

" Helps force plugins to load correctly when it is turned back on below
filetype off

" TODO: Load plugins here (pathogen or vundle)

" Turn on syntax highlighting
syntax on

" For plugins to load correctly
filetype plugin indent on

" TODO: Pick a leader key
" let mapleader = ","

" Security
set modelines=0

" Show line numbers
set number

" Show file stats
set ruler

" Blink cursor on error instead of beeping (grr)
set visualbell

" Encoding
set encoding=utf-8

" Whitespace
set wrap
set textwidth=79
set formatoptions=tcqrn1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set noshiftround

" Cursor motion
set scrolloff=3
set backspace=indent,eol,start
set matchpairs+=<:> " use % to jump between pairs
runtime! macros/matchit.vim

" Move up/down editor lines
nnoremap j gj
nnoremap k gk

" Allow hidden buffers
set hidden

" Rendering
set ttyfast

" Status bar
set laststatus=2

" Last line
set showmode
set showcmd

" Searching
nnoremap / /\v
vnoremap / /\v
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
map <leader><space> :let @/=''<cr> " clear search

" Remap help key.
inoremap <F1> <ESC>:set invfullscreen<CR>a
nnoremap <F1> :set invfullscreen<CR>
vnoremap <F1> :set invfullscreen<CR>

" Textmate holdouts

" Formatting
map <leader>q gqip

" Visualize tabs and newlines
set listchars=tab:▸\ ,eol:¬
" Uncomment this to enable by default:
" set list " To enable by default
" Or use your leader key + l to toggle on/off
map <leader>l :set list!<CR> " Toggle tabs and EOL

" Color scheme (terminal)
set t_Co=256
set background=dark
let g:solarized_termcolors=256
let g:solarized_termtrans=1
" put https://raw.github.com/altercation/vim-colors-solarized/master/colors/solarized.vim
" in ~/.vim/colors/ and uncomment:
colorscheme default
