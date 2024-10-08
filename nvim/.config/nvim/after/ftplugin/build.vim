" TODO: move to nvim async run
" compile and run
autocmd filetype cpp    nnoremap <F5> :w <bar> !rm -rf %:r && g++ -ggdb -O2 -ulimit -Wall -Wno-unused-result -std=c++11 % -o %:r && ./%:r <CR>
" autocmd filetype c      nnoremap <F5> :w <bar> !rm -rf %:r && gcc -ggdb -Wall -Werror -std=c99 -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined % -o %:r && ./%:r <CR>
autocmd filetype c      nnoremap <F5> :w <bar> !rm -rf %:r && gcc -ggdb -Wall -Werror -std=c99 % -o %:r && ./%:r <CR>
autocmd filetype python nnoremap <F5> :w <bar> !python3 % <CR>
autocmd BufNewFile,BufRead *.hy set filetype=hy
autocmd filetype hy     nnoremap <F5> :w <bar> !hy % <CR>
autocmd filetype perl   nnoremap <F5> :w <bar> !perl % <CR>

" writing with rmarkdown
" https://github.com/img-paste-devs/img-paste.vim
autocmd filetype rmd    nnoremap <F5> :w <bar> !Rscript -e "rmarkdown::render('%')"<CR>
autocmd FileType markdown let g:PasteImageFunction = 'g:MarkdownPasteImage'
autocmd FileType tex let g:PasteImageFunction = 'g:LatexPasteImage'

autocmd FileType rmd nmap <buffer><silent> <leader>p :call mdip#MarkdownClipboardImage()<CR>
" there are some defaults for image directory and image name, you can change them
let g:mdip_imgdir = 'imgs'
" let g:mdip_imgname = 'image'

if !exists("g:os")
    if has("macunix") || has("Darwin")
        autocmd filetype rmd    nnoremap <F6> :w <bar> !open '%<'.pdf&;disown<cr>:redraw!<cr>
    else
        autocmd filetype rmd    nnoremap <F6> :w <bar> !zathura '%<'.pdf&;disown<cr>:redraw!<cr>
    endif
endif


autocmd filetype md     nnoremap <F5> :w <bar> !pandoc %  -o %.pdf<CR>
autocmd filetype md     nnoremap <F6> :w <bar> !zathura '%<'.pdf&;disown<cr>:redraw!<cr>

" Run file in Floaterm
autocmd filetype c      nnoremap <F6> :FloatermNew --autoclose=0 ./%< <CR>
autocmd filetype lua    nnoremap <F6> :FloatermNew --autoclose=0 lua % <CR>
autocmd filetype python nnoremap <F6> :FloatermNew --autoclose=0 python3 % <CR>
autocmd filetype hy     nnoremap <F6> :FloatermNew --autoclose=0 hy % <CR>
autocmd filetype perl   nnoremap <F6> :FloatermNew --autoclose=0 perl % <CR>

" c++ syntax highlighting
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Syntax Checkers
let g:syntastic_c_checkers = ['gcc', 'make']
let g:syntastic_cpp_checkers = ["gcc"]
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_shell_checkers = ['shellcheck']

" Syntastic Highligting
highlight link SyntasticError SpellBad
highlight link SyntasticWarning SpellCap

