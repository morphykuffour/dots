function Scratch()
    execute 'tabnew '
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
endfunction

command! -nargs=0 Ns call Newscratch()
" command! Scratch call Scratch()
