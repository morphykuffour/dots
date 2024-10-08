" TODO convert to lua
" The function Nr2Bin() returns the binary string representation of a number.
func Nr2Bin(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '01'[n % 2] . r
    let n = n / 2
  endwhile
  return r
endfunc

command! Nr2Bin call Nr2Bin()

" Diff lines next to eachother
function! DiffLineWithNext()
    let f1=tempname()
    let f2=tempname()

    exec ".write " . f1
    exec ".+1write " . f2

    exec "tabedit " . f1
    exec "vert diffsplit " . f2
endfunction

nnoremap <F10> :call DiffLineWithNext()

function Scratch()
    execute 'tabnew '
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
endfunction

" command! -nargs=0 Ns call Newscratch()
command! Scratch call Scratch()

" send output of command to a new tab
function! TabCommandOutput(cmd)
    redir => message
    silent execute a:cmd
    redir END
    if empty(message)
        echoerr "no output"
    else
        " use "new" instead of "tabnew" below if you prefer split windows instead of tabs
        tabnew
        setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted nomodified
        silent put=message
    endif
endfunction

command! SyntasticDisableBuffer call SyntasticDisableBuffer()

" The following command allows you to compile and run your C code in the floaterm window:
function! Run()
    FloatermNew --autoclose=0 gcc -g -Wall -Werror -std=c99 -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined % -o %< && ./%<
endfunction

command! Run call Run()

" get full buffername
function! PrintBufferName()
  echo expand('%:p')
endfunction

command! PrintBufferName call  PrintBufferName()
function ColemakDH()

    "Top row
    noremap q q
    noremap w w
    noremap f e
    noremap p r
    noremap b t
    
    noremap j y
    noremap l u
    noremap u i
    noremap ; p
    
    "Middle row
    noremap a a
    noremap r s
    noremap s d
    noremap t f
    noremap g g
    
    noremap m h
    noremap n j
    noremap e k
    noremap i l
    noremap o ;
    
    "Bottom row
    noremap z z
    noremap x x
    noremap c c
    noremap d v
    noremap v b

    noremap k n
    noremap h m
    noremap , ,
    noremap . .
    noremap / /

endfunction

" call ColemakDH()
command! ColemakDH call  ColemakDH()
