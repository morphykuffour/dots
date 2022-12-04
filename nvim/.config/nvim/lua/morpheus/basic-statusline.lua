-- statusline
-- %<                                             trim from here
-- %{fugitive#head()}                             name of the current branch (needs fugitive.vim)
-- %f                                             path+filename
-- %m                                             check modifi{ed,able}
-- %r                                             check readonly
-- %w                                             check preview window
-- %=                                             left/right separator
-- %l/%L,%c                                       rownumber/total,colnumber
-- %{&fileencoding?&fileencoding:&encoding}       file encoding
vim.opt.statusline =
	" <b>[%N]  %< %{FugitiveHead()}  %F %m %r %w %= %y %{&fileencoding?&fileencoding:&encoding} [%{&fileformat}]  Ln %l, Col %c "

-- %#DiffAdd#%{(mode()=='n')?'\ \ NORMAL\ ':''}
-- %#DiffChange#%{(mode()=='i')?'\ \ INSERT\ ':''}
-- %#DiffDelete#%{(mode()=='r')?'\ \ RPLACE\ ':''}
-- %#Cursor#%{(mode()=='v')?'\ \ VISUAL\ ':''}
