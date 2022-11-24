"" Tab mappings TODO move to mappings.vim
"" Goodies
"" command! W w

""Navigate quickfix list
"nnoremap <leader>k :lnext<CR>zz
"nnoremap <leader>j :lprev<CR>zz

"" Remove trailing whitespaces
"nnoremap <silent> <leader>qed :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

"" Better Y
"nnoremap Y y$

"" Undo Break points
"inoremap , ,<c-g>u
"inoremap . .<c-g>u
"inoremap ! !<c-g>u
"inoremap ? ?<c-g>u
"inoremap [ [<c-g>u

"" Jumplist mutatuions
"nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
"nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'j'

"" Mappings to move lines
"xnoremap K :move '<-2<CR>gv-gv
"xnoremap J :move '>+1<CR>gv-gv

"" Special g
"vmap j gj
"vmap k gk
"vmap $ g$
"vmap ^ g^
"nmap $ g$
"nmap ^ g^

"nnoremap <leader><F1> :Startify<CR>

"nnoremap vs :vs<CR>
"nnoremap sp :sp<CR>
"nnoremap <leader>y "+y
"vnoremap <leader>y "+y
"nnoremap <leader>Y gg"+yG
"nnoremap <leader>d "_d
"vnoremap <leader>d "_d
"nnoremap <leader>q :q!<CR>

"vnoremap <leader>p "_dP
"nnoremap <leader>y "+y
"vnoremap <leader>y "+y
"nnoremap <leader>Y gg"+yG<C-o>
"nnoremap <leader>d "d_
"vnoremap <leader>d "d_
