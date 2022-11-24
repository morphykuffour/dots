vim.cmd([["set statusline+=%#warningmsg#"]])
vim.cmd([["set statusline+=%{SyntasticStatuslineFlag()}"]])
vim.cmd([["set statusline+=%*"]])

vim.g.syntastic_always_populate_loc_list = 1
vim.g.syntastic_auto_loc_list = 1
vim.g.syntastic_check_on_open = 1
vim.g.syntastic_check_on_wq = 0
