" curl -sLo/tmp/win32yank.zip https://github.com/equalsraf/win32yank/releases/download/v0.0.4/win32yank-x64.zip
" unzip -p /tmp/win32yank.zip win32yank.exe > /tmp/win32yank.exe
" chmod +x /tmp/win32yank.exe
" sudo mv /tmp/win32yank.exe /usr/local/bin/

" let g:clipboard = {
"       \   'name': 'win32yank-wsl',
"       \   'copy': {
"       \      '+': 'win32yank.exe -i --crlf',
"       \      '*': 'win32yank.exe -i --crlf',
"       \    },
"       \   'paste': {
"       \      '+': 'win32yank.exe -o --lf',
"       \      '*': 'win32yank.exe -o --lf',
"       \   },
"       \   'cache_enabled': 0,
"       \ }
"
"
" -- gx => open url in browser
" -- if vim.fn.has("wsl") then
" -- 	vim.g.netrw_browsex_viewer = "/usr/bin/wslview"
" -- 	vim.g.clipboard = {
" -- 		name = "wsl clipboard",
" -- 		copy = { ["+"] = { "clip.exe" }, ["*"] = { "clip.exe" } },
" -- 		paste = { ["+"] = { "nvim_paste" }, ["*"] = { "nvim_paste" } },
" -- 		cache_enabled = true,
" -- 	}
" -- elseif vim.fn.has("mac") then
" -- 	vim.g.netrw_browsex_viewer = "open"
" -- elseif vim.fn.has("linux") then
" -- 	-- vim.g.netrw_browsex_viewer = "/usr/bin/xdg-open"
" -- 	vim.g.netrw_browsex_viewer = "/run/current-system/sw/bin/xdg-open"
" -- 	-- vim.g.netrw_browsex_viewer = vim.env.BROWSER
" -- end
