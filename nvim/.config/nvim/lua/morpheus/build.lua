-- Build and run configurations for various filetypes
-- Converted from after/ftplugin/build.vim

local M = {}

-- Compile and run keymaps (F5)
local build_commands = {
  cpp = ':w <bar> !rm -rf %:r && g++ -ggdb -O2 -ulimit -Wall -Wno-unused-result -std=c++11 % -o %:r && ./%:r <CR>',
  c = ':w <bar> !rm -rf %:r && gcc -ggdb -Wall -Werror -std=c99 % -o %:r && ./%:r <CR>',
  python = ':w <bar> !python3 % <CR>',
  hy = ':w <bar> !hy % <CR>',
  perl = ':w <bar> !perl % <CR>',
  rmd = ':w <bar> !Rscript -e "rmarkdown::render(\'%\')"<CR>',
  md = ':w <bar> !pandoc %  -o %.pdf<CR>',
}

-- Floaterm run keymaps (F6)
local floaterm_commands = {
  c = ':FloatermNew --autoclose=0 ./%< <CR>',
  lua = ':FloatermNew --autoclose=0 lua % <CR>',
  python = ':FloatermNew --autoclose=0 python3 % <CR>',
  hy = ':FloatermNew --autoclose=0 hy % <CR>',
  perl = ':FloatermNew --autoclose=0 perl % <CR>',
  md = ':w <bar> !zathura \'%<\'.pdf&;disown<cr>:redraw!<cr>',
}

-- PDF viewer command for rmd (F6)
local function get_rmd_pdf_command()
  if vim.fn.has("macunix") == 1 or vim.fn.has("Darwin") == 1 then
    return ":w <bar> !open '%<'.pdf&;disown<cr>:redraw!<cr>"
  else
    return ":w <bar> !zathura '%<'.pdf&;disown<cr>:redraw!<cr>"
  end
end

function M.setup()
  local augroup = vim.api.nvim_create_augroup("BuildCommands", { clear = true })

  -- Set up F5 build commands
  for ft, cmd in pairs(build_commands) do
    vim.api.nvim_create_autocmd("FileType", {
      group = augroup,
      pattern = ft,
      callback = function()
        vim.keymap.set('n', '<F5>', cmd, { buffer = true, silent = true })
      end,
    })
  end

  -- Set up F6 floaterm/run commands
  for ft, cmd in pairs(floaterm_commands) do
    vim.api.nvim_create_autocmd("FileType", {
      group = augroup,
      pattern = ft,
      callback = function()
        vim.keymap.set('n', '<F6>', cmd, { buffer = true, silent = true })
      end,
    })
  end

  -- Special handling for rmd F6 (OS-dependent)
  vim.api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "rmd",
    callback = function()
      vim.keymap.set('n', '<F6>', get_rmd_pdf_command(), { buffer = true, silent = true })
    end,
  })

  -- Register .hy files as hy filetype
  vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
    group = augroup,
    pattern = "*.hy",
    callback = function()
      vim.bo.filetype = "hy"
    end,
  })

  -- Markdown image paste setup
  vim.api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "markdown",
    callback = function()
      vim.g.PasteImageFunction = 'g:MarkdownPasteImage'
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "tex",
    callback = function()
      vim.g.PasteImageFunction = 'g:LatexPasteImage'
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "rmd",
    callback = function()
      vim.keymap.set('n', '<leader>p', ':call mdip#MarkdownClipboardImage()<CR>', { buffer = true, silent = true })
    end,
  })

  -- Image directory for markdown paste
  vim.g.mdip_imgdir = 'imgs'

  -- C++ syntax highlighting options
  vim.g.cpp_class_scope_highlight = 1
  vim.g.cpp_member_variable_highlight = 1
  vim.g.cpp_class_decl_highlight = 1

  -- Syntastic settings
  vim.g.syntastic_always_populate_loc_list = 1
  vim.g.syntastic_auto_loc_list = 1
  vim.g.syntastic_check_on_open = 1
  vim.g.syntastic_check_on_wq = 0

  -- Syntastic checkers
  vim.g.syntastic_c_checkers = { 'gcc', 'make' }
  vim.g.syntastic_cpp_checkers = { 'gcc' }
  vim.g.syntastic_python_checkers = { 'pylint' }
  vim.g.syntastic_shell_checkers = { 'shellcheck' }

  -- Syntastic highlighting
  vim.cmd([[
    highlight link SyntasticError SpellBad
    highlight link SyntasticWarning SpellCap
  ]])
end

return M
