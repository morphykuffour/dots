-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`

-- Configure compiler environment for cross-platform compatibility
local function setup_compiler_env()
  local uv = vim.uv or vim.loop
  local os_name = uv.os_uname().sysname:lower()
  local arch = uv.os_uname().machine:lower()
  
  -- Common compiler flags
  local cxx_flags = ""
  local ld_flags = ""
  local cpp_flags = ""
  
  if os_name == "darwin" then
    -- macOS configuration - Use system clang for better compatibility
    local clt_path = "/Library/Developer/CommandLineTools"
    local homebrew_prefix = arch == "arm64" and "/opt/homebrew" or "/usr/local"
    local homebrew_llvm = homebrew_prefix .. "/opt/llvm"
    
    -- Prefer system clang over Homebrew LLVM for treesitter compatibility
    if vim.fn.executable("/usr/bin/clang") == 1 then
      vim.env.CC = "/usr/bin/clang"
      vim.env.CXX = "/usr/bin/clang++"
    elseif vim.fn.isdirectory(homebrew_llvm) == 1 then
      vim.env.CC = homebrew_llvm .. "/bin/clang"
      vim.env.CXX = homebrew_llvm .. "/bin/clang++"
      ld_flags = "-L" .. homebrew_llvm .. "/lib"
      cpp_flags = "-I" .. homebrew_llvm .. "/include"
    end
    
    -- Set SDK path for macOS
    if vim.fn.isdirectory(clt_path) == 1 then
      vim.env.SDKROOT = clt_path .. "/SDKs/MacOSX.sdk"
    end
    
    -- macOS-specific flags - Use C++14 for better compatibility with macOS SDK
    cxx_flags = "-std=c++14 -stdlib=libc++ -fPIC"
    
    -- Additional macOS-specific environment variables
    vim.env.MACOSX_DEPLOYMENT_TARGET = "10.15"
    
  elseif os_name == "linux" then
    -- Linux configuration
    local possible_compilers = {
      -- Check for system clang first
      { cc = "clang", cxx = "clang++" },
      -- Fallback to gcc
      { cc = "gcc", cxx = "g++" }
    }
    
    for _, compiler in ipairs(possible_compilers) do
      if vim.fn.executable(compiler.cc) == 1 and vim.fn.executable(compiler.cxx) == 1 then
        vim.env.CC = compiler.cc
        vim.env.CXX = compiler.cxx
        break
      end
    end
    
    -- Linux-specific flags
    cxx_flags = "-std=c++17 -fPIC"
    
  elseif os_name == "windows" or os_name == "windows_nt" then
    -- Windows configuration
    local possible_paths = {
      -- Visual Studio Build Tools
      "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC",
      "C:\\Program Files\\Microsoft Visual Studio\\2022\\BuildTools\\VC\\Tools\\MSVC",
      "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC",
      "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC",
      -- LLVM for Windows
      "C:\\Program Files\\LLVM",
      "C:\\Program Files (x86)\\LLVM"
    }
    
    -- Try to find MSVC or LLVM
    for _, path in ipairs(possible_paths) do
      if vim.fn.isdirectory(path) == 1 then
        if path:match("MSVC") then
          -- Found Visual Studio
          vim.env.CC = "cl"
          vim.env.CXX = "cl"
          cxx_flags = "/std:c++17"
        elseif path:match("LLVM") then
          -- Found LLVM
          vim.env.CC = path .. "\\bin\\clang.exe"
          vim.env.CXX = path .. "\\bin\\clang++.exe"
          cxx_flags = "-std=c++17"
        end
        break
      end
    end
    
    -- Windows-specific environment setup
    if vim.env.VCVARSALL then
      -- Use Visual Studio environment if available
      vim.env.CC = "cl"
      vim.env.CXX = "cl"
    end
  end
  
  -- Set environment variables
  if ld_flags ~= "" then
    vim.env.LDFLAGS = ld_flags
  end
  if cpp_flags ~= "" then
    vim.env.CPPFLAGS = cpp_flags
  end
  vim.env.CXXFLAGS = cxx_flags
  
  -- Debug information
  if vim.env.NVIM_TREESITTER_DEBUG then
    print("Treesitter compiler environment:")
    print("OS: " .. os_name)
    print("Arch: " .. arch)
    print("CC: " .. (vim.env.CC or "not set"))
    print("CXX: " .. (vim.env.CXX or "not set"))
    print("CXXFLAGS: " .. (vim.env.CXXFLAGS or "not set"))
    print("SDKROOT: " .. (vim.env.SDKROOT or "not set"))
  end
end

-- Optimized treesitter setup with lazy loading and caching
local function setup_treesitter()
  -- Set up compiler environment only when needed
  setup_compiler_env()
  
  require('nvim-treesitter.configs').setup {
    -- Do not auto-install parsers; rely on vim-markdown for fenced highlighting
    ensure_installed = {},

    -- Disable autoinstall to prevent recompilation on startup
    auto_install = false,
    
    -- Disable sync install to prevent blocking startup
    sync_install = false,
    
    -- Only install parsers when explicitly requested
    ignore_install = {},
    
    -- Enable parser caching to prevent recompilation
    parser_install_dir = vim.fn.stdpath('data') .. '/treesitter',

    highlight = { 
    enable = true,
    -- Enable additional languages for better highlighting
    additional_vim_regex_highlighting = { 'markdown' },
    -- Disable for large files to improve performance
    disable = function(lang, buf)
      local max_filesize = 100 * 1024 -- 100 KB
      local ok, stats = pcall((vim.uv or vim.loop).fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
  },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<c-space>',
      node_incremental = '<c-space>',
      scope_incremental = '<c-s>',
      node_decremental = '<M-space>',
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['<leader>a'] = '@parameter.inner',
      },
      swap_previous = {
        ['<leader>A'] = '@parameter.inner',
      },
    },
  },
}

-- Additional configuration for markdown files
-- This ensures code blocks in markdown get proper syntax highlighting
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "markdown" },
  callback = function()
    -- Enable syntax highlighting for code blocks
    vim.opt_local.syntax = "markdown"
    
    -- Set up markdown folding with custom expression (more reliable than vim-markdown)
    vim.opt_local.foldmethod = "expr"
    vim.opt_local.foldexpr = "getline(v:lnum)=~'^#'?'>'.len(matchstr(getline(v:lnum),'^#\\+')):getline(v:lnum)=~'^\\s*$'?'=':'1'"
    vim.opt_local.foldlevel = 99 -- Start with all folds open like VSCode
    vim.opt_local.foldenable = true
    
    -- Set up proper highlighting for code blocks
    vim.cmd([[
      " Enable syntax highlighting for code blocks
      syntax sync fromstart
      
      " Custom highlighting for code blocks
      hi markdownCodeBlock ctermbg=235 guibg=#262626
      hi markdownCode ctermbg=235 guibg=#262626
      
      " Ensure code blocks are properly highlighted
      hi link markdownCodeBlock markdownCode
    ]])
  end,
})
end

-- Initialize Treesitter at startup, not on first file open
setup_treesitter()

-- Add command to update treesitter parsers with proper compiler environment
vim.api.nvim_create_user_command('TSUpdateCompile', function()
  local os_name = (vim.uv or vim.loop).os_uname().sysname:lower()
  
  -- Set up compiler environment before updating
  setup_compiler_env()
  
  -- Update treesitter parsers
  vim.cmd('TSUpdate')
  
  -- Show completion message with OS info
  vim.notify('Treesitter parsers updated with proper compiler environment for ' .. os_name, vim.log.levels.INFO)
end, { desc = 'Update treesitter parsers with proper compiler environment' })

-- Add command to clean and reinstall treesitter parsers
vim.api.nvim_create_user_command('TSReinstall', function()
  local os_name = (vim.uv or vim.loop).os_uname().sysname:lower()
  
  -- Set up compiler environment
  setup_compiler_env()
  
  -- Clean and reinstall (handle case where TSUninstall might not work)
  local success = pcall(vim.cmd, 'TSUninstall all')
  if not success then
    vim.notify('TSUninstall failed, proceeding with TSInstall only', vim.log.levels.WARN)
  end
  
  -- Install all configured parsers
  local parsers = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'tsx', 'javascript', 'typescript', 'vimdoc', 'vim', 'markdown', 'markdown_inline', 'bash', 'json', 'yaml' }
  for _, parser in ipairs(parsers) do
    vim.cmd('TSInstall ' .. parser)
  end
  
  vim.notify('Treesitter parsers cleaned and reinstalled for ' .. os_name, vim.log.levels.INFO)
end, { desc = 'Clean and reinstall treesitter parsers' })

-- Add command to show current compiler environment
vim.api.nvim_create_user_command('TSShowEnv', function()
  local uv = vim.uv or vim.loop
  local os_name = uv.os_uname().sysname:lower()
  local arch = uv.os_uname().machine:lower()
  
  local info = {
    "Treesitter Compiler Environment:",
    "OS: " .. os_name,
    "Architecture: " .. arch,
    "CC: " .. (vim.env.CC or "not set"),
    "CXX: " .. (vim.env.CXX or "not set"),
    "CXXFLAGS: " .. (vim.env.CXXFLAGS or "not set"),
    "LDFLAGS: " .. (vim.env.LDFLAGS or "not set"),
    "CPPFLAGS: " .. (vim.env.CPPFLAGS or "not set"),
  }
  
  if vim.env.SDKROOT then
    table.insert(info, "SDKROOT: " .. vim.env.SDKROOT)
  end
  
  vim.notify(table.concat(info, "\n"), vim.log.levels.INFO)
end, { desc = 'Show current treesitter compiler environment' })

