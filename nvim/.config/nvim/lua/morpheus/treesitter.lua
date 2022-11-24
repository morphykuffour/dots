if vim.fn.has("mac") ~= 1 then --support for wsl see :h has
	require("nvim-treesitter.configs").setup({
		-- Add languages to be installed here that you want installed for treesitter
		ensure_installed = { "lua", "typescript", "rust", "go", "python" },
		-- ensure_installed = { enable = "all", },

		-- tree_docs = { enable = true },
		highlight = {
			enable = true,
			-- disable = {'org'},
			disable = { "nix" },
		},

		context_commentstring = {
			enable = true,
			enable_autocmd = false,
		},

		indent = { enable = true },
		incremental_selection = {
			enable = true,
			keymaps = {
				init_selection = "<c-space>",
				node_incremental = "<c-space>",
				-- TODO: I'm not sure for this one.
				scope_incremental = "<c-s>",
				node_decremental = "<c-backspace>",
			},
		},

		rainbow = {
			enable = true,
			-- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
			extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
			max_file_lines = nil, -- Do not enable for files with more than n lines, int
			-- colors = {}, -- table of hex strings
			-- termcolors = {} -- table of colour name strings
		},

		playground = {
			enable = true,
			disable = {},
			updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
			persist_queries = false, -- Whether the query persists across vim sessions
			keybindings = {
				toggle_query_editor = "o",
				toggle_hl_groups = "i",
				toggle_injected_languages = "t",
				toggle_anonymous_nodes = "a",
				toggle_language_display = "I",
				focus_language = "f",
				unfocus_language = "F",
				update = "R",
				goto_node = "<cr>",
				show_help = "?",
			},
		},
		textobjects = {
			select = {
				enable = true,
				lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
				keymaps = {
					-- You can use the capture groups defined in textobjects.scm
					["af"] = "@function.outer",
					["if"] = "@function.inner",
					["ac"] = "@class.outer",
					["ic"] = "@class.inner",
				},
			},
			move = {
				enable = true,
				set_jumps = true, -- whether to set jumps in the jumplist
				goto_next_start = {
					["]m"] = "@function.outer",
					["]]"] = "@class.outer",
				},
				goto_next_end = {
					["]M"] = "@function.outer",
					["]["] = "@class.outer",
				},
				goto_previous_start = {
					["[m"] = "@function.outer",
					["[["] = "@class.outer",
				},
				goto_previous_end = {
					["[M"] = "@function.outer",
					["[]"] = "@class.outer",
				},
			},
			swap = {
				enable = true,
				swap_next = {
					["<leader>a"] = "@parameter.inner",
				},
				swap_previous = {
					["<leader>A"] = "@parameter.inner",
				},
			},
		},
	})
end
