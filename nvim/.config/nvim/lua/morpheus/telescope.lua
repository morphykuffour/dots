local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
	return
end

local actions = require("telescope.actions")
local fb_actions = require("telescope").extensions.file_browser.actions
local fb_utils = require("telescope._extensions.file_browser.utils")
local action_state = require("telescope.actions.state")
local Job = require("plenary.job")

telescope.setup({
	defaults = {

		prompt_prefix = " ",
		selection_caret = " ",
		path_display = { "smart" },

		mappings = {
			i = {
				["<C-n>"] = actions.cycle_history_next,
				["<C-p>"] = actions.cycle_history_prev,
				["<C-j>"] = actions.move_selection_next,
				["<C-k>"] = actions.move_selection_previous,
				["<C-c>"] = actions.close,
				["<Down>"] = actions.move_selection_next,
				["<Up>"] = actions.move_selection_previous,
				["<CR>"] = actions.select_default,
				["<C-x>"] = actions.select_horizontal,
				["<C-v>"] = actions.select_vertical,
				["<C-t>"] = actions.select_tab,
				["<C-u>"] = actions.preview_scrolling_up,
				["<C-d>"] = actions.preview_scrolling_down,
				["<PageUp>"] = actions.results_scrolling_up,
				["<PageDown>"] = actions.results_scrolling_down,
				["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
				["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
				["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
				["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
				["<C-l>"] = actions.complete_tag,
				["<C-_>"] = actions.which_key, -- keys from pressing <C-/>
			},

			n = {
				["<esc>"] = actions.close,
				["<CR>"] = actions.select_default,
				["<C-x>"] = actions.select_horizontal,
				["<C-v>"] = actions.select_vertical,
				["<C-t>"] = actions.select_tab,
				["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
				["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
				["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
				["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
				["j"] = actions.move_selection_next,
				["k"] = actions.move_selection_previous,
				["H"] = actions.move_to_top,
				["M"] = actions.move_to_middle,
				["L"] = actions.move_to_bottom,
				["<Down>"] = actions.move_selection_next,
				["<Up>"] = actions.move_selection_previous,
				["gg"] = actions.move_to_top,
				["G"] = actions.move_to_bottom,
				["<C-u>"] = actions.preview_scrolling_up,
				["<C-d>"] = actions.preview_scrolling_down,
				["<PageUp>"] = actions.results_scrolling_up,
				["<PageDown>"] = actions.results_scrolling_down,
			},
		},
	},
	pickers = {
		-- Default configuration for builtin pickers goes here:
		-- picker_name = {
		--   picker_config_key = value,
		--   ...
		-- }
		-- Now the picker_config_key will be applied every time you call this
		-- builtin picker
	},
	extensions = {
		fzf = {
			fuzzy = true, -- false will only do exact matching
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
			-- the default case_mode is "smart_case"
		},
		bookmarks = {
			selected_browser = "brave",
			url_open_command = "open",
			url_open_plugin = "open_browser",
			full_path = true,
			firefox_profile_name = nil,
		},
		file_browser = {
			theme = "ivy",
			hijack_netrw = true,
			mappings = {
				["i"] = {
					["<C-h>"] = fb_actions.goto_home_dir,

					-- mass renameing with edir
					["<C-r>"] = function(prompt_bufnr)
						-- bulk rename with edir
						-- https://github.com/bulletmark/edir
						-- local quiet = action_state.get_current_picker(prompt_bufnr).finder.quiet
						local selections = fb_utils.get_selected_files(prompt_bufnr, true)
						Job:new({ "edir", selections }):start()
					end,
				},
				["n"] = {
					-- your custom normal mode mappings
					["<C-h>"] = fb_actions.goto_home_dir,
					["."] = fb_actions.toggle_hidden,
					["dd"] = fb_actions.remove,
					["re"] = fb_actions.rename,
					["yy"] = fb_actions.copy,
					["c"] = fb_actions.create,
					["p"] = fb_actions.move,
					["o"] = fb_actions.open,
				},
			},
		},
	},
})

-- require("telescope").load_extension("fzy_native")
-- telescope.load_extension("fzf")
-- telescope.load_extension("neoclip")
-- telescope.load_extension("bookmarks")
telescope.load_extension("file_browser")

local M = {}

M.search_dotfiles = function()
	require("telescope.builtin").find_files({
		prompt_title = "< dotfiles >",
		cwd = vim.env.DOTFILES,
		hidden = true,
	})
end

M.search_vimrc = function()
	require("telescope.builtin").find_files({
		prompt_title = "< vimrc >",
		cwd = "~/.config/nvim/",
		hidden = true,
	})
end

function M.installed_plugins()
	require("telescope.builtin").find_files({
		prompt_title = "< searching installed plugins >",
		cwd = vim.fn.stdpath("data") .. "/site/pack/packer/start/",
	})
end

function M.search_all_files()
	require("telescope.builtin").find_files({
		prompt_title = "< searching all files >",
		find_command = { "rg", "--no-ignore", "--files" },
	})
end

-- local function set_background(content)
--     vim.fn.system(
--         "dconf write /org/mate/desktop/background/picture-filename \"'"
--             .. content
--             .. "'\""
--     )
-- end
--
-- local function select_background(prompt_bufnr, map)
--     local function set_the_background(close)
--         local content = require("telescope.actions.state").get_selected_entry(
--             prompt_bufnr
--         )
--         set_background(content.cwd .. "/" .. content.value)
--         if close then
--             require("telescope.actions").close(prompt_bufnr)
--         end
--     end
--
--     map("i", "<C-p>", function()
--         set_the_background()
--     end)
--
--     map("i", "<CR>", function()
--         set_the_background(true)
--     end)
-- end
--
-- local function image_selector(prompt, cwd)
--     return function()
--         require("telescope.builtin").find_files({
--             prompt_title = prompt,
--             cwd = cwd,
--
--             attach_mappings = function(prompt_bufnr, map)
--                 select_background(prompt_bufnr, map)
--
--                 -- Please continue mapping (attaching additional key maps):
--                 -- Ctrl+n/p to move up and down the list.
--                 return true
--             end,
--         })
--     end
-- end
-- M.anime_selector = image_selector("< Wallpapers > ", "~/Pictures/wallpapers/")

return M
