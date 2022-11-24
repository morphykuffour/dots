vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append("c")

-- Complextras.nvim configuration
vim.api.nvim_set_keymap(
	"i",
	"<C-x><C-m>",
	[[<c-r>=luaeval("require('complextras').complete_matching_line()")<CR>]],
	{ noremap = true }
)

vim.api.nvim_set_keymap(
	"i",
	"<C-x><C-d>",
	[[<c-r>=luaeval("require('complextras').complete_line_from_cwd()")<CR>]],
	{ noremap = true }
)

local cmp = require("cmp")
local source_mapping = {
	buffer = "[buffer]",
	nvim_lsp = "[LSP]",
	nvim_lua = "[api]",
	cmp_tabnine = "[tn]",
	luasnip = "[snip]",
	path = "[path]",
}

local ok, lspkind = pcall(require, "lspkind")
if not ok then
	return
end

lspkind.init()

cmp.setup({
	mapping = {
		["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-u>"] = cmp.mapping.scroll_docs(-4),
		["<C-d>"] = cmp.mapping.scroll_docs(4),
		["<c-space>"] = cmp.mapping.complete(),
		["<c-y>"] = cmp.mapping(
			cmp.mapping.confirm({
				behavior = cmp.ConfirmBehavior.Insert,
				select = true,
			}),
			{ "i", "c" }
		),

		-- ["<tab>"] = false,
		["<tab>"] = cmp.config.disable,

		-- If you want tab completion :'(
		--  First you have to just promise to read `:help ins-completion`.
		--
		-- ["<Tab>"] = function(fallback)
		--   if cmp.visible() then
		--     cmp.select_next_item()
		--   else
		--     fallback()
		--   end
		-- end,
		-- ["<S-Tab>"] = function(fallback)
		--   if cmp.visible() then
		--     cmp.select_prev_item()
		--   else
		--     fallback()
		--   end
		-- end,
	},

	sources = {
		{ name = "cmp_tabnine" },
		{ name = "nvim_lsp" },
		{ name = "buffer", keyword_length = 3 },
		{ name = "nvim_lua" },
		{ name = "path" },
		{ name = "luasnip" },
	},

	sorting = {
		-- TODO: Would be cool to add stuff like "See variable names before method names" in rust, or something like that.
		comparators = {
			cmp.config.compare.offset,
			cmp.config.compare.exact,
			cmp.config.compare.score,

			-- copied from cmp-under, but I don't think I need the plugin for this.
			-- I might add some more of my own.
			function(entry1, entry2)
				local _, entry1_under = entry1.completion_item.label:find("^_+")
				local _, entry2_under = entry2.completion_item.label:find("^_+")
				entry1_under = entry1_under or 0
				entry2_under = entry2_under or 0
				if entry1_under > entry2_under then
					return false
				elseif entry1_under < entry2_under then
					return true
				end
			end,

			cmp.config.compare.kind,
			cmp.config.compare.sort_text,
			cmp.config.compare.length,
			cmp.config.compare.order,
		},
	},

	-- Youtube: mention that you need a separate snippets plugin
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},

	-- formatting = {
	-- 	-- Youtube: How to set up nice formatting for your sources.
	-- 	format = lspkind.cmp_format({
	-- 		with_text = true,
	-- 		menu = {
	-- 			buffer = "[bufffer]",
	-- 			nvim_lsp = "[LSP]",
	-- 			nvim_lua = "[api]",
	-- 			path = "[path]",
	-- 			luasnip = "[snip]",
	-- 			-- gh_issues = "[issues]",
	-- 			tn = "[TabNine]",
	-- 		},
	-- 	}),
	-- },

	formatting = {
		format = function(entry, vim_item)
			vim_item.kind = lspkind.presets.default[vim_item.kind]
			local menu = source_mapping[entry.source.name]
			if entry.source.name == "cmp_tabnine" then
				if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
					menu = entry.completion_item.data.detail .. " " .. menu
				end
				vim_item.kind = ""
			end
			vim_item.menu = menu
			return vim_item
		end,
	},

	experimental = {
		-- I like the new menu better! Nice work hrsh7th
		native_menu = false,

		-- Let's play with this for a day or two
		ghost_text = false,
	},
})

_ = vim.cmd([[
  augroup CmpZsh
    au!
    autocmd Filetype zsh lua require'cmp'.setup.buffer { sources = { { name = "zsh" }, } }
  augroup END
]])
