version = "0.18.0"
-- xplr.config.general.show_hidden = true
-- Needed for plugins
local home = os.getenv("HOME")
package.path = home .. "/.config/xplr/plugins/?/init.lua;" .. home .. "/.config/xplr/plugins/?.lua;" .. package.path

require("icons").setup()

require("qrcp").setup({
	mode = "action",
	key = "Q",
	send_options = "-i eth0",
	receive_options = "-i eth0",
})

-- nvim-ctrl
require("nvim-ctrl").setup({
	mode = "default",
	keys = {
		["ctrl-e"] = "tabedit",
		["e"] = "e",
	},
})

local function stat(node)
	return node.mime_essence
end

local function read(path, lines)
	local out = ""
	local p = io.open(path)

	if p == nil then
		return stat(path)
	end

	local i = 0
	for line in p:lines() do
		out = out .. line .. "\n"
		if i == lines then
			break
		end
		i = i + 1
	end
	p:close()

	return out
end

xplr.config.layouts.builtin.default = {
	Horizontal = {
		config = {
			constraints = {
				{ Percentage = 60 },
				{ Percentage = 40 },
			},
		},
		splits = {
			"Table",
			{
				CustomContent = {
					title = "preview",
					body = { DynamicParagraph = { render = "custom.preview_pane.render" } },
				},
			},
		},
	},
}

xplr.fn.custom.preview_pane = {}
xplr.fn.custom.preview_pane.render = function(ctx)
	local n = ctx.app.focused_node

	if n.canonical then
		n = n.canonical
	end

	if n then
		if n.is_file then
			return read(n.absolute_path, ctx.layout_size.height)
		else
			return stat(n)
		end
	else
		return ""
	end
end
