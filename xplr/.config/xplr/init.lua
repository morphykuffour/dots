version = "0.18.0"
-- xplr.config.general.show_hidden = true
-- Needed for plugins
local home = os.getenv("HOME")
package.path = home
.. "/.config/xplr/plugins/?/init.lua;"
.. home
.. "/.config/xplr/plugins/?.lua;"
.. package.path

require("qrcp").setup{
  mode = "action",
  key = "Q",
  send_options = "-i wlp2s0",
  receive_options = "-i wlp2s0",
}

-- nvim-ctrl
require("nvim-ctrl").setup{
  mode = "default",
  keys = {
    ["ctrl-e"] = "tabedit",
    ["e"] = "e",
  },
}

