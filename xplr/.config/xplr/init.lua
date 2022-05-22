-- xplr.config.general.show_hidden = true

local home = os.getenv("HOME")
package.path = home
.. "/.config/xplr/plugins/?/init.lua;"
.. home
.. "/.config/xplr/plugins/?.lua;"
.. package.path


-- Or

require("qrcp").setup{
  mode = "action",
  key = "Q",
  send_options = "-i wlp2s0",
  receive_options = "-i wlp2s0",
}

-- Type `:Q` to send or receive files.
