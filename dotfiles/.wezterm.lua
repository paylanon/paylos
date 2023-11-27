-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font 'Sarasa Mono CL Nerd Font'

-- This is where you actually apply your config choices
config.font_size = 11

-- For example, changing the color scheme:
-- config.color_scheme = 'Bespin (dark) (terminal.sexy)'
config.color_scheme = 'Ashes (base16)'
config.color_scheme = 'Tomorrow Night Eighties'
config.color_scheme = 'Pnevma'
config.color_scheme = 'Red Planet'

config.window_close_confirmation = 'NeverPrompt'

config.window_padding = {
  left = 10,
  right = 10,
  top = 10,
  bottom = 10,
}

config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true

config.window_background_opacity = 0.95

-- and finally, return the configuration to wezterm
return config
