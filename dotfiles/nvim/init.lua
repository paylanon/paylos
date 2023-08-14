-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Configure clipboard:
vim.opt.clipboard = "unnamedplus"

-- Initialize lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
'junegunn/vim-easy-align',
'https://github.com/junegunn/vim-github-dashboard.git',
'SirVer/ultisnips',
'honza/vim-snippets',
'preservim/nerdtree',
'tpope/vim-fireplace',
'rdnetto/YCM-Generator',
'fatih/vim-go',
'nsf/gocode',
'junegunn/fzf',
-- '~/my-prototype-plugin',
'lervag/vimtex',
'https://github.com/rebelot/kanagawa.nvim',
'nvim-lualine/lualine.nvim',
'nvim-tree/nvim-web-devicons',
'https://github.com/akinsho/bufferline.nvim',
'glepnir/dashboard-nvim',
'nvim-tree/nvim-tree.lua',
'folke/which-key.nvim',
'nvim-lua/plenary.nvim',
'nvim-telescope/telescope.nvim',
-- 'neoclide/coc.nvim',
'nvim-treesitter/nvim-treesitter',
'sharkdp/fd',
'https://github.com/tom-anders/telescope-vim-bookmarks.nvim',
'MattesGroeger/vim-bookmarks'
}

require("lazy").setup(plugins, opts)

-- -- VIM-Plug (Deprecated)
-- -- Execute Vimscript using vim.cmd
-- vim.cmd [[
-- " Initialize Plug
-- call plug#begin()

-- 'junegunn/vim-easy-align'
-- 'https://github.com/junegunn/vim-github-dashboard.git'
-- 'SirVer/ultisnips'
-- 'honza/vim-snippets'
-- 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }
-- 'tpope/vim-fireplace', { 'for': 'clojure' }
-- 'rdnetto/YCM-Generator', { 'branch': 'stable' }
-- 'fatih/vim-go', { 'tag': '*' }
-- 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
-- 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
-- '~/my-prototype-plugin'
-- 'lervag/vimtex'
-- 'https://github.com/rebelot/kanagawa.nvim'
-- 'nvim-lualine/lualine.nvim'
-- 'nvim-tree/nvim-web-devicons'
-- 'https://github.com/akinsho/bufferline.nvim'
-- 'glepnir/dashboard-nvim'
-- 'nvim-tree/nvim-tree.lua'
-- 'folke/which-key.nvim'
-- 'nvim-lua/plenary.nvim'
-- 'nvim-telescope/telescope.nvim', { 'tag': '0.1.2' }
-- 'neoclide/coc.nvim', {'branch': 'release'}
-- 'nvim-treesitter/nvim-treesitter'
-- 'sharkdp/fd'
-- 'https://github.com/tom-anders/telescope-vim-bookmarks.nvim'
-- 'MattesGroeger/vim-bookmarks'

-- call plug#end()
-- ]]

-- Additional configurations
vim.cmd [[filetype plugin indent on]]
vim.cmd [[syntax enable]]

vim.g.vimtex_view_method = 'zathura'
vim.g.vimtex_view_general_viewer = 'okular'
vim.g.vimtex_view_general_options = '--unique file:@pdf\\#src:@line@tex'
vim.g.vimtex_compiler_method = 'latexrun'
vim.g.maplocalleader = ","

require('telescope').load_extension('vim_bookmarks')

require('lualine').setup {
    options = {
      icons_enabled = true,
      theme = 'iceberg_dark',
      component_separators = { left = '', right = ''},
      section_separators = { left = '', right = ''},
      disabled_filetypes = {
        statusline = {},
        winbar = {},
      },
      ignore_focus = {},
      always_divide_middle = true,
      globalstatus = false,
      refresh = {
        statusline = 1000,
        tabline = 1000,
        winbar = 1000,
      }
    },
    sections = {
      lualine_a = {'mode'},
      lualine_b = {'branch', 'diff', 'diagnostics'},
      lualine_c = {'filename'},
      lualine_x = {'encoding', 'fileformat', 'filetype'},
      lualine_y = {'progress'},
      lualine_z = {'location'}
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = {'filename'},
      lualine_x = {'location'},
      lualine_y = {},
      lualine_z = {}
    },
    tabline = {},
    winbar = {},
    inactive_winbar = {},
    extensions = {}
}

-- vim.opt.termguicolors = true
require("bufferline").setup{}

-- Neovide configurations
if vim.g.neovide then
  vim.opt.guifont = 'JetBrains Mono:h10'
  vim.g.neovide_transparency = 0.9
  vim.cmd("colorscheme kanagawa-dragon")
end

-- Devilvim Greeter
local db = require'dashboard'

db.setup({
    theme = 'doom',
    config = {
        header = {
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                                                                             ",
              "                ⠀ ⠀⣠⡾⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⡇⢀⣀⣠⠤⠴⠶⢶⣶⠤⡤⣄⡀⣸⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⣿⣿⣿⠊⠉⠁⠀⠀⠒⢶⡉⠻⠕⠇⢈⣩⣿⣿⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ⠀",
              "      ⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠉⠀⠀⠀⣾⣿⣿⠈⠻⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⠀⠀⠀⡰⠋⢸⡿⠛⢋⣉⣩⠉⣀⢤⣭⡉⠉⠐⠤⣀⠀⢸⣿⣿⣿⢣⠀⠀⠙⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⠀⢠⠞⢀⡔⠃⠀⠚⣭⡴⠂⠀⠀⢻⣬⣝⠳⣤⠀⠀⢳⣟⢿⣿⣿⠈⢣⠀⠀⠈⢳⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⠀⢠⠏⡰⠋⠀⠀⢀⡼⠋⠀⠀⡞⠀⠀⠹⣎⠱⣌⠳⣄⠀⢷⠱⡹⣿⠀⠘⠃⠀⠀⠈⢷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⢀⢏⡎⢠⡾⠀⢀⣾⠁⠀⠀⠀⡇⠀⠀⠀⢻⡄⢻⠳⡈⠃⢸⡇⠸⣽⡀⠀⢠⠀⠀⠀⠸⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⡜⡸⠀⡾⠁⠀⢨⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⡆⠈⡌⢏⢆⠈⣿⠀⢰⣣⠀⢸⠀⠀⠀⠀⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⢧⠇⢸⠇⠀⡰⣿⠀⠀⠀⠀⠀⣇⠀⢀⠀⠀⠘⠀⢣⠈⢎⢣⡏⡇⠀⠏⡇⠘⣇⠀⠀⠀⢸⣇⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⡾⠀⡾⢀⡜⠁⢻⢰⠀⠀⠀⠀⣿⠀⢸⣆⠀⠀⠀⠘⠀⠈⡆⡇⡇⠀⠸⢰⠀⣿⠀⠀⠀⢸⢸⠀⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⢠⡇⢠⢇⣎⠀⠢⣸⠏⡆⠀⠀⠀⢸⡆⢸⠻⣦⠀⠀⢀⡇⠀⠸⡇⢻⡆⠀⠘⣆⣻⠀⠀⠀⢸⠀⡆⠀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⢸⠁⠈⣼⠾⠗⣶⣿⡼⣴⠀⠀⠀⠈⣧⢸⠀⠈⢷⠀⠈⢷⠀⠀⠁⠀⢻⡄⠀⣧⢿⠀⠀⠀⢸⠀⣷⠀⠀⠀⠀D E V I L      ⠀⠀⠀",
              "      ⠀⠀⢸⠀⢸⢻⣇⠸⣿⣺⣧⢹⣣⠀⠀⠀⢻⢸⠀⣀⣀⢑⣄⡘⣷⡀⠀⠰⡄⢳⡄⢈⢿⠀⠀⠀⡟⡇⢸⠀⠀⠀⠀V I M⠀⠀⠀          ",
              "      ⠀⠀⢸⠀⠸⡎⠸⠳⠼⠿⠉⢇⢻⢦⠀⠀⠘⣽⣰⡶⣶⣶⣤⣙⣯⣻⡇⢀⠈⠘⣿⣾⣝⠇⠀⠀⠇⢳⡜⡀⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⢸⠀⠀⡇⠀⠀⠀⠀⠀⡼⢆⢫⠁⠀⠀⠹⠁⠀⣇⣏⣓⣿⠿⣿⣿⠘⢻⣿⡎⢸⢿⢸⠀⢰⢀⠘⢷⡅⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠈⠀⠀⢁⠀⠀⠀⠀⡼⠁⠈⢣⣣⠀⠀⠀⡇⠛⢦⣍⣭⡥⠢⠀⢸⠚⠋⡭⢋⡬⢊⠎⠀⡘⡏⠀⣟⡇⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⠀⠀⡄⠀⢸⣆⠀⠀⠠⠄⣀⠀⠀⠹⣷⠀⠀⢻⠀⠀⠈⠀⠀⠀⠀⣸⠀⠀⢒⣠⣶⣿⠀⠀⡟⠇⠀⢹⣷⠀⠀⠀⠀⠀⠀               ",
              "      ⠀⢀⡴⡇⠀⠈⠙⢆⠀⢠⣄⣀⠀⠀⠀⢹⡆⠀⣸⠀⠀⠀⠀⠀⢀⣴⣿⠰⣶⣿⣿⣿⣿⠀⢠⢿⠀⠀⠈⣿⡆⠀⠀⠀⠀⠀               ",
              "      ⢠⠋⠀⠃⠀⢰⠀⠘⣆⠘⡏⠉⠙⢲⣶⡤⣿⢸⡏⠀⠀⢀⣠⣶⣿⣷⣿⢀⣟⢿⣿⣿⡏⠀⢸⣾⡄⠀⠀⢿⣧⠀⠀⠀⠀⠀               ",
              "      ⢿⠀⠀⢸⠀⢸⠀⠀⣿⣆⣇⠀⡤⠚⠋⠁⡿⢋⣡⣴⣾⣿⣿⣿⣿⣿⣿⢸⠋⣽⡿⣿⡇⠀⢸⣿⣿⡀⠀⠘⣿⠀⠀⠀⠀⠀               ",
              "      ⠘⣆⠀⢸⠀⢸⠀⠀⢻⡏⠻⣍⣀⣤⣤⣶⣿⣿⣿⣿⣿⣿⣯⣿⣿⣿⣿⢰⠺⣿⣿⣿⡇⠀⡟⠙⠿⣷⠀⠀⣿⡃⠀⠀⠀⠀               ",
              "      ⠀⠹⣆⠘⡆⣼⠀⠀⠈⢃⠀⠀⣰⣿⣇⠙⡟⣻⠿⣿⣿⣿⣿⢿⣿⣿⣿⠰⣟⣿⣿⣿⠃⢠⠇⠀⠀⠈⢧⡀⢸⣇⠀⠀⠀⠀               ",
              "      ⠀⠀⠹⡄⡇⣿⠀⠀⠀⢸⣀⣼⣿⣿⣿⣄⡟⣽⣧⠀⠉⠛⠻⠿⠿⡿⢿⠸⠛⠉⠁⠸⠀⡜⠀⢀⠖⠀⠀⠙⢦⣿⠀⠀⠀⠀               ",
              "      ⠀⠀⢀⣵⡇⡇⡇⠀⠀⢀⡋⠉⠉⣿⣷⣶⣾⢿⣿⣷⣀⠀⠀⢀⡜⠀⢸⠀⠀⠀⠀⡄⡸⠁⠀⣡⠆⠀⢀⣤⠀⠻⣧⠀⠀⠀               ",
              "                                                                             ",
              "                                                                             ",


-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",
-- "                                                                                                             ",              
-- "                              _                                                                              ",
-- "                             /_\\                                                                            ",
-- "                _            )_(            _                                                                ",
-- "                |`-.___,.-~'`|=|`'~-.,___,-'|                                                                ",
-- "                |  __________|=|__________  |                                                                ",
-- "                | |    ______|=|__________| |                                                                ",
-- "                | |   |  ____|=|_____     / |                                                                ",
-- "                | |   | /    |=|    /    /| |                                                                ",
-- "                | |   |/   ,-|_|-. / /  /_|_|______ ______     _______        ____                           ",
-- "           | |      ,' _____ / // / \\    ___  |\\    /     \\      `.      \\   \\                     ",
-- "             | |     / ,'| A |/ // /   |  |   \\ | |  |       |  |`.  \\     /    \\                      ",
-- "           | |    /_// |/V\\/ // /    |  |    \\| |  |       |  |  \\  \\   /  /\\  \\                  ",
-- "              | |      /__| |/  / /     |  |       |  |       |  |   \\  | /  /  \\  \\                    ",
-- "               | |     /\\  | / /| /\\     |  |__/|   |  |       |  |   |  ||  |    |  |                    ",
-- "               | |    /  \\ |/ // // \\    |   __ |   |  |       |  |   |  ||  |____|  |                    ",
-- "              | |   /    \\/ |/ //   \\   |  |  \\|   |  |       |  |   |  ||   ____   |                   ",
-- "               | |  /     /    //     \\  |  |       |  |       |  |   /  ||  |    |  |                     ",
-- "               | | /     / /  /|       \\ |  |    /| |  |    /| |  |  /  / |  |    |  |                     ",
-- "               | |/_____/ // / |________\\|  |___/ | |  |___/ | |  |,'  /  |  |    |  |                     ",
-- "               | |     / // /| |        /_________|/_________|/______,'  /____\\  /____\\                   ",
-- "              \\ \\    / // / | |       /|/ /                                N E O V I M                   ",
-- "               \\ \\  /  / /| | |______/ | /                                                               ",
-- "                \\ \\/______| | |________|/                                                                ",
-- "                   `.`.     | | |     ,','                                                                   ",
-- "                     `.`.   | | |   ,','                                                                     ",
-- "                         `-.| | |,-'                                                                         ",                                                             
-- "                            | | |                                                                            ",
-- "                            | | |                                                                            ",
-- "                            \\|/                                                                            ",
-- "                              V                                                                              ",
-- "                                                                                                            ",


            -- "                                                                              ",
            -- "=================     ===============     ===============   ========  ========",
            -- "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //",
            -- "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||",
            -- "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||",
            -- "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||",
            -- "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||",
            -- "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||",
            -- "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||",
            -- "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||",
            -- "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||",
            -- "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||",
            -- "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||",
            -- "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||",
            -- "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||",
            -- "||   .=='    _-'          `-__\\._-'         `-_./__-'         `' |. /|  |   ||",
            -- "||.=='    _-'                                                     `' |  /==.||",
            -- "=='    _-'                        N E O V I M                         \\/   `==",
            -- "\\   _-'                                                                `-_   /",
            -- " `''                                                                      ``'  ",
          },
      center = {
        {
          icon = ' ',
          icon_hl = 'Title',
          desc = 'Find File           ',
          desc_hl = 'String',
          key = 'b',
          keymap = 'SPC f f',
          key_hl = 'Number',
          action = 'lua print(2)'
        },
        {
          icon = ' ',
          desc = 'Find Dotfiles',
          key = 'f',
          keymap = 'SPC f d',
          action = 'lua print(3)'
        },
      },
      footer = {}  --your footer
    }
  })

-- NVIM Tree
-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- empty setup using defaults
-- require("nvim-tree").setup()

-- OR setup with some options
require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    width = 30,
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = false,
  },
})
