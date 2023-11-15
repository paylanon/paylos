-- |) [- \/ | |_ \/ | |\/| Configuration File

-- KEYMAPS
vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set({'n', 'i'}, "<C-+>", function() ResizeGuiFont(1)  end)
vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeGuiFont(-1) end)
vim.keymap.set({'n', 'i'}, "<C-BS>", function() ResetGuiFont() end)
-- LuaSnip keymaps
vim.cmd[[
    " Expand or jump in insert mode
    imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
    " Jump forward through tabstops in visual mode
    smap <silent><expr> <Tab> luasnip#jumpable(1) ? '<Plug>luasnip-jump-next' : '<Tab>'
    " Jump backward through snippet tabstops with Shift-Tab (for example)
    imap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
    smap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
    " Cycle forward through choice nodes with Control-f (for example)
    imap <silent><expr> <C-f> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-f>'
    smap <silent><expr> <C-f> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-f>'
]]

-- GENERAL CONFIG
vim.opt.termguicolors = true
-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
-- Configure clipboard:
vim.opt.clipboard = "unnamedplus"
-- Configure Merlin (OCaml):
vim.cmd([[set nocompatible]])
vim.cmd([[syntax on]])
vim.cmd([[filetype plugin on]])
vim.cmd([[set rtp+=/home/alsuwaidi/.opam/default/share/merlin/vim]])
-- Additional configurations
vim.cmd [[filetype plugin indent on]]
vim.cmd [[syntax enable]]
vim.cmd [[set number]]

-- LAZY.NVIM
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

-- -- PLUGIN LIST --
local plugins = {
'junegunn/vim-easy-align',
'https://github.com/junegunn/vim-github-dashboard.git',
'SirVer/ultisnips',
'honza/vim-snippets',
'tpope/vim-fireplace',
'rdnetto/YCM-Generator',
'fatih/vim-go',
'nsf/gocode',
'junegunn/fzf',
{
    'lervag/vimtex',
    config = function()
      vim.g.vimtex_view_method = 'zathura'
      vim.g.vimtex_view_general_viewer = 'okular'
      vim.g.vimtex_view_general_options = [[--unique file:@pdf\#src:@line@tex]]
      vim.g.vimtex_compiler_method = 'latexmk'
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_syntax_enabled = 1
      vim.g.vimtex_quickfix_mode = 1
      vim.g.maplocalleader = ","
      vim.g.zenbones_compat = 1
      vim.cmd("colorscheme zenbones")
    end,
    ft = 'tex'
},
{
    'https://github.com/rebelot/kanagawa.nvim',
    lazy = false
},
{
    'mcchrish/zenbones.nvim',
    lazy = false
},
'nvim-lualine/lualine.nvim',
'nvim-tree/nvim-web-devicons',
'https://github.com/akinsho/bufferline.nvim',
'glepnir/dashboard-nvim',
'nvim-tree/nvim-tree.lua',
'folke/which-key.nvim',
'nvim-lua/plenary.nvim',
'nvim-telescope/telescope.nvim',
'nvim-treesitter/nvim-treesitter',
'sharkdp/fd',
'https://github.com/tom-anders/telescope-vim-bookmarks.nvim',
'MattesGroeger/vim-bookmarks',
'numToStr/Comment.nvim',
'LhKipp/nvim-nu',
{
    'sitiom/nvim-numbertoggle',
    lazy = false
},
'nvim-lua/plenary.nvim',
'ThePrimeagen/vim-be-good',
'ThePrimeagen/harpoon',
'williamboman/mason.nvim',
'williamboman/mason-lspconfig.nvim',
'neovim/nvim-lspconfig',
'simrat39/rust-tools.nvim',
'hrsh7th/nvim-cmp',
'hrsh7th/cmp-nvim-lsp',
'hrsh7th/cmp-nvim-lua',
'hrsh7th/cmp-nvim-lsp-signature-help',
'hrsh7th/cmp-vsnip',                         
'hrsh7th/cmp-path',                         
'hrsh7th/cmp-buffer',                            
'L3MON4D3/LuaSnip'
}

-- -- PLUGIN SETUP () --
require("lazy").setup(plugins, {defaults = {lazy = true}})
require('Comment').setup()
require('telescope').load_extension('vim_bookmarks')
require("bufferline").setup{}
require('nu').setup{
	use_lsp_features = false,
}
require("mason").setup({
    ui = {
        icons = {
            package_installed = "",
            package_pending = "",
            package_uninstalled = "",
        },
    }
})
require("mason-lspconfig").setup()
require("nvim-tree").setup()
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
require("luasnip").config.set_config({ -- Setting LuaSnip config
  -- Enable autotriggered snippets
  enable_autosnippets = true,
  -- Use Tab (or some other key if you prefer) to trigger visual selection
  store_selection_keys = "<Tab>",
})

-- (RUST TOOLS)
local rt = require("rust-tools")
rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})
-- -- LSP Diagnostics --
local sign = function(opts)
  vim.fn.sign_define(opts.name, {
    texthl = opts.name,
    text = opts.text,
    numhl = ''
  })
end
sign({name = 'DiagnosticSignError', text = ''})
sign({name = 'DiagnosticSignWarn', text = ''})
sign({name = 'DiagnosticSignHint', text = ''})
sign({name = 'DiagnosticSignInfo', text = ''})
vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    update_in_insert = true,
    underline = true,
    severity_sort = false,
    float = {
        border = 'rounded',
        source = 'always',
        header = '',
        prefix = '',
    },
})
vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])
-- -- Completeopt --
vim.opt.completeopt = {'menuone', 'noselect', 'noinsert'}
vim.opt.shortmess = vim.opt.shortmess + { c = true}
vim.api.nvim_set_option('updatetime', 300) 
vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])
local cmp = require'cmp'
cmp.setup({
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-S-f>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },
  sources = {
    { name = 'path' },
    { name = 'nvim_lsp', keyword_length = 3 },
    { name = 'nvim_lsp_signature_help'},
    { name = 'nvim_lua', keyword_length = 2},
    { name = 'buffer', keyword_length = 2 },
    { name = 'vsnip', keyword_length = 2 },
    { name = 'calc'},
  },
  window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
  },
  formatting = {
      fields = {'menu', 'abbr', 'kind'},
      format = function(entry, item)
          local menu_icon ={
              nvim_lsp = 'λ',
              vsnip = '⋗',
              buffer = 'Ω',
              path = '🖫',
          }
          item.menu = menu_icon[entry.source.name]
          return item
      end,
  },
})

-- (TREESITTER)
-- -- Plugins --
require('nvim-treesitter.configs').setup {
  ensure_installed = { "lua", "rust", "toml" },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting=false,
  },
  ident = { enable = true }, 
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = nil,
  }
}
-- -- Folding --
vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldenable = false
vim.treesitter.language.register("markdown", "mdx")

-- (DASHBOARD)
-- -- Greeter --
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
-- Previous Version Greeters: Zelda Neovim, Doom Neovim

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
      footer = {'©2023 PAYLHORSE'}  --your footer
    }
  })

-- NEOVIDE CONFIG
if vim.g.neovide then
  vim.opt.guifont = 'Sarasa Mono CL Nerd Font:h11.5'
  -- vim.g.neovide_transparency = 0.9
  vim.cmd("colorscheme kanagawa-dragon")
  -- Zoom in and out
  vim.api.nvim_set_keymap("n", "<C-+>", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>", { silent = true })
  vim.api.nvim_set_keymap("n", "<C-->", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>", { silent = true })
  vim.api.nvim_set_keymap("n", "<C-0>", ":lua vim.g.neovide_scale_factor = 1<CR>", { silent = true })
end
