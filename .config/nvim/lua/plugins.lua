return require('packer').startup(function()
     use 'wbthomason/packer.nvim'
     use 'pineapplegiant/spaceduck'
     use {
        'windwp/nvim-autopairs',
        config = function()
            require('nvim-autopairs').setup()
        end,
    } 
    use 'jxnblk/vim-mdx-js' -- Syntax highlighting for mdx, I have to use mdx quite often for my blog (youssefbouzekri.vercel.app)
    use 'tpope/vim-surround' -- should be part of vim IMO
    use 'junegunn/goyo.vim' -- sometimes all you need to get in the mood is a clean view, try :Goyo
    use 'tpope/vim-commentary' -- toggle comments with <leader>c
    use 'ap/vim-css-color' -- colors RGB and Hex codes
    use 'tpope/vim-sleuth' -- allows you to remove all settings related to tabs and indentation
    use 'rhysd/clever-f.vim' -- extend vim's f and t functionality
    use {
          'hoob3rt/lualine.nvim',
          requires = {'kyazdani42/nvim-web-devicons', opt = true},
          config =  function()
              require('lualine').setup {
                 options = {
                    theme = 'spaceduck',
                    section_separators = {'', ''},
                    icons_enabled = false
                 }
              }
          end,
    }
    use {
        'kyazdani42/nvim-tree.lua', -- experimenting with this as an alternative to NERDTree
        requires = 'kyazdani42/nvim-web-devicons'
    }
end)

