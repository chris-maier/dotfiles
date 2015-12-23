" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin('~/.vim/bundle')
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'flazz/vim-colorschemes'

" === File Finder ===
Plugin 'kien/ctrlp.vim'
"Plugin 'wincent/command-t'
"Plugin 'vim-scripts/FuzzyFinder'
" ===================

Plugin 'kien/rainbow_parentheses.vim'
Plugin 'bling/vim-airline'
Plugin 'scrooloose/nerdtree'

""" NEW
Plugin 'ervandew/supertab'


" === Snippets Engine ===
Plugin 'MarcWeber/vim-addon-mw-utils'   " dep of snipmate
Plugin 'tomtom/tlib_vim'                " dep of snipmate
"Plugin 'SirVer/ultisnips'   "python, supports all snippets in this repo.
Plugin 'garbas/vim-snipmate' " VimL, snipmate-snippets, engine sometimes behaves strange. Supports snippets/*
"Plugin 'Shougo/neosnippet'  " VimL, supports snippets/* with some configuration.
Plugin 'honza/vim-snippets'  " a whole bunch of snippets
" ===================
"
"Plugin 'tpope/vim-surround'
"Plugin 'tmhedberg/matchit'
"Plugin 'yegappan/mru'
"Plugin 'blarghmatey/split-expander'
"Plugin 'rking/ag.vim'

Plugin 'scrooloose/nerdcommenter'
Plugin 'majutsushi/tagbar'
Plugin 'vim-scripts/a.vim'

"Plugin 'Shougo/neocomplete.vim'
"Plugin 'Shougo/neosnippet.vim'
"Plugin 'Shougo/neosnippet-snippets'
"Plugin 'SirVer/ultisnips'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
"Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
"Plugin 'L9'
" Git plugin not hosted on GitHub
"Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
"Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
"Plugin 'user/L9', {'name': 'newL9'}

" All of your Plugins must be added before the following line
call vundle#end()            " required
