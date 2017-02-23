" Setting up Vundle - The Vim Plugin Bundler
let vundleAlreadyExists=1
let vundle_readme=g:vundle_dir . "/README.md"

if !filereadable(vundle_readme)
    echo "Installing Vundle..."
    if isdirectory(g:bundle_dir) == 0
        call mkdir(g:bundle_dir, 'p')
    endif
    execute 'silent !git clone https://github.com/VundleVim/Vundle.vim "' . g:vundle_dir . '"'
    let vundleAlreadyExists=0
endif

" set the runtime path to include Vundle and initialize
exe 'set rtp+=' . g:vundle_dir

call vundle#begin(g:bundle_dir)
	" let Vundle manage Vundle, required
	Plugin 'gmarik/Vundle.vim'

	" === Style ============
	Plugin 'flazz/vim-colorschemes'
	Plugin 'kien/rainbow_parentheses.vim'
	Plugin 'bling/vim-airline'
	" ======================

	" === Search Engines ===
	Plugin 'Shougo/neoyank.vim'
	Plugin 'Shougo/neomru.vim'
	Plugin 'Shougo/unite.vim'
	Plugin 'tsukkee/unite-tag'
	Plugin 'thinca/vim-unite-history'
	Plugin 'mileszs/ack.vim'
	" ======================

	" === File tree Support ===
	Plugin 'scrooloose/nerdtree'
	" Git support in file tree
	Plugin 'Xuyuanp/nerdtree-git-plugin'
	" =========================

	" === Completion Engine ===
	Plugin 'Shougo/deoplete.nvim'
	Plugin 'zchee/deoplete-jedi'
	Plugin 'zchee/deoplete-clang'
	Plugin 'Shougo/neco-vim'
	Plugin 'Shougo/neoinclude.vim'
	Plugin 'Shougo/neosnippet.vim'
	Plugin 'Shougo/neosnippet-snippets'
	Plugin 'honza/vim-snippets'
	" =========================
	"
	Plugin 'tpope/vim-repeat'
	Plugin 'tpope/vim-speeddating'
	Plugin 'jceb/vim-orgmode'

	" === Commenter ===
	Plugin 'scrooloose/nerdcommenter'
	" =================
	"
	" === GIT ===
	Plugin 'tpope/vim-fugitive'
	" =================
	
	""" NEW
	" Plugin 'ervandew/supertab'

	" === Snippets Engine ===
	" Plugin 'MarcWeber/vim-addon-mw-utils'   " dep of snipmate
	" Plugin 'tomtom/tlib_vim'                " dep of snipmate
	"Plugin 'SirVer/ultisnips'   "python, supports all snippets in this repo.
	" Plugin 'garbas/vim-snipmate' " VimL, snipmate-snippets, engine sometimes behaves strange. Supports snippets/*
	" ===================
	"
	" === Try later ===
	"  call dein#add('othree/es.next.syntax.vim', {'on_ft': 'javascript'})
	"  call dein#add('elzr/vim-json', {'on_ft': 'json'})
	"  call dein#add('tpope/vim-markdown', {'on_ft': 'markdown'})
	"  call dein#add('tmhedberg/SimpylFold', {'on_ft': 'python'})
	"  call dein#add('HerringtonDarkholme/yats.vim', {'on_ft': 'typescript'})
	"  call dein#add('Quramy/tsuquyomi', {'on_ft': 'typescript'})
	"
	"  call dein#add('Yggdroot/indentLine')
	"  call dein#add('Raimondi/delimitMate', {'on_ft': ['javascript', 'typescript', 'css', 'scss']})
	"  call dein#add('valloric/MatchTagAlways', {'on_ft': 'html'})
	"
	"  call dein#add('tpope/vim-fugitive')
	"  call dein#add('jreybert/vimagit')
	"  call dein#add('mhinz/vim-signify')
	"  call dein#add('tpope/vim-repeat')
	"  call dein#add('benekastah/neomake')
	"  call dein#add('editorconfig/editorconfig-vim')
	"  call dein#add('AndrewRadev/switch.vim')
	"  call dein#add('tpope/vim-surround')
	"  call dein#add('tomtom/tcomment_vim')
	"  call dein#add('mattn/emmet-vim', {'on_ft': 'html'})
	"  call dein#add('Chiel92/vim-autoformat')
	"  call dein#add('Shougo/unite-outline')
	"  call dein#add('ujihisa/unite-colorscheme')
	"  call dein#add('junkblocker/unite-codesearch')
	"  call dein#add('Shougo/vimproc.vim', {'build' : 'make'})
	"  call dein#add('ujihisa/neco-look')
	"  call dein#add('mhinz/vim-sayonara')
	"  call dein#add('junegunn/goyo.vim')
	"  call dein#add('vim-scripts/SyntaxRange')
	"
	"
	"Plugin 'tpope/vim-surround'
	"Plugin 'tmhedberg/matchit'
	"Plugin 'yegappan/mru'
	"Plugin 'blarghmatey/split-expander'
	"
	"Plugin 'scrooloose/nerdcommenter'
	"Plugin 'majutsushi/tagbar'
	"Plugin 'vim-scripts/a.vim'

	"Plugin 'Shougo/neocomplete.vim'
	"Plugin 'Shougo/neosnippet.vim'
	"Plugin 'Shougo/neosnippet-snippets'
	"Plugin 'SirVer/ultisnips'

	" Installing plugins the first time
	if vundleAlreadyExists == 0
	    " Executing below line Causes Vim to crash.
	    " The bug has been reported @ https://github.com/gmarik/vundle/issues/275
	    execute 'PluginInstall'
	endif

" All of your Plugins must be added before the following line
call vundle#end()            " required
