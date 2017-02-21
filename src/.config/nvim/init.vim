" DIRECTORIES
let CONF_DIR = "~/.config/nvim/"
let UNDO_DIR = CONF_DIR . "undo/"
let BUNDLE_DIR = CONF_DIR . "bundle/"

# CONFIGURATION FILES
let PLUGINS_FILEPATH = CONF_DIR . "plugins.vim"
let KEYMAPS_FILEPATH = CONF_DIR . "keymaps.vim"
let SETTINGS_FILEPATH = CONF_DIR . "settings.vim"
let FUNCTIONS_FILEPATH = CONF_DIR . "functions.vim"
let VUNDLE_FILEPATH = BUNDLE_DIR . "Vundle.vim"

if &compatible
	set nocompatible
endif

let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

" Required:
filetype plugin indent on
syntax enable
syntax on

let &undodir=UNDO_DIR
set undofile
set undolevels=1000

set title
"set history=500                 " remember more commands and search history
"set wildignore=*.swp,*.bak,*.pyc,*.class

set scroll=10
set autoread " reread files when changed from outside

set so=10 " set 10 lines to the cursor - when moving vertical

" Folding
set foldmethod=syntax
set foldnestmax=99

" show line number/relative
set number
set relativenumber
" activate mouse usage
set mouse=a

" =============== Homemade functions ===============
if filereadable (expand (FUNCTIONS_FILEPATH))
	execute "source" FUNCTIONS_FILEPATH
endif

augroup my_python
	autocmd BufWritePre *.py call StripTrailingWhitespaces()
	autocmd BufWritePre, BufRead *.py call ReindentFile()
augroup END

" =============== Plugin Manager ===============
if filereadable (expand (PLUGINS_FILEPATH))
	execute "source" PLUGINS_FILEPATH
endif

" =============== Plugin Settings ===============
if filereadable (expand (SETTINGS_FILEPATH))
	execute "source" SETTINGS_FILEPATH
endif

" =============== Key Mappings ===============
if filereadable (expand (KEYMAPS_FILEPATH))
	execute "source" KEYMAPS_FILEPATH
endif
