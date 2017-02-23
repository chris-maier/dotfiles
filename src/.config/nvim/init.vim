" DIRECTORIES
let g:conf_dir = expand("~/.config/nvim/")
let g:undo_dir = g:conf_dir . "undo/"
let g:bundle_dir = g:conf_dir . "bundle/"
let g:vundle_dir = g:bundle_dir . "Vundle.vim"

" CONFIGURATION FILES
let g:plugins_file = g:conf_dir . "plugins.vim"
let g:keymaps_file = g:conf_dir . "keymaps.vim"
let g:settings_file = g:conf_dir . "settings.vim"
let g:functions_file = g:conf_dir . "functions.vim"

if &compatible
	set nocompatible
endif

let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

" Required:
filetype plugin indent on
syntax enable
syntax on

exe 'set undodir=' . g:undo_dir
set undofile
set undolevels=10000

set title
set history=500                 " remember more commands and search history
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
if filereadable (expand (g:functions_file))
	execute "source" g:functions_file 
endif

augroup my_python
	autocmd BufWritePre *.py call StripTrailingWhitespaces()
	autocmd BufWritePre, BufRead *.py call ReindentFile()
augroup END

" =============== Plugin Manager ===============
if filereadable (expand (g:plugins_file))
	execute "source" g:plugins_file 
endif

" =============== Plugin Settings ===============
if filereadable (expand (g:settings_file))
	execute "source" g:settings_file 
endif

" =============== Key Mappings ===============
if filereadable (expand (g:keymaps_file))
	execute "source" g:keymaps_file 
endif
