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
syntax on
syntax enable
filetype plugin indent on

exe 'set undodir=' . g:undo_dir
set undofile
set undolevels=10000

" remember more commands and search history
set history=1000

" reread files when changed from outside
set autoread

" set 10 lines to the cursor - when moving vertical
set so=10

" Folding
set foldmethod=syntax
set foldnestmax=99
set foldlevel=99

" show line number/relative
set number
set relativenumber
" activate mouse usage
set mouse=a

" move over line endings
set whichwrap+=<,>,h,l,[,]

" =============== Homemade functions ===============
if filereadable (expand (g:functions_file))
	execute "source" g:functions_file
endif

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

augroup my_python
	autocmd BufWritePre *.py call StripTrailingWhitespaces()
	autocmd BufWritePre, BufRead *.py call ReindentFile()
augroup END

augroup my_vim
	autocmd BufWritePre *.vim call StripTrailingWhitespaces()
	autocmd BufWritePre, BufRead *.vim call ReindentFile()
augroup END

augroup my_org
	autocmd BufEnter *.org set ft=org
augroup END
