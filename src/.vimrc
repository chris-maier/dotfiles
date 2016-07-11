" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Enable filetype plugins
filetype plugin indent on
set omnifunc=syntaxcomplete#Complete
" Enable syntax highlighting
syntax enable

" =============== Vundle Initialization ===============
" This loads all the plugins specified in ~/.vim/vundles.vim
" Use Vundle plugin to manage all other plugins
if filereadable(expand("~/.vim/vundles.vim"))
  source ~/.vim/vundles.vim
endif

" =============== Key Mappings =============== 
if filereadable (expand ("~/.vim/keymaps.vim"))
    source ~/.vim/keymaps.vim
endif

" =============== Plugin Settings ===============
" Plugin configuration 
if filereadable (expand ("~/.vim/plugins.vim"))
    source ~/.vim/plugins.vim
endif

if has('win32') || has('win64')
    so $HOME/.vim/windows.vim
else
    so $HOME/.vim/linux.vim
endif

" =============== Tools Settings ===============
if filereadable (expand ("~/.vim/tools.vim"))
    so $HOME/.vim/tools.vim
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=700
" Set to auto read when a file is changed from the outside
set autoread

set grepprg=grep
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 10 lines to the cursor - when moving vertically using j/k
set so=10

" Turn on the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o

"Always show current position
set ruler
" Zeilennummer anzeigen
set number

" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases 
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" System clipboard verwenden
set clipboard=unnamed

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions+=e
    set t_Co=256
    set guitablabel=%M\ %t
    set guifont=Source\ Code\ Pro\ Medium\ 11
endif

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
" """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git anyway...
set nobackup
set nowb
set noswapfile
set undolevels=1000             " use many muchos levels of undo

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Linebreak on 200 characters
set lbr
set tw=120

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Specify the behavior when switching between buffers 
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

" Remember info about open buffers on close
set viminfo^=%

""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
set laststatus=1
set statusline="%&lt;%f\ %m%a%=%([%R%H%Y]%)\ %-19(%3l\ of\ %L,%c%)%P"
set showcmd

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => AutoCommand on file actions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif
" Call CTAGS on C/C++ read
autocmd BufReadPost *.h,*.hpp,*.c,*.cpp,*.h++,*.c++ :call GenCtags()
" Call ASTYLE on C/C++ pre-write
autocmd BufReadPost *.h,*.hpp,*.c,*.cpp,*.h++,*.c++ :call ReformatCFiles()
" Strip trailing whitespace on pre-write
autocmd BufWritePre *.h,*.hpp,*.c,*.cpp,*.h++,*.c++,*.tex,*.py :call StripTrailingWhitespaces()
" set the fileencoding to utf-8 before write to a source file
autocmd BufReadPost *.h,*.hpp,*.c,*.cpp,*.c++ :set fileencoding=utf-8
" Save on focus lost
autocmd FocusLost * :wa 

" Start AutoClose on source files
"autocmd BufWritePre *.tex,*.c,*.py,*.js,*.html,*.php,*.wiki,*.cpp,*.h :AutoCloseOn
