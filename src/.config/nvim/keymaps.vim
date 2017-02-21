"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" KEY MAPPINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

" Fast saving
map <leader>w :update<cr>
map <leader>q :wq!<cr>

" fast closing
"nmap <C-q> :q!<cr>

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" map ESC  
imap jk <Esc>
imap kj <Esc>

" Align blocks and keep them selected 
vmap < <gv
vmap > >gv

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ‘d is for delete’ & ‘ leader-d is for cut’ (shared clipboard register mode)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap x "_x
nnoremap X "_X
nnoremap d "_d
nnoremap D "_D
vnoremap d "_d

if has('unnamedplus')
" use system clipboard - using xsel with nvim
  set clipboard=unnamed,unnamedplus
  nnoremap <leader>d "+d
  nnoremap <leader>D "+D
  vnoremap <leader>d "+d
else
  set clipboard=unnamed
  nnoremap <leader>d "*d
  nnoremap <leader>D "*D
  vnoremap <leader>d "*d
endif

" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><space> :noh<cr>

" Move lines up and downwards
map <M-j> :m+<CR>==
map <M-k> :m-2<CR>==
imap <M-j> <Esc>:m+<CR>==gi
imap <M-k> <Esc>:m-2<CR>==gi
vnoremap <M-j> :m'>+<CR>gv=gv
vnoremap <M-k> :m-2<CR>gv=gv

" move cursor faster
map  <C-k>	5<Up>
imap <C-k>  	<Up><Up><Up><Up><Up>
map  <C-j>	5<Down>
imap <C-j>  	<Down><Down><Down><Down><Down>
map <C-l> W
imap <C-l> <C-Right>
map <C-h> B
imap <C-h> <C-Left>

" Delete whole words 
"imap <C-BS> <C-W>
imap <C-DEL> <C-o>dw
map  <C-Del> dw

" move cursor to other screen
nnoremap <leader>, <C-W><C-W>

" switch between header and source file using a.vim script
map <C-Tab> :AV<cr>

" Useful mappings for managing tabs
map <leader>t :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove

" split screens
map <leader>sp :sp
map <leader>vs :100vs

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
"map <leader>cd :cd %:p:h<cr>:pwd<cr>
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Quickly edit/reload the vimrc file
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <leader>sv :so $MYVIMRC<CR>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FUNCTION KEY MAPPINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" execute a Ninja build
map <F2> :call Build() <CR>

" generate ctags
map <F3> :call GenCtags () <CR> 

" GREP OPTIONS ignore case
noremap <F4> :call GrepVisualOperator(mode(), "i")<RETURN>
vnoremap <F4> :<c-u>call GrepVisualOperator(mode(), "i") <RETURN>
map <F6> :TagbarToggle <CR>
" toggle taglist plugin
map <F7> :NERDTreeToggle <CR>
" show list of matching tags
map <F8> :tn <CR>
" HEX MODE
map <F9> :Hexmode<CR>
map <F10> :mksession!<CR> " Quick write session with F10
map <F11> :source Session.vim<CR>     " And load session with F11
map <F12> :call ReformatCFiles () <CR>
