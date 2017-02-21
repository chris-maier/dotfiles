
" Remember cursor position between vim sessions
  autocmd BufReadPost *
              \ if line("'\"") > 0 && line ("'\"") <= line("$") |
              \   exe "normal! g'\"" |
              \ endif
              " center buffer around cursor when opening files
  autocmd BufRead * normal zz


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM-Colorschemes
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme triplejelly
"colorscheme zenburn
"set background=dark

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => UNITE 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_prompt = '» '
let g:unite_source_history_yank_enable = 1

call unite#custom#profile('default', 'context', {
			\   'start_insert': 1,
			\   'winheight': 10,
			\   'direction': 'botright',
                        \   'smartcase': 1,
                        \   'ignorecase': 1,
			\ })
if executable('ag')
	" use AG instead of grep
	let g:unite_source_grep_command = 'ag'
	let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden'
	let g:unite_source_grep_recursive_opt=''
endif

" Ctrl-P
nnoremap <C-t> :Unite -start-insert -no-split buffer file_rec/neovim <CR>

" Yank history - DISABLED
"nnoremap <C-y> :Unite history/yank<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rainbow Parentheses
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:rbpt_colorpairs = [
     \ ['brown',       'RoyalBlue3'],
     \ ['Darkblue',    'SeaGreen3'],
     \ ['darkgray',    'DarkOrchid3'],
     \ ['darkgreen',   'firebrick3'],
     \ ['darkcyan',    'RoyalBlue3'],
     \ ['darkred',     'SeaGreen3'],
     \ ['darkmagenta', 'DarkOrchid3'],
     \ ['brown',       'firebrick3'],
     \ ['gray',        'RoyalBlue3'],
     \ ['black',       'SeaGreen3'],
     \ ['darkmagenta', 'DarkOrchid3'],
     \ ['Darkblue',    'firebrick3'],
     \ ['darkgreen',   'RoyalBlue3'],
     \ ['darkcyan',    'SeaGreen3'],
     \ ['darkred',     'DarkOrchid3'],
     \ ['red',         'firebrick3'],
     \ ]
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ack
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if executable('ag')
	" Use Ag over Ac
	let g:ackprg = 'ag'
	" Use Ag over Grep
	set grepprg=ag\ --nogroup\ --nocolor
else
	echoerr "Silver-Searcher AG is not installed"
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Airline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = '>'
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_detect_whitespace = 1 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERDTree 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDTreeShowHidden = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OMNIFUNCS 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup omnifuncs
  "autocmd!
  "autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  "autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  "autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  "autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  "autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"augroup end

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" DEOPLETE *MATCHING WORKS - KEYMAPS needs update*
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call deoplete#custom#set('_', 'matchers', ['matcher_full_fuzzy'])
let g:deoplete#sources = {}
"let g:deoplete#sources.py = ['buffer', 'tag', 'jedi']
let g:deoplete#sources.py = ['jedi']
let g:deoplete#enable_at_startup = 1

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" DEOPLETE-CLANG *UNTESTED*
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#sources#clang#libclang_path = '/usr/lib/llvm-3.6/lib/libclang.so'
let g:deoplete#sources.cpp = ['buffer', 'tag']
let g:deoplete#sources#clang#clang_header = '/usr/lib/llvm-3.6/include/clang'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" DEOPLETE-JEDI *UNTESTED*
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#sources#jedi#show_docstring = 1
let g:deoplete#sources#jedi#python_path = '/usr/bin/python'

" automatically closing the scratch window at the top
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SNIPPETS 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable snipMate compatibility feature.
"let g:neosnippet#enable_snipmate_compatibility = 1
"imap <C-k>     <Plug>(neosnippet_expand_or_jump)
"smap <C-k>     <Plug>(neosnippet_expand_or_jump)
"xmap <C-k>     <Plug>(neosnippet_expand_target)
" Tell Neosnippet about the other snippets
"let g:neosnippet#snippets_directory='~/.vim/bundle/neosnippet-snippets/neosnippets, ~/Github/ionic-snippets, ~/.vim/bundle/angular-vim-snippets/snippets'

" SuperTab like snippets behavior.
"imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
"\ "\<Plug>(neosnippet_expand_or_jump)"
"\: pumvisible() ? "\<C-n>" : "\<TAB>"
"smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
"\ "\<Plug>(neosnippet_expand_or_jump)"
"\: "\<TAB>"

"imap <expr><silent><CR> pumvisible() ? deoplete#mappings#close_popup() .
      "\ "\<Plug>(neosnippet_jump_or_expand)" : "\<CR>"
"smap <silent><CR> <Plug>(neosnippet_jump_or_expand)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SuperTAB 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:SuperTabDefaultCompletionType = "<c-n>"
