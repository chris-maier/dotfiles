"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM-Colorschemes
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme zmrok
"colorscheme zenburn
set background=dark

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CTRLP 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = '<C-t>'
let g:ctrlp_cmd = 'tabnew \| CtrlP ' .expand("%:p")
" search by full path
let g:ctrlp_by_filename = 1
"Set this to 0 to show the match window at the top of the screen: >
let g:ctrlp_match_window_bottom = 0
"Set the maximum height of the match window: >
" Enable/Disable per-session caching: >
let g:ctrlp_use_caching = 1

let g:ctrlp_max_height = 25
let g:ctrlp_clear_cache_on_exit = 1
let g:ctrlp_custom_ignore = {
\ 'dir':  '\v[\/]\.(git|hg|svn|doc)$',
\ 'file': '\v\.(exe|so|dll|o|swp|zip|gz)$',
\ }

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
" Airline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = '>'
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_detect_whitespace = 1 " turn off the whitespace extension

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SuperTAB 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:SuperTabDefaultCompletionType = "<c-n>"
