"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MAIN FUNCTIONS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"   Delete trailing whitespaces 
"   - Target the whole file
function! StripTrailingWhitespaces()
        " Preparation: save last search, and cursor position.
        let _s=@/
        let l = line(".")
        let c = col(".")
        " Do the business:
        %s/\s\+$//e
        " Clean up: restore previous search history, and cursor position
        let @/=_s
        call cursor(l, c)
endfunction

" Reindent the whole file
" - save cursor position and restore it afterwards
function! ReindentFile()
        " Preparation: save last search, and cursor position.
        let _s=@/
        let l = line(".")
        let c = col(".")
        " Do the business:
	:normal gg=G
	echom "ReindentFile"
        " Clean up: restore previous search history, and cursor position
        let @/=_s
        call cursor(l, c)
endfunction
