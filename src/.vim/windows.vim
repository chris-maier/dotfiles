"
"Persistent undo
try
	set undodir="C:\Users\Maierc\temp\undodir"
    set undofile
catch
endtry

set wildignore+=tmp\*,*.swp,*.zip,*.exe,*.dll,*\\Debug\*,*\\doc\*   " Windows

if has("win32")
    :set guifont=Consolas:h10:b:cANSI
endif

" build Doxyfile
map <S-F2> :!start dox.cmd <CR><CR>
"

:map <M-F3> :!start bash.exe <CR>

" -----------------------------------------------------------------------------
" call the ctags program
map <F6> :!start ctags -R -h [".h.c.cpp.py.cs"] --file-scope=yes --sort=yes --extra=+f * <CR>

" ignore some filetypes
set wildignore+=tmp\*,*.swp,*.zip,*.exe,*.dll,*\\Debug\*,*\\doc\*   " Windows

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CTAGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let Tlist_Ctags_Cmd = 'C:\Program Files (x86)\utils'

" Reformat C files with AStyle in Windows
function! <SID>ReformatCFiles()
        let _s=@/
        let l = line(".")
        let c = col(".")

        " reformat C files  
		" see -> http://astyle.sourceforge.net/astyle.html
        :execute "!AStyle --style=bsd --indent=tab=4 --indent-switches -C -M80 -m0 -f -w -Y -p -xd -H -j -n -k1 -W1 " . expand('%:p') 

        edit
        w!
        " pad the block comments
        %s/^\*/ \*/g
        w!
        " reload file
        :edit
        " restore previous search history and cursor position 
        let @/=_s
        call cursor(l,c)
endfunction


