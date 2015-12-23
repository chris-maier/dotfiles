"
"Persistent undo
try
	set undodir="/tmp/undodir"
    set undofile
catch
endtry

" Use Unix as the standard file type
set ffs=unix,dos,mac

" -----------------------------------------------------------------------------
" call the ctags program
function! GenCtags ()
    " Generate ctags file on
    silent !ctags -h [".h.hpp.h++.c.cpp.c++.py"] -R --exclude=.git --exclude=.svn --file-scope=yes --sort=yes --extra=+f * 
endfunction

function! ReformatCFiles ()
    " reformat C files  
    " see -> http://astyle.sourceforge.net/astyle.html
    silent execute "!astyle --style=bsd --indent=tab=4 -CSwYpxdHUjnk1W1 " . expand('%:p')
    "tabdo exec 'windo e'
endfunction

"
" Execute a build depending on what kind of build tool is present
" Supports Ninja or Makefile
"
function! Build ()
    let oldpath = getcwd ()
    " Check if we are using MAKE
    if filereadable(expand ("[mM]akefile"))
        " Makefile is present 
        make
    "Check if we are using NINJA 
    elseif filereadable(expand("build.ninja"))
        cexpr system('ninja -v') | copen
    elseif isdirectory (expand (oldpath . "/build"))
        execute "cd " . oldpath . "/build"
        cexpr system('ninja -v') | copen
        "!ninja -v
        execute "cd " . oldpath
    else 
        echo "Nothing to do" 
    endif 
endfunction

