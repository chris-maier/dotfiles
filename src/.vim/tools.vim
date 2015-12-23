"
" VIM helper functions. 
" Source in vimrc

" ----------------------------------------------------------------------------- "
" - GREP 
" - @param type visual or normal mode 'n' or 'v'
" - @param case Case sensitivity 'c' 'i'
" ----------------------------------------------------------------------------- "
function! GrepVisualOperator(type, case)
    " grep options, include and exclude pattern
    let GrepSourceFiles = "--include=*.c --include=*.h --include=*.cpp --include=*.txt --include=*.dox --include=*.py --exclude=*.o"
    let GrepExclDirs	= "--exclude-dir=.svn --exclude-dir=CMakeFiles --exclude-dir=doc "

    " Case argument 
    if a:case ==# 'c' 
        let GrepOptions 	= "-Isrn ". GrepSourceFiles . GrepExclDirs . " . -e "
    elseif a:case ==# 'i'
        let GrepOptions = "-Iisrn ". GrepSourceFiles . GrepExclDirs . " . -e "
    else
        echo "Case Argument is wrong"
        return
    endif

    " VISUAL mode
    if a:type ==# 'v'
        normal! `<v`>y
        " so that we can search for marcos with leading '#'
        let l:bar = substitute(shellescape(@@), "#", "\\\\\\\\\\#", "")
        
    " NORMAL mode
    elseif a:type ==# 'n'
        let l:bar = shellescape(expand("<cword>"))
    endif
    
    " Execute grep command. 
    silent exe "grep! " . GrepOptions . l:bar
    " Open the created list
    copen
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HEX MODE 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ex command for toggling hex mode - define mapping if desired
command! -bar Hexmode call ToggleHex()

" helper function to toggle hex mode
function! ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    %!xxd
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0
    " return to normal editing
    %!xxd -r
  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
endfunction

" TOGGLE SLASH
" This works forward and backwards
function! ToggleSlash(independent) range
  let from = ''
  for lnum in range(a:firstline, a:lastline)
    let line = getline(lnum)
    let first = matchstr(line, '[/\\]')
    if !empty(first)
      if a:independent || empty(from)
        let from = first
      endif
      let opposite = (from == '/' ? '\' : '/')
      call setline(lnum, substitute(line, from, opposite, 'g'))
    endif
  endfor
endfunction
command! -bang -range ToggleSlash <line1>,<line2>call ToggleSlash(<bang>1)

"   Leerzeichen am Zeilenende
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
