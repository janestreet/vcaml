if exists("g:mli_cycler_loaded")
  finish
endif
let g:mli_cycler_loaded = 1

let s:bin_path = fnamemodify(resolve(expand('<sfile>:p')), ':h').'/bin/main.exe'
" Build the code in bin/ and then source this file in your vimrc in order to
" use the plugin.

" Goes to the next ml/mli/intf file in the list of ml/mli/intf files for the
" current buffer.
function! s:next(...)
  if a:0 == 0
    call jobstart([s:bin_path, 'next'])
  else
    call jobstart([s:bin_path, 'next', "-sink", a:1])
  endif
endfunction

" Goes to the next ml/mli/intf file in the list of ml/mli/intf files for the
" current buffer.
function! s:previous(...)
  if a:0 == 0
    call jobstart([s:bin_path, 'prev'])
  else
    call jobstart([s:bin_path, 'prev', "-sink", a:1])
  endif
endfunction

" This helper function is needed because we can't pass the result of [fzf#wrap] over
" Msgpack RPC to then call [fzf#run] because the sink value is a funcref.
function! MliCyclerFzf(config)
  call fzf#run(fzf#wrap(a:config))
endfunction

" Displays the list of ml/mli/intf files for the current buffer in an fzf
" explorer.
function! s:list_fzf()
  call jobstart([s:bin_path, 'list-fzf'])
endfunction

" Echoes the list of ml/mli/intf files for the current buffer in the command
" line (useful for exploring, or if you don't have fzf installed).
function! s:list()
  call jobstart([s:bin_path, 'list'])
endfunction

function! s:set_up_commands()
  command! -bar -buffer -nargs=? MliCyclerNext call s:next(<f-args>)
  command! -bar -buffer -nargs=? MliCyclerPrev call s:previous(<f-args>)
  command! -bar -buffer MliCyclerListFzf call s:list_fzf()
  command! -bar -buffer MliCyclerList call s:list()
endfunction

augroup mli_cycler
  autocmd!
  autocmd FileType ocaml call s:set_up_commands()
augroup END
