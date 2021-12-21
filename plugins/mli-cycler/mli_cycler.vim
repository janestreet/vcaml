if exists("g:mli_cycler_loaded")
  finish
endif
let g:mli_cycler_loaded = 1

" Build the code in bin/ and then source this file in your init.vim to use this plugin.
let s:bin_path = fnamemodify(resolve(expand('<sfile>:p')), ':h').'/bin/main.exe'

function! s:rpcrequest(...) abort
  let l:job_id = jobstart([s:bin_path], { 'rpc': 1 })
  " [rpcrequest] blocks neovim until it gets a response (when the plugin completes),
  " ensuring actions are performed synchronously.
  call call('rpcrequest', [l:job_id] + a:000)
endfunction

" Go to the next ml/mli/intf file in the list of ml/mli/intf files for the current buffer.
function! s:next(...)
  let l:sink = a:0 == 0 ? 'edit' : a:1
  call s:rpcrequest('next', l:sink)
endfunction

" Go to the previous ml/mli/intf file in the list of ml/mli/intf files for the current
" buffer.
function! s:previous(...)
  let l:sink = a:0 == 0 ? 'edit' : a:1
  call s:rpcrequest('prev', l:sink)
endfunction

" This helper function is needed because we can't pass the result of [fzf#wrap] over
" Msgpack RPC to then call [fzf#run] because the sink value is a funcref.
function! MliCyclerFzf(config)
  call fzf#run(fzf#wrap(a:config))
  return v:null
endfunction

" Display the list of ml/mli/intf files for the current buffer in an fzf explorer.
function! s:list_fzf()
  call s:rpcrequest('list-fzf')
endfunction

" Echo the list of ml/mli/intf files for the current buffer in the command line (useful
" for exploring, or if you don't have fzf installed).
function! s:list()
  call s:rpcrequest('list')
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
