let s:current_dir = fnamemodify(resolve(expand("<sfile>")), ':h')

" Sample bindings for the mli cycling plugin
"
" Build the code in bin/ and then source this file in your vimrc in order to
" use the plugin.

" Goes to the next ml/mli/intf file in the list of ml/mli/intf files for the
" current buffer.
function! mli_cycling#next() 
  call jobstart([s:current_dir . '/bin/main.exe', 'next'])
endfunction

" Goes to the next ml/mli/intf file in the list of ml/mli/intf files for the
" current buffer.
function! mli_cycling#previous()
  call jobstart([s:current_dir . '/bin/main.exe', 'prev'])
endfunction

" Displays the list of ml/mli/intf files for the current buffer in an fzf
" explorer.
function! mli_cycling#list_fzf()
  call jobstart([s:current_dir . '/bin/main.exe', 'list-fzf'])
endfunction

" Echoes the list of ml/mli/intf files for the current buffer in the command
" line (useful for exploring, or if you don't have fzf installed).
function! mli_cycling#list()
  call jobstart([s:current_dir . '/bin/main.exe', 'list'])
endfunction

" Sample bindings 
noremap <leader>a :call mli_cycling#next()<cr>
noremap <leader><leader>a :call mli_cycling#previous()<cr>
noremap <leader>l :call mli_cycling#list_fzf()<cr>
noremap <leader>e :call mli_cycling#list()<cr>
