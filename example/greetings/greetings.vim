let current_dir = expand('<sfile>:p:h')

" Start the plugin
call jobstart([current_dir . '/bin/main.exe'])

" This function will be called when the main process of the plugin has
" connected to neovim
function! OnGreetingsPluginStart (rpc_chan)
  echo rpcrequest(a:rpc_chan, 'greeting', 'Vcaml user')
  call rpcrequest(a:rpc_chan, 'shutdown')
endfunction
