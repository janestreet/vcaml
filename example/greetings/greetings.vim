let current_dir = expand('<sfile>:p:h')

" Start the plugin.
call jobstart([current_dir . '/bin/main.exe'])

" This function will be called after the Neovim plugin finishes starting up.
function! OnGreetingsPluginStart (rpc_chan) abort
  echo rpcrequest(a:rpc_chan, 'greeting', $USER)
  call rpcrequest(a:rpc_chan, 'shutdown')
endfunction
