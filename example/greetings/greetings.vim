let current_dir = expand('<sfile>:p:h')

" Start the plugin.
call jobstart([current_dir . '/bin/main.exe'])

" This function will be called after the Neovim plugin finishes starting up.
function! OnGreetingsPluginStart (channel) abort
  echo rpcrequest(a:channel, 'greeting', $USER)
  call rpcrequest(a:channel, 'shutdown')
endfunction
