const g:my_plugin_log = tempname()

function! s:start_plugin()
  if exists('s:job')
    return
  endif
  let opts = {}
  " VCaml plugins should automatically notify Neovim when errors occur, but to be safe
  " (e.g., in the event of a library bug or protocol-level error) we capture and log
  " stderr. We don't display it because in most cases the message will be duplicative of
  " error notifications VCaml already sends.
  let opts.stderr_buffered = 1
  function! opts.on_exit(job_id, exit_code, event) closure
    " [v:exiting] is [v:null] when not exiting and 0 when exiting with status 0. Because
    " in VimL v:null == 0, we need to first encode v:exiting in JSON to distinguish the
    " possible values.
    if json_encode(v:exiting) != "null"
      return
    endif
    let l:stderr = opts['stderr']
    if len(l:stderr > 1) then
      call writefile(l:stderr, g:my_plugin_log, "ab")
    endif
    if a:exit_code != 0
      call nvim_echo([['MyPlugin exited with code '.a:exit_code.'.', 'ErrorMsg']], 1, {})
    endif
  endfunction
  let s:job = jobstart(['/path/to/exe'], opts)
endfunction

function! s:stop_plugin()
  if !exists('s:job')
    return
  endif
  call jobstop(s:job)
  unlet s:job
  unlet! s:channel
endfunction

" Pass the name of this function ("On_startup" in this example) to [notify_fn] to have it
" called when the plugin starts up.
function! On_startup(channel)
  " Save the channel so we can invoke RPCs.
  let s:channel = a:channel
  echo "Plugin started and is ready for use!"
endfunction

function! s:my_rpc()
  if !exists("s:channel")
    echoerr "Plugin has not started yet."
    return
  endif
  call rpcrequest(s:channel, "my-rpc")
endfunction

" Start the plugin when this file is sourced.
call s:start_plugin()

" Give the user explicit controls over stopping and starting the plugin. This is better
" than auto-restarting the plugin to avoid a loop for a plugin that crashes on startup.
command! -bar MyPluginStart call s:start_plugin()
command! -bar MyPluginStop call s:stop_plugin()

command! -bar MyPluginMyRpc call s:my_rpc()
