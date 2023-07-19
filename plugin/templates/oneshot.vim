" This function kicks off the job and immediately issues a blocking request. It uses
" variable arguments to mirror the normal usage of [rpcrequest], so you can use it for
" calling any method your plugin supports.
function! s:rpcrequest(...) abort
  let l:job_id = jobstart(['/path/to/exe'], { 'rpc': 1 })
  return call('rpcrequest', [l:job_id] + a:000)
endfunction

" Example call:
call s:rpcrequest("my-rpc", arg1, arg2)
