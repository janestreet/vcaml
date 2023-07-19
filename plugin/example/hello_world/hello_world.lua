-- This plugin provides a :SayHello command to say hello.

local path_to_exe = vim.fn.expand("<sfile>:h") .. "/bin/main.exe"

vim.api.nvim_create_user_command(
  "SayHello",
  function(args)
    local job = vim.fn.jobstart({ path_to_exe }, { rpc = true })
    vim.rpcrequest(job, "hello", args.args)
  end,
  { bar = true, nargs = 1, complete = "user" })
