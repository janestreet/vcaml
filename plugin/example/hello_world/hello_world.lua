-- This plugin provides a :SayHello command to say hello.

local source_file = vim.fn.expand("<sfile>")
if source_file == ":source (no file)" then
  -- We need the file path to discover the binary's location.
  error("This lua file must be sourced with a path")
end
local path_to_exe = vim.fs.dirname(source_file).."/bin/main.exe"

vim.api.nvim_create_user_command(
  "SayHello",
  function(args)
    local job = vim.fn.jobstart({ path_to_exe }, { rpc = true })
    vim.rpcrequest(job, "hello", args.args)
  end,
  { bar = true, nargs = 1, complete = "user" })
