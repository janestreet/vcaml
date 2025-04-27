-- This plugin provides a :SayHello command to say hello and a :SayGoodbye command to say
-- goodbye (and shut down the plugin).

local source_file = vim.fn.expand("<sfile>")
if source_file == ":source (no file)" then
  -- We need the file path to discover the binary's location.
  error("This lua file must be sourced with a path")
end
local path_to_exe = vim.fs.dirname(source_file).."/bin/main.exe"

local job

if not job then
  local opts = { on_exit = function() job = nil end }
  job = vim.fn.jobstart({ path_to_exe }, opts)
end
