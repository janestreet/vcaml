-- This plugin provides a :SayHello command to say hello and a :SayGoodbye command to say
-- goodbye (and shut down the plugin).

local path_to_exe = vim.fn.expand("<sfile>:h").."/bin/main.exe"
local job

if not job then
  local opts = { on_exit = function() job = nil end }
  job = vim.fn.jobstart({ path_to_exe }, opts)
end
