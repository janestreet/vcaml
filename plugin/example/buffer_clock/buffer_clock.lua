-- This plugin opens a window with a clock. Close the window to shut down the plugin.

local path_to_exe = vim.fn.expand("<sfile>:h").."/bin/main.exe"

function buffer_clock_setup(channel)
  -- The channel is exposed as a global vim variable for use in tests.
  vim.g.buffer_clock_channel = channel
end

if not vim.g.buffer_clock_job then
  local opts = { on_exit = function() vim.g.buffer_clock_job = nil end }
  -- The job is exposed as a global vim variable for use in tests.
  vim.g.buffer_clock_job = vim.fn.jobstart({ path_to_exe }, opts)
end
