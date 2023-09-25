-- This plugin provides a :SayHello command to say hello and a :SayGoodbye command to say
-- goodbye (and shut down the plugin).

local path_to_exe = vim.fn.expand("<sfile>:h").."/bin/main.exe"
local job

if not job then
  -- Although we don't demonstrate error handling in the VCaml Lua examples to keep them
  -- simple, we recommend thinking about how to handle errors. By default, we recommend
  -- taking the following steps:
  -- 1. Add [stderr_buffered = true] to [opts] to ensure stderr is captured.
  -- 2. Add an [on_stderr] callback to handle stderr. In most cases errors that occur in
  --    VCaml plugins will be handled for you by VCaml and communicated to Neovim as
  --    notifications, so displaying stderr on the screen will result in duplicate error
  --    reporting. However, it is still possible for errors to occur in places in the
  --    plugin that VCaml does not handle, e.g., in logic that runs at the toplevel, so
  --    best practice is to capture stderr and log it to a file that can be used for
  --    debugging. Once https://github.com/neovim/neovim/issues/22940 is resolved,
  --    [opts.stderr] can be accessed directly inside [on_exit].
  -- 3. Add logic in [on_exit] to report a non-zero exit.
  local opts = { on_exit = function() job = nil end }
  job = vim.fn.jobstart({ path_to_exe }, opts)
end
