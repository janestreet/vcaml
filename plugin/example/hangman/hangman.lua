-- This plugin provides a :Hangman command to start a game of Hangman. In the game window,
-- the lowercase letter keys are remapped to guesses.

local path_to_exe = vim.fn.expand("<sfile>:h").."/bin/main.exe"

local job
local buffer

function hangman_setup(channel)
  buffer = vim.rpcrequest(channel, "buffer")
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    callback = function()
      if job then
        vim.fn.jobstop(job)
        job = nil
        buffer = nil
      end
    end,
  })
end

vim.api.nvim_create_user_command("Hangman", function()
  if buffer then
    if vim.fn.jobwait({ job }, 0)[1] == -1 then
      vim.api.nvim_err_writeln("Already in the middle of a game!")
      return
    end
    vim.api.nvim_buf_delete(buffer, { force = true, unload = false })
  end
  job = vim.fn.jobstart({ path_to_exe })
end, {})
