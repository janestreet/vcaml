" To run the plugin, first build the executable in bin/, and then source this file.

let current_dir = expand('<sfile>:p:h')

" Start the plugin.
call jobstart([current_dir . '/bin/main.exe'])
