let current_dir = expand('<sfile>:p:h')

" To run the plugin, first build the executable in bin/, and then source this
" file

" Start the plugin
call jobstart([current_dir . '/bin/main.exe'])
