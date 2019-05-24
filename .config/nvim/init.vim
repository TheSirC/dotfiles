" Check that vim-plug is installed
for f in split(glob('~/.config/nvim/config/*.vim'), '\n')
    exe 'source' f
endfor
