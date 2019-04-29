" Check that vim-plug is installed
if exists(":PlugInstall")
	silent! execute '!curl --create-dirs -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
	autocmd VimEnter * silent! PlugInstall
endif
for f in split(glob('~/.config/nvim/config/*.vim'), '\n')
    exe 'source' f
endfor
