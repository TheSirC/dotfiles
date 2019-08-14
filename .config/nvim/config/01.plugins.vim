if !exists(":PlugInstall")
	silent! execute '!curl --create-dirs -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin() 
	" GUI
	Plug 'itchyny/lightline.vim'
	Plug 'junegunn/goyo.vim' 
	Plug 'junegunn/limelight.vim' 
	Plug 'patstockwell/vim-monokai-tasty'
	Plug 'reedes/vim-pencil'
	Plug 'scrooloose/nerdtree'
	Plug 'tpope/vim-fugitive'
	Plug 'majutsushi/tagbar'
	Plug 'tmux-plugins/vim-tmux'
	Plug 'inside/vim-search-pulse'
	Plug 'terryma/vim-smooth-scroll'
	Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
	Plug 'blindFS/vim-taskwarrior'
	" Fuzzy searcher
	Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }
	Plug 'lotabout/skim.vim'
	" Language support
	Plug 'LnL7/vim-nix'
	Plug 'dag/vim-fish'
	Plug 'mattn/emmet-vim'
	Plug 'pest-parser/pest.vim'
	Plug 'plasticboy/vim-markdown'
	Plug 'rust-lang/rust.vim'
	Plug 'vim-pandoc/vim-pandoc' 
	Plug 'vim-pandoc/vim-pandoc-syntax' 
	" Completion support
	Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
	" Snippets
	Plug 'honza/vim-snippets'
	Plug 'sirver/ultisnips'
	" Quality of life
	Plug 'JAffleck/vim-titlecase', {'branch': 'improveTitlecase'}
	Plug 'vim-scripts/ZoomWin'
	Plug 'terryma/vim-multiple-cursors'
	Plug 'christoomey/vim-sort-motion' 
	Plug 'dhruvasagar/vim-table-mode'
	Plug 'easymotion/vim-easymotion'
	Plug 'junegunn/vim-easy-align'
	Plug 'raimondi/delimitmate'
	Plug 'tpope/vim-sensible' 
	Plug 'tpope/vim-surround'
call plug#end() 
