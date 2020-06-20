" Import vim configuration parameters
set termguicolors
set runtimepath^=~/.vim runtimepath+=~/.vim/after 
let &packpath = &runtimepath 
source ~/.vimrc 
set ttyfast
set nofoldenable

let g:ale_completion_enabled = 1
" Ultisnips parameters
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Colorscheme
let g:vim_monokai_tasty_italic = 1                    " allow italics, set this before the colorscheme
colorscheme vim-monokai-tasty                         " set the colorscheme

" Light line parameters
set background=dark 
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ]
            \ },
            \ 'component_function': {
            \   'cocstatus': 'coc#status'
            \ },
      \ }
set noshowmode

" Goyo configuration
function! s:goyo_enter()
	Limelight
	let b:quitting = 0
	let b:quitting_bang = 0
  	autocmd QuitPre <buffer> let b:quitting = 1
  	cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
endfunction

function! s:goyo_leave()
	Limelight!
	" Quit Vim if this is the only remaining buffer
	if b:quitting && len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
  	  if b:quitting_bang
  	    qa!
  	  else
  	    qa
  	  endif
  	endif
endfunction

autocmd! User GoyoEnter call <SID>goyo_enter()
autocmd! User GoyoLeave call <SID>goyo_leave()

" use ripgreg instead of grep
set grepprg=rg\ --vimgrep

" =============================================================================
" # Editor settings
" =============================================================================

" relative line numbers
" Sets relative line numbers in normal mode, absolute line numbers in insert
" mode
set number
set relativenumber

" enable mouse support
set mouse=a mousemodel=popup

" Required for operations modifying multiple buffers like rename.
set hidden

" Autoindentation
set ai
filetype plugin indent on
set autoindent
set timeoutlen=100 
set encoding=utf-8
set scrolloff=2
set hidden
set nojoinspaces

" Sane splits
set splitright
set splitbelow

" Permanent undo
set undodir=~/.vimdid
set undofile

" Decent wildmenu
set wildmenu
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor

" Use wide tabs
set shiftwidth=8
set softtabstop=8
set tabstop=8
set noexpandtab

" Get syntax
syntax on

" Wrapping options
set formatoptions=tc " wrap text and comments using textwidth
set formatoptions+=r " continue comments when pressing ENTER in I mode
set formatoptions+=q " enable formatting of comments with gq
set formatoptions+=n " detect lists for formatting
set formatoptions+=b " auto-wrap in insert mode, and do not wrap old long lines

" Smooth-scroll settings
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" Language specific parameters
" Rust
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -selection clipboard'
