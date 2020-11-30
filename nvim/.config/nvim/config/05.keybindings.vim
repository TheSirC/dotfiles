" Defining leader key 
let mapleader = " "

" Search results centered please
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz

" Very magic by default
nnoremap ? ?\v
nnoremap / /\v
cnoremap %s/ %sm/

" Workaround for easymotion inconssitent behaviour
map <Leader> <Plug>(easymotion-prefix)

" Open new file adjacent to current file
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Open outline on <leader>o
nnoremap <silent> <Leader>o :TagbarToggle<CR>

" change working directory to where the file in the buffer is located
" if user types `,cd`
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

" Filetype specific map
filetype plugin on
autocmd Filetype markdown nmap <leader>m :! pandoc --template template/lettre_de_motivation.tex --pdf-engine=xelatex % -o %:r.pdf <CR>
autocmd Filetype markdown nmap <leader>l :! pandoc --template cv-template/cv.tex --pdf-engine=xelatex % -o %:r.pdf <CR>

" Rust specific keybindings
autocmd Filetype rust map <leader>f :RustFmt <CR>
autocmd Filetype rust map <leader>r <C-S>:RustRun <CR>
autocmd Filetype rust map <leader>c <C-S>:Cargo clippy <CR>

" Jump to start and end of line using the home row keys
map H ^
map L $

" Neat X clipboard integration
" <space>p will paste clipboard into buffer
" <space>c will copy entire buffer into clipboard
noremap <leader>p :read !xsel --clipboard --output<cr>
noremap <leader>c :w !xsel -ib<cr><cr>

" Spell-check set to <leader>o, 'o' for 'orthography':
map <F5> :silent :setlocal spell! spelllang=fr<CR>
map <F6> :silent :setlocal spell! spelllang=en_US<CR>

" <leader>s for Rg search
noremap <leader>s :Rg
let g:fzf_layout = { 'down': '~20%' }
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
\ <bang>0)

"Turn off highlight search after using it
nnoremap <F1> :set hlsearch!<CR>

" Temporarly fullscreen a window 
nnoremap <silent> <S-w> :ZoomWin<CR>

" split pane navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Save with normal keybinding
nnoremap <C-S> :w<CR>

" Open hotkeys
noremap <C-p> :Files<CR>
nnoremap <leader>; :Buffers<CR>

" <leader><leader> toggles between buffers
nnoremap <leader><Tab> <c-^>

" Left and right can switch buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>

" Up and down for quickfix list 
nnoremap <up> :cprev<CR>
nnoremap <down> :cnext<CR>

" Shift up and down for switching tabs
nnoremap <S-left> :tabp<CR>
nnoremap <S-right> :tabnext<CR>

" Close buffer with <leader>d
nnoremap <leader>q :bd<CR>

" nerdtree
autocmd vimenter * NERDTree " start nerdtree automatically when vim starts up
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>N :NERDTreeFocus<CR>

" Goyo plugin makes text more readable when writing prose:
nnoremap <leader>g :Goyo \| set linebreak<CR>

