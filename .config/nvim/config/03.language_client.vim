let $RUST_SRC_PATH = systemlist("rustc --print sysroot")[0] . "/lib/rustlib/src/rust/src"

"Coc settings
" use <tab> for trigger completion and navigate to next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" Language server parameters
let g:LanguageClient_autoStart = 1
let g:LanguageClient_selectionUI = "fzf"

let g:LanguageClient_serverCommands = {
	\ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
\}

let g:LanguageClient_rootMarkers = {
            \ 'rust': ['Cargo.toml'],
\}

" don't make errors so painful to look at
let g:LanguageClient_diagnosticsDisplay = {
    \     1: {
    \         "name": "Error",
    \         "texthl": "ALEError",
    \         "signText": "✖",
    \         "signTexthl": "ErrorMsg",
    \         "virtualTexthl": "WarningMsg",
    \     },
    \     2: {
    \         "name": "Warning",
    \         "texthl": "ALEWarning",
    \         "signText": "⚠",
    \         "signTexthl": "ALEWarningSign",
    \         "virtualTexthl": "Todo",
    \     },
    \     3: {
    \         "name": "Information",
    \         "texthl": "ALEInfo",
    \         "signText": "ℹ",
    \         "signTexthl": "ALEInfoSign",
    \         "virtualTexthl": "Todo",
    \     },
    \     4: {
    \         "name": "Hint",
    \         "texthl": "ALEInfo",
    \         "signText": "➤",
    \         "signTexthl": "ALEInfoSign",
    \         "virtualTexthl": "Todo",
    \     },
\ }

