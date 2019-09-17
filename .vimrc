syntax on
filetype plugin on
filetype indent on

set nocompatible
set number
set autoindent
set cindent
set cinoptions=:0,l1,g0,t0,(0
set textwidth=80
set hlsearch
set incsearch
set encoding=utf-8
set backspace=indent,eol,start
set wildmenu
set mouse+=a
set splitbelow
set hidden
colorscheme behelit

" Neovim {{{
if has("nvim")
    " use both primary and secondary X clipboards
    set clipboard=unnamed,unnamedplus

    " force xsel as clipboard backend
    let g:clipboard = {
          \   'name': 'xsel',
          \   'copy': {
          \      '+': 'xsel --nodetach -i -b',
          \      '*': 'xsel --nodetach -i -p',
          \    },
          \   'paste': {
          \      '+': 'xsel -o -b',
          \      '*': 'xsel -o -p',
          \   },
          \   'cache_enabled': 1,
          \ }
endif
" }}}

" vim-plug {{{
" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
    execute '!mkdir -p ~/.vim/autoload'
    execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

if has('python3')
    Plug 'roxma/nvim-yarp'
endif

" neovim compatibility for vim
if !has('nvim')
    Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'

" fzf
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" moves block of code
Plug 'matze/vim-move'

" quick video tutorial: http://vimcasts.org/e/29
Plug 'godlygeek/tabular'

" syntax
Plug 'plasticboy/vim-markdown'
Plug 'jceb/vim-orgmode'
Plug 'ntpeters/vim-better-whitespace'

" You need to run these commands:
"   rustup component add rust-src
"   rustup component add rustfmt
"   rustup component add clippy
"   rustup component add rls
Plug 'rust-lang/rust.vim'

" gtags
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ozelentok/denite-gtags'

" auto-completion
if has('nvim')
    Plug 'ncm2/ncm2'
    Plug 'ncm2/ncm2-path'
    Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh', }
endif

call plug#end()
" }}}

" ncm2 {{{
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANT: `:help Ncm2PopupOpen` and `:help completeopt` for more information
set completeopt=noinsert,menuone,noselect
" }}}

" NERDTree {{{
noremap <silent><leader>n :NERDTreeToggle<cr>
" }}}

" vim-airline {{{
let g:airline_theme='darkness'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
set laststatus=2
" we don't need mode since airline show it
set noshowmode
" }}}

" vim-markdown {{{
let g:vim_markdown_folding_disabled = 1
" }}}

" fzf {{{
" hide status line
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

nnoremap <silent><c-b> :Buffers<cr>
nnoremap <silent><leader>fb :Buffers<cr>
nnoremap <silent><leader>ff :Files<cr>
nnoremap <silent><leader>fg :Ag<cr>
nnoremap <silent><leader>ft :Tags<cr>
" }}}

" vim-move {{{
let g:move_map_keys = 0
vmap <c-down> <Plug>MoveBlockDown
vmap <c-up> <Plug>MoveBlockUp
nmap <c-down> <Plug>MoveLineDown
nmap <c-up> <Plug>MoveLineUp
" }}}

" rust {{{
let g:rustfmt_autosave = 1
let g:rust_fold = 1
autocmd FileType rust nnoremap <buffer><silent><leader>i :RustFmt<cr>
" }}}

" LanguageClient {{{
set signcolumn=yes

let g:LanguageClient_serverCommands = {
    \ 'rust':   ['rustup', 'run', 'stable', 'rls'],
    \ }

let g:LanguageClient_diagnosticsDisplay = {
    \ 1: { "signTexthl": "LC_ErrorSign", "virtualTexthl": "LC_Error" },
    \ 2: { "signTexthl": "LC_WarnSign", "virtualTexthl": "LC_Warn" },
    \ 3: { "signTexthl": "LC_InfoSign", "virtualTexthl": "LC_Info" },
    \ 4: { "signTexthl": "LC_HintSign", "virtualTexthl": "LC_Hint" },
    \ }

nnoremap <F5> :call LanguageClient_contextMenu()<cr>
nnoremap <silent><leader>ld :call LanguageClient#textDocument_definition()<cr>
nnoremap <silent><leader>lr :call LanguageClient#textDocument_references()<cr>
nnoremap <silent><leader>lm :call LanguageClient#textDocument_hover()<cr>
nnoremap <silent><leader>li :call LanguageClient#explainErrorAtPoint()<cr>
" }}}

" denite-gtags {{{
call denite#custom#option('_', {
            \ 'mode': 'normal',
            \ 'winheight': 10,
            \ 'highlight_cursor': 'Cursor',
            \ 'highlight_matched_range': 'Statement',
            \ 'highlight_matched_char': 'Statement',
            \ 'highlight_mode_normal': 'Type',
            \ 'highlight_mode_insert': 'Type',
            \ 'highlight_preview_line': 'Underlined',
            \ })

call denite#custom#source('tag', 'matchers', ['matcher_substring'])
call denite#custom#source('gtags_ref', 'matchers', ['matcher_substring'])
call denite#custom#source('gtags_def', 'matchers', ['matcher_substring'])

function! s:denite_maps() abort
    nnoremap <buffer><silent><expr><CR> denite#do_map('do_action')
    nnoremap <buffer><silent><expr>d denite#do_map('do_action', 'delete')
    nnoremap <buffer><silent><expr>p denite#do_map('do_action', 'preview')
    nnoremap <buffer><silent><expr>q denite#do_map('quit')
    nnoremap <buffer><silent><expr>i denite#do_map('open_filter_buffer')
    nnoremap <buffer><silent><expr><Space> denite#do_map('toggle_select').'j'
endfunction

function! s:c_cpp_maps() abort
    nnoremap <buffer><silent><leader>gd :DeniteCursorWord gtags_def<cr>
    nnoremap <buffer><silent><leader>gr :DeniteCursorWord gtags_ref<cr>
    nnoremap <buffer><silent><leader>gc :DeniteCursorWord gtags_context<cr>
    nnoremap <buffer><silent><leader>gn :Denite -resume -cursor-pos=+1 -immediately<cr>
    nnoremap <buffer><silent><leader>gp :Denite -resume -cursor-pos=+1 -immediately<cr>
    nnoremap <buffer><silent><leader>gg :Denite -resume<cr>
endfunction

autocmd FileType denite call s:denite_maps()
autocmd FileType c,cpp call s:c_cpp_maps()
" }}}

" Misc {{{
set guicursor=

" Disable ex mode
nnoremap Q <Nop>

" Reset search highlight
noremap <leader>h :nohl<cr>

" move between windows
nnoremap <m-down>  <c-w>j
nnoremap <m-up>    <c-w>k
nnoremap <m-left>  <c-w>h
nnoremap <m-right> <c-w>l

" move between buffers
nnoremap <silent><m-l> :bnext<cr>
nnoremap <silent><m-j> :bprevious<cr>

" split window
nnoremap <silent><leader>- :split<cr>
nnoremap <silent><leader>\ <c-g>:vsplit<cr>
nnoremap <silent><leader>q :quit<cr>
nnoremap <silent><leader>o :only<cr>

command! EnableSpaces set expandtab softtabstop=4 shiftwidth=4 tabstop=4
command! EnableTabs   set noexpandtab softtabstop=0 shiftwidth=8 tabstop=8
EnableSpaces

" reindent whole file
nnoremap <silent><leader>i mzgg=G`z

command SyntaxAttr call SyntaxAttr()

vnoremap <silent><m-c> :!xclip -f -sel clip<cr>
inoremap <silent><m-v> <esc>:set paste<cr>:r!xclip -o -sel clip<cr>:set nopaste<cr>i
nnoremap <silent><m-v> :set paste<cr>:r!xclip -o -sel clip<cr>:set nopaste<cr>

" use ]s and [s to move to mispelled word
" use z= to get suggestion list
" use zg to add a word to dictionary
" use zw to mark a word as incorrect
set spelllang=en_us

function! ToggleSpell()
    if &spell
        set nospell
    else
        set spell
    endif
endfunction

command ToggleSpell call ToggleSpell()
nnoremap <silent><leader>s :ToggleSpell<cr>

command ShowPath echo expand('%:p')
" }}}

if !empty(glob("~/.vim/custom.vim"))
    source ~/.vim/custom.vim
endif

" vim: nowrap fdm=marker foldcolumn=2
