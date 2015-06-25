syntax on
filetype plugin on
filetype indent on

set nocompatible
set number
set autoindent
set cindent
set hlsearch
set incsearch
set encoding=utf-8
set backspace=indent,eol,start
set wildmenu
set mouse+=a
colorscheme behelit

" Neovim {{{
if has("nvim")
	" yank uses X clipboard
	set clipboard+=unnamedplus
endif
" }}}

" vim-plug {{{
" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
    execute '!mkdir -p ~/.vim/autoload'
    execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/plugged')
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'klen/python-mode'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'bling/vim-airline'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc', { 'do' : 'make' }
Plug 'Shougo/neomru.vim'
Plug 'Valloric/YouCompleteMe', { 'do' : './install.sh --clang-completer --system-libclang --system-boost' }
Plug 'tpope/vim-fugitive'
call plug#end()
" }}}

" YouCompleteMe {{{
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
" }}}

" NERDTree {{{
noremap <leader>n :NERDTreeToggle<cr>
" }}}

" vim-airline {{{
let g:airline_theme='behelit'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
set laststatus=2
" we don't need mode since airline show it
set noshowmode
" }}}

" python-mode {{{
let g:pymode_run_bind = '<leader>pr'
let g:pymode_breakpoint_bind = '<leader>pb'
let g:pymode_python = 'python3'
" }}}

" Unite.vim {{{
let g:unite_source_grep_max_candidates = 200

" This allows buffers to be hidden if you've modified a buffer.
set hidden

if executable('ag')
	" Use ag in unite grep source.
	let g:unite_source_grep_command = 'ag'
	let g:unite_source_grep_default_opts = '-i --line-numbers --nocolor --nogroup --hidden'
	let g:unite_source_grep_recursive_opt = ''
endif

call unite#filters#matcher_default#use(['matcher_fuzzy'])
nnoremap <leader>t :<C-u>Unite -buffer-name=files_rec -start-insert file_rec/neovim:!<cr>
nnoremap <leader>f :<C-u>Unite -buffer-name=files -start-insert file<cr>
nnoremap <leader>r :<C-u>Unite -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>e :<C-u>Unite -buffer-name=buffer -start-insert buffer<cr>
" }}}

" Misc {{{
" Reset search highlight
noremap <leader>h :nohl<cr>

" move between windows
nnoremap <m-down>  <c-w>j
nnoremap <m-up>    <c-w>k
nnoremap <m-left>  <c-w>h
nnoremap <m-right> <c-w>l

" move between buffers
nnoremap <m-l> :bnext<cr>
nnoremap <m-j> :bprevious<cr>

" split window
nnoremap <leader>- :split<cr>
nnoremap <leader>\ :vsplit<cr>
" }}}

" vim: nowrap fdm=marker foldcolumn=2
