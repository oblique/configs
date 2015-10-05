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
	autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')
Plug 'SyntaxAttr.vim'
Plug 'klen/python-mode'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'bling/vim-airline'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc', { 'do' : 'make' }
Plug 'Shougo/neomru.vim'
Plug 'tpope/vim-fugitive'
Plug 'danro/rename.vim', { 'on' : 'Rename' }
Plug 'gtags.vim'
Plug 'hewes/unite-gtags'

" Install YouCompleteMe only if we have the dependencies installed
if executable('cmake') && isdirectory('/usr/include/clang-c') && isdirectory('/usr/include/boost')
	Plug 'Valloric/YouCompleteMe', { 'do' : './install.sh --clang-completer --system-libclang --system-boost' }
endif
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
call plug#end()
" }}}

" YouCompleteMe {{{
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
" }}}

" Eclim {{{
let g:EclimDefaultFileOpenAction = 'edit'
let g:EclimCompletionMethod = 'omnifunc'
nnoremap <silent><leader>es :CSearchContext<cr>
nnoremap <silent><leader>eh :CCallHierarchy<cr>
nnoremap <silent><leader>et :ProjectTreeToggle<cr>
nnoremap <silent><leader>ep :ProjectProblems<cr>
nnoremap <silent><leader>ef :LocateFile<cr>

" Same as the default, but here we have Edit action as first action.
" The first action is used when we press enter on a file in ProjectTree.
let g:EclimProjectTreeActions = [
    \ {'pattern': '.*', 'name': 'Edit', 'action': 'edit'},
    \ {'pattern': '.*', 'name': 'Split', 'action': 'split'},
    \ {'pattern': '.*', 'name': 'VSplit', 'action': 'vsplit'},
    \ {'pattern': '.*', 'name': 'Tab', 'action': 'tablast | tabnew'},
  \ ]
" }}}

" NERDTree {{{
noremap <silent><leader>n :NERDTreeToggle<cr>
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
let g:pymode_folding = 0
" }}}

" vim-markdown {{{
let g:vim_markdown_folding_disabled = 1
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
nnoremap <silent><leader>t :Unite -buffer-name=files_rec -start-insert file_rec/neovim:!<cr>
nnoremap <silent><leader>f :Unite -buffer-name=files -start-insert file<cr>
nnoremap <silent><leader>r :Unite -buffer-name=mru -start-insert file_mru<cr>
nnoremap <silent><leader>b :Unite -buffer-name=buffer -start-insert buffer<cr>
nnoremap <silent><leader>z] :UniteNext<cr>
nnoremap <silent><leader>z[ :UnitePrevious<cr>
nnoremap <silent><leader>zr :UniteResume<cr>
" }}}

" unite-gtags {{{
nnoremap <silent><leader>gd :Unite gtags/def<cr>
nnoremap <silent><leader>gr :Unite gtags/ref<cr>
nnoremap <silent><leader>gc :Unite gtags/context<cr>
nnoremap <silent><leader>gg :Unite gtags/grep<cr>
" }}}

" Misc {{{
if &term =~ "rxvt.*"
	execute "set <m-j>=\ej"
	execute "set <m-l>=\el"
	execute "set <m-c>=\ec"
	execute "set <m-v>=\ev"
endif
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
" }}}

if !empty(glob("~/.vim/custom.vim"))
	source ~/.vim/custom.vim
endif

" vim: nowrap fdm=marker foldcolumn=2
