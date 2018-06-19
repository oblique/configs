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
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'danro/rename.vim', { 'on' : 'Rename' }

" neovim compatibility for vim
if !has('nvim')
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'

" denite
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ozelentok/denite-gtags'

" moves block of code
Plug 'matze/vim-move'

" quick video tutorial: http://vimcasts.org/e/29
Plug 'godlygeek/tabular'

" syntax
Plug 'plasticboy/vim-markdown'
Plug 'jceb/vim-orgmode'
Plug 'ntpeters/vim-better-whitespace'

" You need to run these commands:
"   cargo install racer
"   rustup component add rust-src
"   rustup component add rustfmt-preview
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

call plug#end()
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

" vim-markdown {{{
let g:vim_markdown_folding_disabled = 1
" }}}

" denite.vim {{{
if executable('ag')
    " The Silver Searcher
    call denite#custom#var('file_rec', 'command',
                \ ['ag', '-U', '--hidden', '--follow', '--nocolor', '--nogroup', '-g', ''])

    " Setup ignore patterns in your .agignore file!
    " https://github.com/ggreer/the_silver_searcher/wiki/Advanced-Usage

    call denite#custom#var('grep', 'command', ['ag'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', [])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
    call denite#custom#var('grep', 'default_opts',
                \ [ '--skip-vcs-ignores', '--vimgrep', '--smart-case', '--hidden' ])
endif

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

" use cursor to move
call denite#custom#map('normal', '<Down>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('normal', '<Up>', '<denite:move_to_previous_line>', 'noremap')

nnoremap <silent><leader>fn :Denite -resume -cursor-pos=+1 -immediately<cr>
nnoremap <silent><leader>fp :Denite -resume -cursor-pos=+1 -immediately<cr>
nnoremap <silent><leader>fr :Denite -resume<cr>
nnoremap <silent><leader>ft :DeniteCursorWord tag<cr>
nnoremap <silent><leader>fg :DeniteCursorWord grep<cr>
nnoremap <silent><leader>gd :DeniteCursorWord gtags_def<cr>
nnoremap <silent><leader>gr :DeniteCursorWord gtags_ref<cr>
nnoremap <silent><leader>gc :DeniteCursorWord gtags_context<cr>
nnoremap <silent><leader>gg :DeniteCursorWord gtags_grep<cr>
" }}}

" vim-move {{{
let g:move_map_keys = 0
vmap <c-down> <Plug>MoveBlockDown
vmap <c-up> <Plug>MoveBlockUp
nmap <c-down> <Plug>MoveLineDown
nmap <c-up> <Plug>MoveLineUp
" }}}

" rust {{{
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
" }}}

" Misc {{{
set guicursor=

if &term =~ "rxvt.*"
    execute "set <m-j>=\ej"
    execute "set <m-l>=\el"
    execute "set <m-c>=\ec"
    execute "set <m-v>=\ev"
endif

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
