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

" neovim compatibility for vim
if !has('nvim')
    Plug 'roxma/vim-hug-neovim-rpc'
endif

" airline
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
"
" Install rust-analyzer:
"   git clone https://github.com/rust-analyzer/rust-analyzer
"   cd rust-analyzer
"   cargo install-ra --server
Plug 'rust-lang/rust.vim'

" langserver
"
" After installation of coc.nvim run:
"   :CocInstall coc-rust-analyzer
"   :CocInstall coc-lists
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" tags
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'

call plug#end()
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

command EnableRustfmt let g:rustfmt_autosave = 1
command DisableRustfmt let g:rustfmt_autosave = 0
" }}}

" coc {{{
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes
let g:airline#extensions#coc#enabled = 1

" Disable underlining
autocmd FileType * highlight CocUnderline cterm=NONE gui=NONE

" completion menu settings {{{
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <tab> to complete
inoremap <silent><expr><TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()

" Use <s-tab> to complete with previous
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <enter> to confirm
inoremap <expr><cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

" Use `[g` and `]g` to navigate diagnostics
nmap <silent>[g <Plug>(coc-diagnostic-prev)
nmap <silent>]g <Plug>(coc-diagnostic-next)

" Gotos
nmap <silent>gd <Plug>(coc-definition)
nmap <silent>gy <Plug>(coc-type-definition)
nmap <silent>gi <Plug>(coc-implementation)
nmap <silent>gr <Plug>(coc-references)

" Use `K` to show doc
nnoremap <silent>K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

" Rename
nmap <leader>rn <Plug>(coc-rename)
" Menu
nnoremap <silent><F5> :<C-u>CocList<cr>
" Show all diagnostics
nnoremap <silent><space>a  :<C-u>CocList diagnostics<cr>
" Show commands
nnoremap <silent><space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent><space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent><space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent><space>p  :<C-u>CocListResume<CR>

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')
" }}}

" tags {{{
" You can regenerate tags with `:GutentagsUpdate`
let g:gutentags_modules = ['ctags', 'gtags_cscope']
let g:gutentags_cache_dir = expand('~/.cache/tags')
let g:gutentags_plus_switch = 1
let g:gutentags_plus_nomap = 1

" Find the root directory of a project. If is not under revision system, just
" create `.root` empty file.
let g:gutentags_project_root = ['.git', '.svn', '.hg', '.root']

autocmd FileType c,cpp nnoremap <buffer><silent>gd :GscopeFind g <C-R><C-W><cr>
autocmd FileType c,cpp nnoremap <buffer><silent>gr :GscopeFind s <C-R><C-W><cr>
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
