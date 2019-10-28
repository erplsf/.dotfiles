" if empty(glob('~/.vim/autoload/plug.vim'))
"   silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
" endif

call plug#begin('~/.local/share/nvim/plugged')

" General plugin section
Plug 'tpope/vim-eunuch' " nice unix commands inside vim
Plug 'tpope/vim-surround' " nice surrounding plugin
Plug 'w0rp/ale' " linting/fixing for the future!
" ? mattn/emmet-vim ?
" ? editorconfig/editorconfig-vim ?
" ? terryma/vim-multiple-cursors ?
Plug 'tpope/vim-fugitive' " Git from the Vim!
Plug 'tpope/vim-rhubarb'

Plug 'tpope/vim-vinegar'

" JS/Typescript/React plugins section
Plug 'pangloss/vim-javascript'
Plug 'jason0x43/vim-js-indent'
Plug 'mxw/vim-jsx'
Plug 'Quramy/tsuquyomi'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

Plug 'sbdchd/neoformat'
Plug 'prettier/vim-prettier', { 'do': 'npm install' }

Plug 'godlygeek/tabular'

" terraform
Plug 'hashivim/vim-terraform'

Plug 'triglav/vim-visual-increment'

Plug 'scrooloose/nerdcommenter'

Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & npm install' }
" theme

Plug 'chriskempson/base16-vim'

Plug 'ctrlpvim/ctrlp.vim'

call plug#end()

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_working_path_mode = 'rca'

let g:terraform_align=1
let g:terraform_fmt_on_save=1

filetype indent off

set updatetime=100
let g:mkdp_preview_options = {
    \ 'mkit': {"level": 2},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {}
    \ }


syntax on

colorscheme base16-tomorrow-night-eighties
set termguicolors

" On pressing tab, insert 2 spaces
set expandtab
" show existing tab with 2 spaces width
set tabstop=2
set softtabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2

" Change the leader
let mapleader=","

let g:netrw_dirhistmax = 0
