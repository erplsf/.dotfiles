if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.local/share/nvim/plugged')

" General plugin section
Plug 'tpope/vim-eunuch' " nice unix commands inside vim
Plug 'tpope/vim-surround' " nice surrounding plugin
Plug 'w0rp/ale' " linting/fixing for the future!
" ? mattn/emmet-vim ?
" ? editorconfig/editorconfig-vim ?
" ? terryma/vim-multiple-cursors ?
Plug 'tpope/vim-fugitive' " Git from the Vim!

" JS/React plugins section
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

call plug#end()

" On pressing tab, insert 2 spaces
set expandtab
" show existing tab with 2 spaces width
set tabstop=2
set softtabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2
