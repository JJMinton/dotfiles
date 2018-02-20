source ~/dotfiles/vimrc/vimrc_colors
source ~/dotfiles/vimrc/vimrc_tabs
source ~/dotfiles/vimrc/vimrc_arrows
source ~/dotfiles/vimrc/vimrc_beeps
source ~/dotfiles/vimrc/vimrc_numbering
"source ~/dotfiles/vimrc/vimrc_spell
source ~/dotfiles/vimrc/vimrc_netrw "replacement for NERDTree

" no vi compat
set nocompatible
setlocal foldmethod=indent

" filetype func off
filetype off

" initialize vundle
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
" start- all plugins below

Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'aperezdc/vim-template'
Plugin 'davidhalter/jedi-vim'
"Plugin 'SirVer/ultisnips'
Plugin 'andviro/flake8-vim'

" stop - all plugins above
call vundle#end()

" filetype func on
filetype plugin indent on

"lightline fix
set laststatus=2

" Customize the settings for vim-template plugin
let g:email = "jeremy.minton@eigentech.com"
let g:user = "jeremy-minton"
let g:fdocstr = "function docstring (to change)"
let g:cdocstr = "class docstring (to change)"

cabbrev q qa
cabbrev x xa
command W w

