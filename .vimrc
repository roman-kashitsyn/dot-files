set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'Blackrush/vim-gocode'
Bundle 'jansenm/vim-cmake'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'nelstrom/vim-visual-star-search'
Bundle 'google/vim-colorscheme-primary'

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set autoindent
set smarttab

set encoding=utf8
set termencoding=utf-8

if has("guirunning")
    set guifont=Meslo_LG_S:h11
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
endif

set nobackup
set nowritebackup
set noswapfile

set statusline=%<%F%m%r%h%w\ (%{&ff})\ [%Y]\ %=[%l,%v]\ [%L]\ %=[%3p%%]
set laststatus=2

filetype indent on
filetype on
filetype plugin on

set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell
set noerrorbells

au BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn set ft=markdown
au BufNewFile,BufRead *.gradle set ft=groovy

let Tlist_Ctags_Cmd='/usr/bin/ctags'
map <C-F12> :TlistToggle<CR>
map <C-F11> :TlistUpdate<CR>

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬

if has("autocmd")
    autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
    autocmd FileType go setlocal ts=8 sts=8 sw=8 noexpandtab
endif
