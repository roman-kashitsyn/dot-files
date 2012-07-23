set nocompatible
color desert

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set nu

set encoding=utf8
set termencoding=utf-8

set nobackup
set nowritebackup
set noswapfile
set autoindent
set smarttab

filetype indent on
filetype on
filetype plugin on

let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell
set noerrorbells

autocmd BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
      \ if &ft =~# '^\%(conf\|modula2\)$' |
      \   set ft=markdown |
      \ else |
      \   setf markdown |
      \ endif

let Tlist_Ctags_Cmd='/usr/bin/ctags'
map <C-F12> :TlistToggle<CR>
map <C-F11> :TlistUpdate<CR>

let python_highlight_all = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete

