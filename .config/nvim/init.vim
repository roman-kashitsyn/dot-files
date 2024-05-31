call plug#begin()
Plug 'robertmeta/nofrils'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" Go setup
Plug 'nvim-treesitter/nvim-treesitter'
" End go setup
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'dense-analysis/ale'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'itchyny/lightline.vim'
Plug 'rhysd/vim-grammarous'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-unimpaired'
Plug 'lervag/vimtex'
Plug 'machakann/vim-highlightedyank'
call plug#end()

set termguicolors

let g:nofrils_strbackgrounds=1
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'background': 'dark',
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead',
      \   'tags': 'gutentags#statusline'
      \ },
      \ }

colorscheme nofrils-dark
set background=dark

let g:grammarous#languagetool_cmd = '/usr/local/bin/languagetool'
let g:grammarous#use_location_list = 1

let g:deoplete#enable_at_startup = 1

set wildignore+=*/tmp/*,*/target/*,*/bazel-*/*,bazel-*/*,*.so,*.swp,*.zip

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set ignorecase
set smartcase
set number relativenumber

set grepprg=rg\ --vimgrep
set grepformat^=%f:%l:%c:%m

let mapleader="\<Space>"

if has("unnamedplus")
    set clipboard=unnamedplus
else
    set clipboard=unnamed
endif

" Goyo

let g:goyo_width=200

nmap <Leader>l <Plug>(Limelight)
xmap <Leader>l <Plug>(Limelight)
nnoremap <C-g> :Goyo<CR>
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

augroup markdown
  autocmd BufNewFile,BufRead *.md set filetype=markdown
  autocmd FileType markdown Goyo
  autocmd FileType markdown setlocal spell spelllang=en_us
  autocmd FileType markdown set cursorline
  autocmd FileType markdown set nocursorcolumn
  autocmd FileType markdown set shiftwidth=2
augroup END

augroup pollen
  autocmd BufNewFile,BufRead *.html.pm set ft=pollen
  autocmd FileType pollen imap <buffer> @ ◊
  autocmd FileType markdown setlocal spell spelllang=en_us
  autocmd FileType markdown set cursorline
  autocmd FileType markdown set shiftwidth=2
augroup END

" Build

map <leader>bb :Dispatch<CR>
map <leader>gf :GFiles<CR>
map <leader>gb :Buf<CR>
map <leader>gg :G<CR>

set autoread                                                                                                                                                                                    
au CursorHold * checktime

tnoremap <C-[> <C-\><C-n>

if has('persistent-undo')
    set undofile
endif

if exists('g:neovide')
    let g:neovide_cursor_animation_length=0
    let g:neovide_fullscreen=v:true
    set guifont=PragmataPro\ Liga:h11
endif
