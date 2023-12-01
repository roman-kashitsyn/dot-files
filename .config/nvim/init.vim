let g:syntastic_rust_checkers = []
let g:rustfmt_autosave = 1

"let g:airline_powerline_fonts = 1
"set laststatus=2
let g:nofrils_strbackgrounds=1
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'background': 'dark',
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead',
      \   'tags': 'gutentags#statusline'
      \ },
      \ }

let g:gutentags_modules = ['ctags']
let g:gutentags_project_root = ['.git']
let g:gutentags_add_default_project_roots=0
let g:gutentags_cache_dir = expand('~/.cache/tags')
augroup GutentagsStatusLineRefresher
    autocmd!
    autocmd User GutentagsUpdating call lightline#update()
    autocmd User GutentagsUpdated call lightline#update()
augroup END

colorscheme nofrils-dark
set background=dark

let g:grammarous#languagetool_cmd = '/usr/local/bin/languagetool'
let g:grammarous#use_location_list = 1

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

tnoremap <C-[> <C-\><C-n>

augroup autoformat_settings
  autocmd FileType bzl AutoFormatBuffer buildifier
  autocmd FileType rust AutoFormatBuffer rustfmt
augroup END

if has('persistent-undo')
    set undofile
endif

if exists('g:neovide')
    let g:neovide_cursor_animation_length=0
    let g:neovide_fullscreen=v:true
    set guifont=PragmataPro\ Liga:h11
endif
