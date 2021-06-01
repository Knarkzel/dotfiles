"  _       _ _         _           
" (_)_ __ (_) |___   _(_)_ __ ___  
" | | '_ \| | __\ \ / / | '_ ` _ \ 
" | | | | | | |_ \ V /| | | | | | |
" |_|_| |_|_|\__(_)_/ |_|_| |_| |_|
                                 
let mapleader=" "
set autochdir
set background=dark
set clipboard+=unnamedplus
set expandtab
set formatoptions-=cro
set hidden
set ignorecase
set lazyredraw
set mouse=a
set nobackup
set noshowmode
set noswapfile
set wildignore=*target/*,*.git/*,Cargo.lock
set path=**
set nowrap
set nowritebackup
set number relativenumber
set pumheight=10
set relativenumber
set shiftwidth=4
set showcmd
set showtabline=4
set signcolumn=yes
set smartcase
set smarttab
set switchbuf=usetab
set tabstop=4
set termguicolors
set wildmenu
set t_Co=256
syntax on

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin()
  Plug 'sainnhe/gruvbox-material'
  Plug 'sheerun/vim-polyglot'
call plug#end()

" update buffer when changed
set autoread
au CursorHold * checktime  

" epic saving folds
augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

" faster quitting
map Q :qa!<CR>

" dont create new lines
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" better version
noremap 0 ^

" colorscheme
colorscheme gruvbox-material
highlight SignColumn guibg=NONE

" clear searches with esc
nnoremap <silent> <ESC> :noh<CR><ESC>

" make netrw work as nerdtree
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1

" do not pair quotes in vim mode
let g:autoclose_vim_commentmode = 1

" mappings
nmap <Leader>e :edit .<CR>
nmap <Leader>f :find<space>*
nmap <Leader>o :e ~/.config/nvim/init.vim<CR>
nmap <Leader>t :tabnew<CR>:term<CR>i
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l
imap < <><Left>

" tab mappings
nmap <Leader>1 1gt
nmap <Leader>2 2gt
nmap <Leader>3 3gt
nmap <Leader>4 4gt
