" turn on line numbering
set number
" turn on cursor line
set cursorline
" do not break words at line wrap
set linebreak
" wrapped line chars
set showbreak=+++
" wrap after 100 chars
set textwidth=100
" highlight matching braces
set showmatch
" flash screen instead of a bell
set visualbell

" search highlight
set hlsearch
" switch to case sensitive whe nsearch has uppercase char
set smartcase
" ignore case when searching
set ignorecase
" incremental search w/ partial matches
set incsearch

" new lines inherit from prev
set autoindent
" auto C indenting
set cindent

set shiftwidth=4
set softtabstop=4
set tabstop=4
" convert tabs to spaces
set expandtab
" auto increase/reduce indent when needed
set smartindent
" insert tabstop no of spaces for tab
set smarttab

" show cursor position
set ruler

set undolevels=1000
" allow backspacing over indentation, line breaks and insertion start
set backspace=indent,eol,start

colorscheme monokai

" Make frequent typos work.
command! Q :q
command! Qall :qall
command! QAll :qall
command! W :w
command! Wq :wq
command! WQ :wq
command! Wqall :wqall
command! WQall :wqall
command! WQAll :wqall

