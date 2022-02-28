" turn on line numbering
set number
" turn on cursor line
set cursorline
" do not break words at line wrap
set linebreak
" wrapped line prefix chars
set showbreak=+++
" wrap after 100 chars
set textwidth=100
" highlight matching braces
set showmatch
" flash screen instead of a bell
set visualbell
" search highlight
set hlsearch
" switch to case sensitive when search has uppercase char
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




" Statusline

" Always show status line
set laststatus=2

"   current time
set statusline=%{strftime(\"%H:%M\")}
"   FAIL or OK depending on last shell cmd
set statusline+=\ \(%{v:shell_error?\"FAIL:\":\"ok:\"}
"   exit code of last shell command
set statusline+=\%{v:shell_error}\)
"   modified flag
set statusline+=\%m
"   last 22 chars of file name
set statusline+=\ \%-.22F
"   file type
set statusline+=%-y
"   left-right separator
set statusline+=%=
" character encoding
set statusline+=%{strlen(&fenc)?&fenc:&enc}
"   last operator
set statusline+=\ \[%{v:operator}\]
"   current line and column
set statusline+=\ %5.l,%-3.c
"   ASCII code of current char
set statusline+=x%02.B
"   scroll position percentage
set statusline+=\ %3.p%%

colorscheme desert

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

" set <leader> as ű for hungarian keyboards
let mapleader = "ű"
nnoremap <C-t>  <Esc>:tabnew<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt

set pastetoggle=<C-<F7>>

" Make constants readable on projector as well
highlight Constant ctermbg=black ctermfg=green

" edit vim config in a split
nnoremap <leader>evf :vsplit  $MYVIMRC<CR>
" reload vim confiv
nnoremap <leader>rvf :so $MYVIMRC<CR>

""""""""""""""""""""""""""""""""
" Functions
""""""""""""""""""""""""""""""""
function! IsMachine(hostname)
    if match(system("hostname"), a:hostname) >= 0
        return 0
    else
        return -1
endfunction

function! OpenAll(command, pattern_to_position_cursor)
    for f in split(system(a:command .  ' 2>/dev/null | grep -Ev ''[.](swp|pyc|pickle)$'''), "\n")
        execute 'tabe ' . f
        if a:pattern_to_position_cursor != ''
            execute '/' . a:pattern_to_position_cursor
            nohl
        endif
    endfor
endfunction


" The command :GitGrep pattern will open all the files in a git repository
" matching pattern in a case sensitive manner
function! GitGrep(pattern)
    call OpenAll('git grep -l -- ''' . a:pattern . ''' *', a:pattern)
endfunction
command! -nargs=1 GitGrep call GitGrep('<args>')
command! -nargs=1 GG call GitGrep('<args>')
command! -nargs=0 GitGrepCurrentWord call GitGrep(expand('<cword>'))
command! -nargs=0 GGCW call GitGrep(expand('<cword>'))

" The command :Grep pattern will open all the files matching pattern
" in a case sensitive manner
function! Grep(pattern)
    call OpenAll('grep -Rlm1 -- ''' . a:pattern . ''' *', a:pattern)
endfunction
command! -nargs=1 Grep call Grep('<args>')
command! -nargs=1 G call Grep('<args>')
command! -nargs=0 GrepCurrentWord call Grep(expand('<cword>'))
command! -nargs=0 GCW call Grep(expand('<cword>'))

" Press Ctrl-g to open all files containing the word under the cursor
nnoremap <c-g> :call Grep(expand('<cword>'))<CR>

" The command :Find d*f.txt will open all the files matching the given path,
" in a case insensitive manner, e.g. 'dir/File.txt', in a new tab.
function! Find(pattern)
    call OpenAll('find -L * -ipath ''*' . a:pattern . '''', '')
endfunction
command! -nargs=1 Find call Find('<args>')
command! -nargs=1 F call Find('<args>')
command! -nargs=0 FindCurrentWord call Find(expand('<cword>'))
command! -nargs=0 FCW call Find(expand('<cword>'))


""""""""""""""""""""""""""""""""
" Auto-install vim plugin manager
""""""""""""""""""""""""""""""""
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
    silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

""""""""""""""""""""""""""""""""
" Plugin list
""""""""""""""""""""""""""""""""
silent! call plug#begin()
    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
call plug#end()

""""""""""""""""""""""""""""""""
" NERDTree 
""""""""""""""""""""""""""""""""
cnoremap <leader>n :NERDTreeFocus<CR>
"nnoremap <C-n> :NERDTree<CR>
nnoremap <leader>n :NERDTreeToggle<CR>
"nnoremap <C-f> :NERDTreeFind<CR>
