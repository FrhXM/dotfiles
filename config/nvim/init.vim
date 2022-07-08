"
"          ███████████████████████████
"          ███████▀▀▀░░░░░░░▀▀▀███████
"          ████▀░░░░░░░░░░░░░░░░░▀████
"          ███│░░░░░░░░░░░░░░░░░░░│███
"          ██▌│░░░░░░░░░░░░░░░░░░░│▐██
"          ██░└┐░░░░░░░░░░░░░░░░░┌┘░██
"          ██░░└┐░░░░░░░░░░░░░░░┌┘░░██
"          ██░░┌┘▄▄▄▄▄░░░░░▄▄▄▄▄└┐░░██
"          ██▌░│██████▌░░░▐██████│░▐██
"          ███░│▐███▀▀░░▄░░▀▀███▌│░███
"          ██▀─┘░░░░░░░▐█▌░░░░░░░└─▀██
"          ██▄░░░▄▄▄▓░░▀█▀░░▓▄▄▄░░░▄██
"          ████▄─┘██▌░░░░░░░▐██└─▄████
"          █████░░▐█─┬┬┬┬┬┬┬─█▌░░█████
"          ████▌░░░▀┬┼┼┼┼┼┼┼┬▀░░░▐████
"          █████▄░░░└┴┴┴┴┴┴┴┘░░░▄█████
"          ███████▄░░░░░░░░░░░▄███████
"          ██████████▄▄▄▄▄▄▄██████████
"          ███████████████████████████
"
"      You are about to experience a potent
"        dosage of Vim. Watch your steps.
"
"  ╔══════════════════════════════════════════╗
"  ║             HERE BE VIMPIRES             ║
"  ╚══════════════════════════════════════════╝
                 
" VARS {{{
set hidden                  " this option is ignored Worning in buffers 
set autoread                " autoload file changes
set autowriteall            " autosave files
set background=dark         " dark colorscheme
set completeopt=menu,menuone,noselect
set cursorline              " set cursor line
set diffopt+=vertical       " split diffopt in vertical mode
set encoding=utf-8          " set the character encoding to UTF-8
set expandtab               " convert tabs to the spaces
set foldlevel=2             " sets the fold level
set foldmethod=indent       " type of indentation
set foldnestmax=10          " sets the maximum nesting of folds
" set gcr=a:blinkon0          " disable cursor blinking
set guioptions=             " remove all GUI components and options.
set history=1000            " store lots of :cmdline history
set hlsearch                " highlights the string matched by the search
set ignorecase              " make searching case insensitive
set incsearch               " incremental search
set nobackup                " disable backups
set nocompatible            " use Vim settings, rather then Vi
set nofoldenable            " when off, all folds are open when open a new file
set number                  " show Number Line 
set relativenumber          " show RelativeNumber
set numberwidth=5           " width of Number Line 
set noshowmode              " don't show mode as we use a status line plugin
set noswapfile              " disable swapfile
set nowrap                  " wrap lines
set scrolloff=10            " keep cursor at the minimum 10 rows from the screen borders
set shiftwidth=2            " 2 spaces
set showmatch               " show match brackets
set sidescroll=1            " incrementally scroll one character
set signcolumn=yes          " always show signcolumns
set smartcase               " unless the query has capital letters
set splitbelow              " open new split below
set splitright              " open new split right
set tabstop=2               " 2 spaces
set termguicolors           " enable True color
set colorcolumn=80          " Line in Coulmn (80<=Caractère <=120)
set ttyfast                 " always assume a fast terminal
set undodir=~/.config/nvim/undo-dir " setup undo directory
set undofile                " save undo chages even after computer restart
set updatetime=250          " reduce update time in Vim
set wildmenu                " visual autocomplete for command menu
" }}}

" ╭────────────────────────────────────────────────────────────────────╮
" │                               Plugins                              │
" ╰────────────────────────────────────────────────────────────────────╯
call plug#begin('~/.config/nvim/plugins')
        " --- Appearance ---"
  Plug 'tiagovla/tokyodark.nvim'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'sheerun/vim-polyglot'

       " --- Programming ---"
  Plug 'mattn/emmet-vim'
  Plug 'jiangmiao/auto-pairs'
  Plug 'alvan/vim-closetag'
  Plug 'lilydjwg/colorizer'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-commentary'

       " ----- Tools -----"
  Plug 'junegunn/fzf.vim'
  Plug 'preservim/nerdtree'
  Plug 'ryanoasis/vim-devicons'
call plug#end()

" ============================== Theme =================================
colorscheme tokyodark
lua << END
require('lualine').setup()
END
" enable transparent terminal bg
hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi EndOfBuffer guibg=NONE ctermbg=NONE

" ============================ Emmet ===================================
let g:user_emmet_mode='a'
let g:user_emmet_leader_key=','

" ============================ Fzf =====================================
nnoremap <C-f> :Files<CR>
nnoremap <C-p> :BLines<CR>
nnoremap <C-g> :GFiles<CR>

" ========================= NERDTree ==================================
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-g> :NERDTreeFind<CR>
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif 

" ╭────────────────────────────────────────────────────────────────────╮
" │                               MyKeys                               │
" ╰────────────────────────────────────────────────────────────────────╯
" ============ Toggle the highlighting or clear the highlight =========
noremap <silent> <leader>hl :set hlsearch!<CR>
noremap <silent> <SPACE> :noh<CR>

" ============== Move the current line up or down =====================
noremap <silent> <C-k> :move -2<CR>
noremap <silent> <C-j> :move +1<CR>

"=========== Center-, right-, or left-align one or more lines. ========
noremap <silent> <leader>c :center<CR>
noremap <silent> <leader>r :right<CR>
noremap <silent> <leader>l :left<CR>

" ============================= Tabs ===================================
nnoremap <Tab> gt
nnoremap <S-Tab> gT
nnoremap <silent> <C-e> :tabnew<CR>
nnoremap <C-x> :tabclose<CR>

" ===================== Normal mode remappings ========================
nnoremap <C-q> :q!<CR>
nnoremap <C-s> :w<CR>
nnoremap <C-b> :bd<CR>

" ╭────────────────────────────────────────────────────────────────────╮
" │                          Arrow Keys                                │
" ╰────────────────────────────────────────────────────────────────────╯
" ==================== Disable arrow keys for normal mode =============
noremap <up> <Nop>
noremap <down> <Nop>
noremap <left> <Nop>
noremap <right> <Nop>
" ==================== Disable arrow keys for insert mode  =============
inoremap <up> <Nop>
inoremap <down> <Nop>
inoremap <left> <Nop>
inoremap <right> <Nop>
" ==================== Disable arrow keys for visual mode  =============
vnoremap <up> <Nop>
vnoremap <down> <Nop>
vnoremap <left> <Nop>
vnoremap <right> <Nop>
" =================== Disable arrow keys for command mode ==============
cnoremap <up> <Nop>
cnoremap <down> <Nop>
cnoremap <left> <Nop>
cnoremap <right> <Nop>
