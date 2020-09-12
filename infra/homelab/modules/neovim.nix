# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:
{
  nixpkgs.config.vim = {
    ftNixSupport = true;
  };

  environment.variables = {
    EDITOR = [ "${pkgs.neovim}/bin/nvim" ];
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withPython3 = true;
    configure = {
      customRC = ''
        " _   _ ___
        " | | | |_ _|
        " | | | || |
        " | |_| || |
        "  \___/|___|

        "Enable terminal colors 256
        set termguicolors

        "Set the background color of nvim
        set background=dark

        "Default colorscheme
        "colorscheme evening

        "Onedark colorscheme
        "colorscheme onedark
        "let g:airline_theme="onedark"

        "Gruvbox colorscheme
        colorscheme gruvbox
        let g:airline_theme="gruvbox"

        "  _____    _ _ _
        " | ____|__| (_) |_ ___  _ __
        " |  _| / _` | | __/ _ \| '__|
        " | |__| (_| | | || (_) | |
        " |_____\__,_|_|\__\___/|_|

        " Remap leader key to ,
        " With a map leader it's possible to do extra key combinations
        " like <leader>w saves the current file
        let mapleader = ","
        let g:mapleader = ","
        let maplocalleader = "."

        " Use Unix as the standard file type
        set ffs=unix,dos,mac

        set encoding=UTF-8

        " == Python provider configurations == "
        let g:python_host_prog='/usr/bin/python2'
        let g:python3_host_prog='/usr/bin/python3'

        "enable loading the plugin files for specific file types
        filetype plugin indent on

        "Switch on syntax highlighting.
        syntax on

        "set assembly language file to use nasm
        let filetype_i = "nasm"
        let g:asmsyntax = "nasm"

        "Set clipboard to the + and * registers
        if has('clipboard')
            set clipboard+=unnamedplus,unnamed
        endif

        "Restore cursor to file position in previous editing session
        "This autocommand jumps to the last known position in a file
        "just after opening it, if the '" mark is set: >
        :au BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
        \ |   exe "normal! g`\""
        \ | endif

        "Keep a backup copy of a file when overwriting it.This also sets the 'undofile' option, if
        "available.  This will store the multi-level undo information in a file.  The result is
        "that when you change a file, exit Vim, and then edit the file again, you can undo
        "the changes made previously.
        if has("vms")
            set nobackup
        else
            set backup
            if has('persistent_undo')
                set undofile
                set undolevels=30000
                set undoreload=30000
            endif
        endif

        "for vim's swap ; undo and backup organization
        set backupdir=/tmp// "Location for backup of files before editing "
        set directory=/tmp// "The location of swap files ,ie buffers that have not been save ie in memory
        set undodir=/tmp//   "Location for storing undo tree of the edited file"


        "Do not keep a backup or .swp file. I don't like to have junk
        "files, my source is anyway in cvs/svn/p4/git.
        set backup       " create a backup of the file before editing
        set undofile     " enable undofile , which helps you undo a lot and redo also a lot
        set noswapfile     " enable saving unsaved/unwritten files in a *.swp file

        set nocompatible   " Use Vim defaults (much better!), Vi is for 70's programmers!

        set expandtab      " always expands tab to spaces. It is good when peers use different editor.

        set wildmenu       "Display completion matches in a status line.  That is when you type <Tab>
                          "and there is more than one match.

        set termguicolors  "Set true colours in terminal

        set completeopt=menuone,preview,noinsert " Don't let autocomplete affect usual typing habits

        set bs=2            " allow backspacing over everything in insert mode

        set hidden          " This option allows you to switch between multiple buffers
                            "without saving a changed buffer

        set mouse=a         " Automatically enable mouse usage

        set mousehide       " Hide the mouse pointer while typing.

        set incsearch       " highlight search string as search pattern is entered

        set hlsearch         "disables last search hilighting

        "set wildmode=full    " get bash-like tab completions with longest and list

        set number           " Show line numbers

        set wrap             " Automatically wrap text that extends beyond the screen length.

        set backspace=indent,eol,start " Fixes common backspace problems

        set laststatus=2      " Status bar

        set showbreak=>>>\ \ \    " Wrap-broken line prefix

        set textwidth=79      " Line wrap (number of cols)

        set showmatch         " Highlight matching brace

        set complete+=kspell  "text complete with CTRL-N or CTRL-P

        "set spell        " Enable spell-checking

        set spelllang=en_us

        set visualbell     "Use visual bell (no beeping)

        set ignorecase      "Always case-insensitie

        set smartcase     " Enable smart-case search

        set autoindent      " Auto-indent new lines

        set wildignorecase    "case insensitive auto completion

        set shiftwidth=4      " Number of auto-indent spaces

        set smartindent      " Enable smart-indent

        set smarttab      " Enable smart-tabs

        set softtabstop=4     " Number of spaces per Tab

        set confirm      " Prompt confirmation dialogs

        set ruler             " Show row and column ruler information

        set cmdheight=2       "Command line height

        set autowriteall      "Auto-write all file changes

        set history=300       "Set the history size to maximum. by default it is 20

        set list          " Display unprintable characters f12 - switches

        set listchars=tab:\ ,trail:,extends:,precedes: " Unprintable chars mapping


        " Enable folding
        set foldmethod=indent
        set foldlevel=99

        "Enable Tags
        set tags=tags

        "Disable default mappings from omni for sql
        let g:omni_sql_no_default_maps = 1

        "Remove Trailing whitespaces in specified documents on write of buffer
        "autocmd FileType c,cpp,java,php autocmd BufWritePre <buffer> %s/\s\+$//e

        "Remove Trailing whitespaces in all files
        autocmd BufWritePre * %s/\s\+$//e

        " _____                   _             _
        " |_   _|__ _ __ _ __ ___ (_)_ __   __ _| |
        "   | |/ _ \ '__| '_ ` _ \| | '_ \ / _` | |
        "   | |  __/ |  | | | | | | | | | | (_| | |
        "   |_|\___|_|  |_| |_| |_|_|_| |_|\__,_|_|

        " open new split panes to right and below
        set splitright
        set splitbelow

        "Always start terminal in insert mode
        autocmd TermOpen * startinsert

        "  _____ _ _      _
        " |  ___(_) | ___| |_ _   _ _ __   ___  ___
        " | |_  | | |/ _ \ __| | | | '_ \ / _ \/ __|
        " |  _| | | |  __/ |_| |_| | |_) |  __/\__ \
        " |_|   |_|_|\___|\__|\__, | .__/ \___||___/
        "                     |___/|_|

        autocmd FileType dart :packadd dart-vim-plugin
        autocmd FileType go :packadd vim-go
        autocmd FileType graphql :packadd vim-graphql
        autocmd FileType ts :packadd yats-vim
        autocmd FileType md :packadd vim-markdown
        autocmd FileType MD :packadd vim-markdown
        autocmd FileType markdown :packadd vim-markdown
      '';
      vimPlugins = {
        # loaded on launch
        start = [
          coc-nvim
          fzf-vim
          gruvbox
          neoformat
          nerdcommenter
          nerdtree
          syntastic
          tabular
          vim-airline
          vim-airline-themes
          vim-dadbod
          vim-devicons
          vim-easymotion
          vim-fugitive
          vim-gitgutter
          vim-multiple-cursors
          vim-polyglot
          vista-vim
        ];

        # needs to be manually loaded via autocmd FileType extension :packadd packagename
        opt = [
          dart-vim-plugin
          vim-graphql
          vim-markdown
          yats-vim
        ];

        # To automatically load a plugin when opening a filetype, add vimrc lines like:
        # autocmd FileType php :packadd phpCompletion
      };
    };
  };
