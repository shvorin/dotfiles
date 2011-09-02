" ~/.vimrc

set modeline
set modelines=5
set nowrap
set hlsearch
syntax on
set fileencodings=utf8,cp1251
set sc
set so=3
set title

au BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif
