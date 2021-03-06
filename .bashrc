# .bashrc
# Source global definitions

if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

. /etc/profile

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
fi

# User specific aliases and functions

if [ -f ~/.dir_colors ]; then
    eval `dircolors ~/.dir_colors`;
else
    eval `dircolors`
fi
alias sudobash="sudo -H bash -l"

export HISTCONTROL=erasedups
PATH=$HOME/SYSTEM/bin:$HOME/bin:$HOME/.cabal/bin:$PATH
BASH_ENV=$HOME/.bashrc
USERNAME="Artem"
PS1="\[\033[1;37;40m\]\h:\W>\[\033[0m\] "
PS2=">>"
INPUTRC="~/.inputrc"

LANG=ru_RU.utf8
LANGUAGE=ru_RU.utf8
LC_ALL=ru_RU.utf8

export LANG LANGUAGE LC_ALL
export USERNAME PS1 PS2 INPUTRC http_proxy BASH_ENV PATH

# Read first /etc/inputrc if the variable is not defined, and after the /etc/inputrc 
# include the ~/.inputrc
[ -z $INPUTRC ] && export INPUTRC=/etc/inputrc

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -lA'
    #alias l='ls -CF'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias untar='tar zvxf'

# needed to compile jscp without altering Makefile.inc
#export CPP="/lib/cpp -P -traditional -Dunix -Umsdos -Uwin32"

export HISTFILESIZE=1000
export HISTSIZE=1000

export EDITOR="emacs -nw"

export PYTHONSTARTUP=/home/art/.pystartup
export TERM=rxvt-256color

export QUARTUS_64BIT=1
