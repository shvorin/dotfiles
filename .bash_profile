# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

export CVS_RSH=ssh
export RSYNC_RSH=ssh

export HISTFILESIZE=1000
export HISTSIZE=1000

# this is workaround for buggy versions of bash
export OSTYPE

USERNAME=""
GS_LIB=/usr/share/fonts/default/Type1-Cyr

export USERNAME GS_LIB
