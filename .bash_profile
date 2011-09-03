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

export MOSMLHOME=/opt/mosml
export JAVA_HOME=/opt/JDKs/jdk1.5.0_01

export JSCP=~/Projects/JScp/src/scps
export XERCES=/usr/share/java
export SAXON_HOME=/opt/saxon

USERNAME=""
GS_LIB=/usr/share/fonts/default/Type1-Cyr

export USERNAME GS_LIB
export CVS_RSH=ssh
