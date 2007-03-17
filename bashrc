# -*- mode: sh; -*-

if [ -e $HOME/.sysname -a -r $HOME/.sysname ]
then
    export SYSNAME=`cat $HOME/.sysname`
fi

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    if [ "$SYSNAME" != "SAO Mac 2" ]; then
        eval "`dircolors -b`"
    fi
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

function cd1 {
  cd "$@";
  wd=`pwd`;
  if [ `expr length "$wd"` -lt '35' ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
  else
    PS1='\n\[\033[01;34m\]\w\[\033[00m\]\n${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]\$ '
  fi
  if [ `ls | wc -l` -lt '20' ]; then
    ls --color=auto
  else
    echo '...'
  fi
}

alias cd='cd1'
alias ssh='ssh -X'

REAL_EMACS=`which emacs`
alias remacs=$REAL_EMACS
alias temacs="$REAL_EMACS -nw"
alias emacs="emacsclient -na $REAL_EMACS"

export PATH=$HOME"/bin:"$PATH
export SVN_EDITOR='/usr/bin/editor'

if [ "$SYSNAME" == "SAO Mac 2" ]
then
    export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
    alias ls='ls -G'
fi

ls
