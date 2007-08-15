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
    
    # set a fancy prompt
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='\u@\h:\w$ '
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

if [ "$SYSNAME" == "SAO Mac 2" ]
then
    export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
    alias ls='ls -G'
fi

function cd1 {
    cd "$@";
    wd=`pwd`;
    if [ "$TERM" != "dumb" ]; then
        if [ ${#wd} -lt '35' ]; then
            PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
        else
            PS1='\n\[\033[01;34m\]\w\[\033[00m\]\n${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]\$ '
        fi
    fi

    if [ `/bin/ls | wc -l` -lt '20' ]; then
        ls
    else
        echo '...'
    fi
}

function exists {
    file=`whereis "$@"`;
    [ ${#file} -gt 1 ]
}

alias cd='cd1'
alias ssh='ssh -X'

alias temacs="`which emacs` -nw"

if exists pager
then false;
else
    alias pager="less"
fi

if exists rlwrap
then
    if exists rl
    then false;
    else
        alias rl="rlwrap"
    fi
fi

if [ -e $HOME"/gems" ]
then
    export GEM_PATH=$HOME"/gems:"$GEM_PATH
    export PATH=$HOME"/gems/bin:"$PATH
fi

if [ -e "/usr/lib/junit4.3" ]
then
    export CLASSPATH="/usr/lib/junit4.3/junit-4.3.1.jar:"$CLASSPATH
fi

export PATH=$HOME"/bin:/var/lib/gems/1.8/bin/:"$PATH
export SVN_EDITOR='/usr/bin/emacs -nw'

ls
