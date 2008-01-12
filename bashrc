# -*- mode: sh; -*-

## ----------
## -- Random Customizations and Configurations
## ----------

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Use this variable to customize for specific computers
[ -r $HOME/.sysname ] && export SYSNAME=`cat $HOME/.sysname`

# don't put duplicate lines in the history.
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# enable programmable completion
[ -f /etc/bash_completion ] && . /etc/bash_completion

# enable color ls
if [ "$TERM" != "dumb" ]; then
    [ -x /usr/bin/dircolors ] && eval "`dircolors -b`"
    if [ "$SYSNAME" = Calliope ]; then
        alias ls='ls -G'
    else
        alias ls='ls --color=auto'
    fi
fi

# Thanks to Bill Clementson for parts of this snippet
# http://bc.tech.coop/
function start_or_join_screen {
    if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" ]
    then
        STARTED_SCREEN=1 ; export STARTED_SCREEN
        sleep 1
        if screen -RR
        then
            test -e /tmp/nex3_screen_abnormal_exit || exit 0
            rm /tmp/nex3_screen_abnormal_exit
        else
            echo "Screen failed! continuing with normal bash startup"
        fi
    fi
}

function descreen {
    touch /tmp/nex3_screen_abnormal_exit
    screen -X quit
}

## Pretty Prompt Configuration

# Black/white prompt for dumb terminals
PROMPT_MAIN='\u@\h'
PROMPT_DIR='\w'

# Colorful prompt for smart terminals
if [ "$TERM" != "dumb" ]; then
    PROMPT_MAIN="\[\033[01;32m\]\u@\h\[\033[00m\]"
    PROMPT_DIR="\[\033[01;34m\]\w\[\033[00m\]"
fi

# Escape directories so the forward slashes
# don't conflict with sed
function sed_escape_dirs {
    echo "$@" | sed 's/\//\\\//g'
}

# Same as pwd,
# but replaces $HOME with ~
function pwd_with_tilde {
    pwd | sed s/"`sed_escape_dirs $HOME`"/~/
}

# If pwd is long,
# sets it on a separate line from the rest of the prompt.
# Otherwise, restores normal prompt.
function reset_prompt {
    if [ `pwd_with_tilde | wc -c` -lt '35' ]; then
        PS1="$PROMPT_MAIN:$PROMPT_DIR$ "
    else
        PS1="\n$PROMPT_DIR\n$PROMPT_MAIN$ "
    fi
}

## ----------
## -- Personal Aliases and Advice
## ----------

function my_cd {
    cd "$@";
    reset_prompt
    ls
}

alias cd='my_cd'
alias ssh='ssh -X'
alias ssh-home='ssh -p 2042 nex3@nex3.mine.nu'
alias svni='svn --ignore-externals'
alias temacs="emacs -nw"
alias pager='less'
alias rl='rlwrap'

## ----------
## -- New Lookup Paths
## ----------

if [ -e $HOME/gems ]
then
    export GEM_PATH=$HOME/gems:$GEM_PATH
    export PATH=$HOME/gems/bin:$PATH
fi

if [ -e $HOME/lib/python ]
then
    export PYTHONPATH=$HOME/lib/python:$PYTHONPATH
fi

export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export C_INCLUDE_PATH=$HOME/include:$C_INCLUDE_PATH
export PATH=$HOME/bin:/var/lib/gems/1.8/bin/:/usr/local/bin:$PATH
export EDITOR='emacs -nw'
export DARCS_EDITOR=emacsclient
export SVN_EDITOR='emacs -nw'

## ----------
## -- Initialization Commands
## ----------

# Set title
if [ $TERM = "xterm" ]
then
    echo -ne '\033]0;Terminal\007'
fi

start_or_join_screen

# Everything after here will only be executed on an in-screen terminal

cd .

