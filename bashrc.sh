## ----------
## -- Random Customizations and Configurations
## ----------

# If not running interactively, don't do anything
if [[ ! -z "$PS1" ]]; then

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
    if [ `uname` = Darwin ]; then
        alias ls='ls -G'
    else
        alias ls='ls --color=auto'
    fi
fi

# Thanks to Bill Clementson for parts of this snippet
# http://bc.tech.coop/
function start_or_join_screen {
    if [ "$TERM" != dumb -a "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" ]
    then
        STARTED_SCREEN=1 ; export STARTED_SCREEN
        sleep 1
        if screen -RR
        then
            test -e /tmp/${USER}_screen_abnormal_exit || exit 0
            rm /tmp/${USER}_screen_abnormal_exit
        else
            echo "Screen failed! continuing with normal bash startup"
        fi
    fi
}

function descreen {
    touch /tmp/${USER}_screen_abnormal_exit
    screen -X quit
}

## Rubygems documentation lookup
## From http://stephencelis.com/archive/2008/6/bashfully-yours-gem-shortcuts

gemdoc() {
  local gemdir=`gem env gemdir`
  gnome-open $gemdir/doc/`ls $gemdir/doc/ | grep $1 | sort | tail -1`/rdoc/index.html
}

_gemdocomplete() {
  COMPREPLY=($(compgen -W '$(ls `gem env gemdir`/doc)' -- ${COMP_WORDS[COMP_CWORD]}))
  return 0
}

complete -o default -o nospace -F _gemdocomplete gemdoc

## Pretty Prompt Configuration

START_GREEN=''
START_BLUE=''
START_RED=''
START_YELLOW=''
END_COLOR=''

# Colorful prompt for smart terminals
if [ "$TERM" != "dumb" ]; then
    START_GREEN="\[\033[01;32m\]"
    START_BLUE="\[\033[01;34m\]"
    START_RED="\[\033[01;31m\]"
    START_YELLOW="\[\033[01;33m\]"
    END_COLOR="\[\033[00m\]"
fi
# Black/white prompt for dumb terminals
PROMPT_MAIN="${START_GREEN}\u@\h${END_COLOR}"
PROMPT_DIR="${START_BLUE}\w${END_COLOR}"

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
#
# Also print git branch and rvm interpreter if available
function prompt_command {
    # Git branch stuff from escherfan on Reddit
    if [ -n ${GITBRANCH} ]; then
        BRANCH=`git name-rev HEAD 2> /dev/null | awk "{ print \\$2 }"`
        DIRTY=`[[ $(git status 2> /dev/null | tail -n1) != \
            "nothing to commit (working directory clean)" ]] && echo '·'`
        if [ $BRANCH ]; then
            BRANCH=" ${START_RED}($BRANCH${DIRTY})${END_COLOR}";
        fi
    else
        BRANCH='';
    fi

    if [ "$rvm_path" -a -e "$rvm_path/tmp/$$/prompt" ]
    then RVM_PROMPT=" ${START_YELLOW}(`cat "$rvm_path/tmp/$$/prompt"`)${END_COLOR}"
    else RVM_PROMPT=''
    fi

    if [ `pwd_with_tilde | wc -c` -lt '35' ]; then
        PS1="$PROMPT_MAIN:$PROMPT_DIR$BRANCH$RVM_PROMPT$PROMPT_VAR$ "
    else
        PS1="\n$PROMPT_DIR$BRANCH$RVM_PROMPT\n$PROMPT_MAIN$PROMPT_VAR$ "
    fi
}

export PROMPT_COMMAND="if type prompt_command &> /dev/null; then prompt_command; fi"

## ----------
## -- Personal Aliases and Advice
## ----------

function advise {
    name="advice-$1-$2"
    if type -p "$name"; then return; fi

    alias super="$1"
    eval "
function $name {
    $3
}
alias $1=$name
"
    unalias super
}

advise cd with-ls 'super "$@" && ls'

function ssh-fn {
    eval "
function $1 {
   if isatty out
   then
       ssh \`$1 \$@\`
   else
       echo $2
   fi
}
"
}

advise ssh with-x 'super -X "$@"'
alias home='ssh -p 2042 nex3@home.nex-3.com'
alias svni='svn --ignore-externals'
alias pager='less'
alias rl='rlwrap'

## ----------
## -- New Lookup Paths
## ----------

function refresh-path {
    PATH=$original_path

    if [ -e $HOME/gems ]; then PATH=$HOME/gems/bin:$PATH; fi
    PATH=/usr/local/heroku/bin:/var/lib/gems/1.8/bin/:/usr/local/bin:$PATH

    for bindir in $HOME/bin/*; do
        if [[ -d "$bindir" && ! ":$PATH" =~ ":$bindir" ]]; then
            PATH="$bindir:$PATH"
        fi
    done
    export PATH=$HOME/bin:$PATH
}

# For some reason this doesn't persist within screen.
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/lib:$LD_LIBRARY_PATH

if [ ! "$STARTED_SCREEN" ]
then
    export original_path=$PATH

    if [ -e $HOME/gems ]; then export GEM_PATH=$HOME/gems:$GEM_PATH; fi

    if [ -e $HOME/.rip ]
    then
        RIPDIR=/home/nex3/.rip
        RUBYLIB="$RUBYLIB:$RIPDIR/active/lib"
        PATH="$PATH:$RIPDIR/active/bin"
        export RIPDIR RUBYLIB PATH
    fi

    export PYTHONPATH=$HOME/lib/python:/usr/local/lib/python2.6/site-packages:/usr/local/lib/python2.6/dist-packages:$PYTHONPATH
    export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
    export C_INCLUDE_PATH=$HOME/include
    export INFOPATH=$HOME/.info:$INFOPATH
    export EDITOR=em
    export DARCS_EDITOR=em
    export SVN_EDITOR=em

    refresh-path
fi

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

if [ "$TERM" != "dumb" ]; then
    if [ `uname` = Darwin ]; then ls -G
    else ls --color=auto
    fi
else
    ls
fi

fi

# This should be required outside of the interactive-only
# and top-level-of-screen-only blocks,
# because we need the rvm command to be a function
# so it can modify our environment variables.
if [[ -s "$HOME/.rvm/scripts/rvm" ]]
then
    source "$HOME/.rvm/scripts/rvm"
    if type advise &> /dev/null; then
      advise cd with-ls-and-rvm 'super "$@" && ls'
    fi
    if [ ! -z "$rvm_path" ]; then
      mkdir -p "$rvm_path/tmp/$$"
    fi
fi
