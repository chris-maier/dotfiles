# This file configures my bash 
# Author Chris Maier 
# I've never had a bashrc file, so this is considered testing 

# Moving around fast
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# ls aliases for faster typing 
alias ll="ls -l"
alias lo="ls -o"
alias lh="ls -lh"
alias la="ls -la"
alias sl="ls"
alias l="ls"
alias s="ls"

# give grep some color 
export GREP_OPTIONS='--color=auto'

# \u = username
# \h = host
# \w = working directory
# $? = last return code
PS1='[\[\033[0;31m\]$?\[\033[1;37m\]]\[\033[1;32m\][\u@\h]: \[\033[1;34m\]\w\[\033[1;37m\]> '
PROMPT_DIRTRIM=4 

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
