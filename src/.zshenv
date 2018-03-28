# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
# export MANPATH="/usr/local/man:$MANPATH"

export PAGER="less"
export LESS="-isMFR"
export EDITOR="ec"
export ALTERNATE_EDITOR="ec"
export VISUAL="ec"
export GIT_EDITOR="ec"

function ec() {
	emacsclient -c -a "" "$@" & > /dev/null
}
