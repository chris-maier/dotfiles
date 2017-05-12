# new keyboard mappings
if [ -f "$HOME/.Xmodmap" ]; then
   xmodmap "$HOME/.Xmodmap"
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="philips"

# Disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Display red dots while waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Disable marking untracked files under VCS as dirty. This makes repository
# status check for large repositories much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HISTFILE=~/.zsh_history
SAVEHIST=100000
HISTORY_IGNORE="(x|j|cd|ls|ll|la|pwd|history)"
HIST_STAMPS="yyyy-mm-dd"
HISTSIZE=100000                   # Lots of history.
HISTFILESIZE=100000               # Lots of history in the file.
HISTCONTROL=ignoreboth            # Ignore entries with leading white space and dupes.
# HISTIGNORE="ls:ll:cd:fg:j:jobs"   # Uninteresting commands to not record in history.

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git command-not-found compleat colorize pip python themes autojump history sudo)

# User configuration
# Configure zsh options #######################################################
setopt AUTO_CD # change directories without cd
setopt AUTO_PUSHD # use directory stack
setopt CHASE_LINKS # resolve symbolic links 
setopt PUSHD_IGNORE_DUPS
setopt COMPLETE_ALIASES
setopt GLOB_COMPLETE
setopt HIST_IGNORE_ALL_DUPS
setopt APPEND_HISTORY
setopt HIST_APPEND

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$PATH:~/bin
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# setopt autolist
# setopt autonamedirs
# setopt histignoredups
# setopt listtypes
# setopt nolistbeep
# setopt setopt
# histappend histverify

# load the completion system 
autoload -Uz compinit
compinit

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

#
# MISC
alias x="exit"
alias more="less"
alias -g G="| grep"
alias -g L="| less"
alias -g TL="| tail -20"
alias -g NUL="> /dev/null 2>&1"

# NVIM shortcuts
alias n="nvim"
alias v="nvim"
export EDITOR="ec"
export ALTERNATE_EDITOR=""
export VISUAL="ec"
export GIT_EDITOR="ec"

# Autojump
alias js="j -s"

# GIT shortcuts
#g	git
#ga	git add
#gaa	git add --all
#gapa	git add --patch
#gb	git branch
#gba	git branch -a
#gbda	git branch --merged | command grep -vE "^(*|\smaster\s$)" | command xargs -n 1 git branch -d
#gbl	git blame -b -w
#gbnm	git branch --no-merged
#gbr	git branch --remote
#gbs	git bisect
#gbsb	git bisect bad
#gbsg	git bisect good
#gbsr	git bisect reset
#gbss	git bisect start
#gc	git commit -v
#gc!	git commit -v --amend
#gca	git commit -v -a
#gcam	git commit -a -m
#gca!	git commit -v -a --amend
#gcan!	git commit -v -a -s --no-edit --amend
#gcb	git checkout -b
#gcf	git config --list
#gcl	git clone --recursive
#gclean	git clean -df
#gcm	git checkout master
#gcd	git checkout develop
#gcmsg	git commit -m
#gco	git checkout
#gcount	git shortlog -sn
#gcp	git cherry-pick
#gcpa	git cherry-pick --abort
#gcpc	git cherry-pick --continue
#gcs	git commit -S
#gd	git diff
#gdca	git diff --cached
#gdt	git diff-tree --no-commit-id --name-only -r
#gdw	git diff --word-diff
#gf	git fetch
#gfa	git fetch --all --prune
#gfo	git fetch origin
#gg	git gui citool
#gga	git gui citool --amend
#ghh	git help
#ggpull	ggl
#ggpur	ggu
#ggpush	ggp
#ggsup	git branch --set-upstream-to = origin/$(current_branch)
#gpsup	git push --set-upstream origin $(current_branch)
#gignore	git update-index --assume-unchanged
#gignored	git ls-files -v | grep "^:lower:"
#git-svn-dcommit-push	git svn dcommit && git push github master:svntrunk
#gk	\gitk --all --branches
#gke	\gitk --all $(git log -g --pretty = format:%h)
#gl	git pull
#glg	git log --stat --color
#glgg	git log --graph --color
#glgga	git log --graph --decorate --all
#glgm	git log --graph --max-count = 10
#glgp	git log --stat --color -p
#glo	git log --oneline --decorate --color
#glog	git log --oneline --decorate --color --graph
#glol	git log --graph --pretty = format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
#glola	git log --graph --pretty = format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --all
#glp	_git_log_prettily
#gm	git merge
#gmom	git merge origin/master
#gmt	git mergetool --no-prompt
#gmtvim	git mergetool --no-prompt --tool = vimdiff
#gmum	git merge upstream/master
#gp	git push
#gpd	git push --dry-run
#gpoat	git push origin --all && git push origin --tags
#gpristine	git reset --hard && git clean -dfx
#gpu	git push upstream
#gpv	git push -v
#gr	git remote
#gra	git remote add
#grb	git rebase
#grba	git rebase --abort
#grbc	git rebase --continue
#grbi	git rebase -i
#grbm	git rebase master
#grbs	git rebase --skip
#grh	git reset HEAD
#grhh	git reset HEAD --hard
#grmv	git remote rename
#grrm	git remote remove
#grset	git remote set-url
#grt	cd $(git rev-parse --show-toplevel || echo ".")
#gru	git reset --
#grup	git remote update
#grv	git remote -v
#gsb	git status -sb
#gsd	git svn dcommit
#gsi	git submodule init
#gsps	git show --pretty = short --show-signature
#gsr	git svn rebase
#gss	git status -s
#gst	git status
#gsta	git stash save
#gstaa	git stash apply
#gstd	git stash drop
#gstl	git stash list
#gstp	git stash pop
#gstc	git stash clear
#gsts	git stash show --text
#gsu	git submodule update
#gts	git tag -s
#gunignore	git update-index --no-assume-unchanged
#gunwip	git log -n 1 | grep -q -c "--wip--" && git reset HEAD~1
#gup	git pull --rebase
#gupv	git pull --rebase -v
#glum	git pull upstream master
#gvt	git verify-tag
#gwch	git whatchanged -p --abbrev-commit --pretty = medium
#gwip	git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit -m "--wip--"

# WEB DEV
alias serve='python -m SimpleHTTPServer 8000'

alias cd..="cd .." # I often make this mistake

# personal shortcuts 
alias cmaPassFile='revelation /home/chris/Documents/Accounts/passwords &'
alias cmaPassPrivate='truecrypt /home/chris/Documents/private &'

