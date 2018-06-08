# new keyboard mappings
if [ -f "$HOME/.Xmodmap" ]; then
   xmodmap "$HOME/.Xmodmap"
fi

# Geze Proxy settings
export ftp_proxy=""
export https_proxy=""
export http_proxy=""

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="../../Workspace/dotfiles/src/my-theme"

# Disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Display red dots while waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Disable marking untracked files under VCS as dirty. This makes repository
# status check for large repositories much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# HISTORY settings
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000
HIST_STAMPS="yyyy-mm-dd"
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git command-not-found colored-man-pages compleat colorize pip python themes fasd history sudo)

# User configuration
# Configure zsh options #######################################################
setopt AUTO_CD # change directories without cd
setopt AUTO_PUSHD # use directory stack
setopt CHASE_LINKS # resolve symbolic links
setopt COMPLETE_ALIASES
setopt GLOB_COMPLETE

source $ZSH/oh-my-zsh.sh

# load the completion system
autoload -Uz compinit
compinit

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# MISC
alias x="exit"
alias xo="xdg-open"
alias more="less"
alias ag="ag $* --pager 'less'"
alias -g G="| grep -i"
alias -g L="| less"
alias -g TL="| tail -20"
alias -g NUL=">/dev/null 2>&1"
alias -g ERRNUL="2>/dev/null"

# colorful LS
alias l='ls -lFh --color=always'     #size,show type,human readable
alias ls='ls --color=always'
alias la='ls -lAFh --color=always'   #long list,show almost all,show type,human readable
alias lr='ls -tRFh --color=always'   #sorted by date,recursive,show type,human readable
alias lt='ls -ltFh --color=always'   #long list,sorted by date,show type,human readable
alias ll='ls -l --color=always'      #long list

# find fast
alias fd='find . -type d -name'
alias ff='find . -type f -name'

# FASD
eval "$(fasd --init auto)"
alias c='fasd_cd -d'
# f for files
# a for dirs and files
# c for dirs
# open everything
alias e='f -e ec' # quick opening files with emacs
alias o='a -e xdg-open' # quick opening files with xdg-open

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
# alias clPassFile='revelation /home/chris/Documents/Accounts/passwords &'
# alias clPassPrivate='truecrypt /home/chris/Documents/private &'
