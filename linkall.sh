# Linkall dotfiles to the place where they can do their work 
# inspired by Aaron Bieber (https://github.com/aaronbieber/dotfiles)

# first link dotfiles in the 'src' directory to the home directory (~) 

# ignore '.' and '..'
SRC_FILES=`ls -A src`

for dotfile in $SRC_FILES 
do 
    ln -fs `pwd`/src/$dotfile ~/$dotfile
done
