#!/bin/bash

git_packages="git git-core libgnome-keyring-dev"
git_result_var=0
git_verbose=""

function git_preinstall (){
	# append necessary git packages to the general packages string
	packages+=" ${git_packages}" 
}

function git_postinstall (){
	# have to compile the libgnome-keyring
	local make_path="/usr/share/doc/git/contrib/credential/gnome-keyring/"
	git_verbose=$(make -C $make_path)
	git_result_var=$?

	# link git config
	sudo -u $SCRIPT_USER ln -fs $SCRIPT_DIR/../src/.config/git ~/.config/
}

# Return the overall result of the git package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function git_result (){

	echo "Package: GIT"

	if [ ! -z ${1+x} ]; then 
		echo $git_verbose
	fi

	if [ $git_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $git_result_var
}

# decide wether we install or not 
if $opt_git; then
	# add functions to the functions array 
	pre_install+=(git_preinstall)
	post_install+=(git_postinstall)
	result_install+=(git_result)
fi
