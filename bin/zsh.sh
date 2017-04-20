#!/bin/bash

zsh_packages="zsh zsh-common zsh-doc"
zsh_result_var=0
zsh_verbose=""

function zsh_preinstall (){
	# append necessary packages to the general packages string
	packages+=" ${zsh_packages}"
}

function zsh_postinstall (){
	local zsh_bin=$(which zsh)

	# post install zsh
	if [ -n $zsh_bin ]; then
		# change login shell of current user, not root
		zsh_verbose+=$(chsh -s $zsh_bin $SUDO_USER)
		# download and install oh-my-zsh
		zsh_verbose+=$(sudo -u $SUDO_USER sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)")
		zsh_verbose+=$(sudo -u $SUDO_USER ln -fs $script_dir/../src/.zshrc ~/.zshrc)
	fi
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function zsh_result (){
	echo "Package: ZSH"

	if [ ! -z ${1+x} ]; then 
		echo $zsh_verbose
	fi

	if [ $zsh_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $zsh_result_var
}

# decide wether we install or not
if $opt_zsh; then
	# add functions to the functions array 
	pre_install+=(zsh_preinstall)
	post_install+=(zsh_postinstall)
	result_install+=(zsh_result)
fi
