#!/bin/bash

neovim_packages=" neovim python-dev python-pip python3-dev python3-pip python-setuptools python3-setuptools"
neovim_result_var=0
neovim_verbose=""

function neovim_preinstall (){
	# append necessary packages to the general packages string
	packages+=${neovim_packages}
	# add ppa
	neovim_verbose=$(add-apt-repository -y ppa:neovim-ppa/stable)
	neovim_result_var=$?
}

function neovim_postinstall (){

	# preinstall was not successful
	if [ $neovim_result_var -gt 0 ]; then 
		neovim_verbose+="\n -> preinstall was not successful. Aborting..."
		return $neovim_result_var
	fi

	local neovim_bin=$(which nvim)
	local editor_bin=$(which editor)

	echodebug "neovim bin: $neovim_bin"
	echodebug "editor bin: $editor_bin"

	neovim_verbose+=$(sudo -u $script_user pip install --upgrade pip neovim)
	neovim_result_var=$?
	if [ $neovim_result_var -gt 0 ]; then 
		neovim_verbose+="\n -> pip was not successful. Aborting..."
		return $neovim_result_var
	fi

	neovim_verbose+=$(sudo -u $script_user pip3 install --upgrade pip neovim)
	neovim_result_var=$?
	if [ $neovim_result_var -gt 0 ]; then 
		neovim_verbose+="\n -> pip3 was not successful. Aborting..."
		return $neovim_result_var
	fi

	# link the config files to ~/.config/neovim/
	sudo -u $script_user ln -fs $script_dir/../src/.config/nvim ~/.config/

	# install as default editor
	update-alternatives --install $editor_bin editor $neovim_bin 60
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function neovim_result (){
	echo "Package: NEOVIM"

	if [ ! -z ${1+x} ]; then 
		echo $neovim_verbose
	fi

	if [ $neovim_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $neovim_result_var
}

# decide wether we install or not
if $opt_neovim; then
	# add functions to the functions array 
	pre_install+=(neovim_preinstall)
	post_install+=(neovim_postinstall)
	result_install+=(neovim_result)
fi
