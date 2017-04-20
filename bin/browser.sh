#!/bin/bash

browser_packages=" google-chrome-stable"
browser_result_var=0
browser_verbose=""

function browser_preinstall ()
{
	browser_verbose=$(sudo -u $script_user wget -q -o - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -)
	browser_result_var=$?

	echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google.list
	packages+=$browser_packages
}

function browser_postinstall ()
{
	# preinstall was not successful
	if [ $browser_result_var -gt 0 ]; then 
		browser_verbose+="\n -> preinstall was not successful. Aborting..."
		return $browser_result_var
	fi

	local browser_bin=$(which $browser)
	browser_result_var=$?

	update-alternatives --install /usr/bin/x-www-browser x-www-browser $browser_bin 60
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function browser_result (){
	echo "Package: BROWSER"

	if [ ! -z ${1+x} ]; then 
		echo $browser__verbose
	fi

	if [ $browser_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $browser_result_var
}

# decide wether we install or not
if $opt_browser; then
	# add functions to the functions array 
	pre_install+=(browser_preinstall)
	post_install+=(browser_postinstall)
	result_install+=(browser_result)
fi
