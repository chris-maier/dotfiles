#!/bin/bash

truecrypt_packages=" truecrypt"
truecrypt_result_var=0
truecrypt_verbose=""

virtualbox_packages=" virtualbox virtualbox_qt"
virtualbox_result_var=0
virtualbox_verbose=""

printer_result_var=0
printer_verbose=""

function truecrypt_preinstall (){
	truecrypt_verbose+=$(add-apt-repository -y ppa:stefansundin/truecrypt)
	truecrypt_result_var=$?

	packages+=$truecrypt_packages
}

function virtualbox_preinstall (){

	sudo -u $script_user wget -q -o - http://download.virtualbox.org/virtualbox/debian/oracle_vbox_2016.asc | apt-key add -
	echo "deb http://download.virtualbox.org/virtualbox/debian yakkety non-free contrib" > /etc/apt/sources.list.d/virtualbox.org.list
	packages+=$virtualbox_packages
}

function printer_preinstall (){
	printer_verbose+=$(sh -c '$script_dir/linux-brprinter-installer-2.1.1-1 MFC-L2700DW')
	printer_result_var=$?
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function truecrypt_result (){
	echo "Package: TRUECRYPT"

	if [ ! -z ${1+x} ]; then 
		echo $truecrypt_verbose
	fi

	if [ $truecrypt_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $truecrypt_result_var
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function virtualbox_result (){
	echo "Package: VIRTUALBOX"

	if [ ! -z ${1+x} ]; then 
		echo $virtualbox_verbose
	fi

	if [ $virtualbox_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $virtualbox_result_var
}

# Return the overall result of the package
# Param: $1 - verbose if set
# Result: 0 on success, -1 otherwise
function printer_result (){
	echo "Package: PRINTER"

	if [ ! -z ${1+x} ]; then 
		echo $printer_verbose
	fi

	if [ $printer_result_var -gt 0 ]; then 
		echo "-> FAILED"
	else
		echo "-> OK"
	fi
	return $printer_result_var
}

# decide wether we install or not
if $opt_truecrypt; then
	# add functions to the functions array 
	pre_install+=(truecrypt_preinstall)
	#post_install+=(truecrypt_postinstall)
	result_install+=(truecrypt_result)
fi

# decide wether we install or not
if $opt_virtualbox; then
	# add functions to the functions array 
	pre_install+=(virtualbox_preinstall)
	#post_install+=(virtualbox_postinstall)
	result_install+=(virtualbox_result)
fi


# decide wether we install or not
if $opt_printer; then
	# add functions to the functions array 
	pre_install+=(printer_preinstall)
	#post_install+=(printer_postinstall)
	result_install+=(printer_result)
fi
