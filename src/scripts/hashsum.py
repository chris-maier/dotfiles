#!/usr/bin/python
# @author C. Maier
# @brief Iterate over arguments and print Hashsums of each file in the arg list

import sys
import hashlib
import os.path

def md5(fname):
    hash = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash.update(chunk)
    return hash.hexdigest()

def sha256(fname):
    hash = hashlib.sha256()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash.update(chunk)
    return hash.hexdigest()

def main(args):
    # iterate over argument
    for arg in args:
        if os.path.isfile (arg):
            print arg
            print "MD5: " + md5(arg)
            print "SHA256: " + sha256(arg)
            print "-----------------------"

if __name__ == "__main__":
    main(sys.argv[1:])
