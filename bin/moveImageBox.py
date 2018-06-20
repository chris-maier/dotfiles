#!/usr/bin/env python

import sys
import os
from PIL import Image

def usage():
    print "\nOpen first argument as image and cut and paste box to all other arguments\n"
    print 'Usage: '+sys.argv[0]+' <file1> <file2> <...>'

def main(argv):
    if len(argv) < 2:
        usage()
        exit(1)

    inFrame = Image.open(argv[0])
    box = (192,92,351,272)

    regionToMove = inFrame.crop(box)
    # debug
    # regionToMove = regionToMove.transpose(Image.ROTATE_180)

    # inFrame.paste(regionToMove,box)
    # inFrame.show()

    for arg in argv[1:]:
        outFrame = Image.open(arg)
        outFrame = outFrame.copy()
        outFrame.paste(regionToMove,box)
        outFrame.show()
        raw_input("Press Enter to continue...")
        newName, ext = os.path.splitext(arg)
        outFrame.save(newName + "_new" + ext)

if __name__ == "__main__":
   main(sys.argv[1:])
