#! /usr/bin/python

import sys
import os

# Find the best implementation available on this platform
try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

def process(input_file):

    invalidchar = ('{','}','\t')
    stack = [0]
    output = StringIO()
    newindent = False

    for i, line in enumerate(input_file):

        if any(x in line for x in invalidchar):
            raise SyntaxError("Invalid character found on line {}".format(i))
    
        lineout = line.rstrip()
        wcount = len(line) - len(line.lstrip(' '))

        while(wcount < stack[-1]):
            lineout = "}" + lineout
            stack.pop()

        if newindent == True:
            stack.append(wcount)
            newindent = False

        if lineout: 

            if lineout[-1] == ':':
                lineout = lineout + '{\n'
                newindent = True

            elif lineout[-1] != '\\':
                lineout = lineout + ';\n'

        output.write(lineout)

    print(output.getvalue())

def usage():
    print"""
    python preprocessor.py [input.sht]
    """

if __name__ == "__main__":

    if len(sys.argv) != 2:
        usage()
        sys.exit(2)

    try:
        f = open(sys.argv[1],"r")
    except IOError:
        sys.stderr.write("ERROR: Cannot read input file %s.\n" % sys.argv[1])
        sys.exit(1)

    name_ext = os.path.basename(f.name)

    if name_ext.lower().endswith((".sht",".sheet")):
        fname = os.path.splitext(name_ext)[0]
    else:
        raise NameError('Input must have Sheets file extension')

    process(f)