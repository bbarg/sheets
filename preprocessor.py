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
  commented = False
  linejoin  = False
  debug     = True

  for i, line in enumerate(input_file):

    lineout = line.rstrip()

    if lineout:

      if any(x in lineout for x in invalidchar):
        raise SyntaxError("Invalid character found on line {}".format(i))

      # Check if first statement is a comment
      lstripline = lineout.lstrip()
      if ('#' == lstripline[0] and '~' == lstripline[1]) or \
         ('/' == lstripline[0] and '/' == lstripline[1]):
        commented = True

      # TODO: Fix for if comment was a '//'
      if commented:
        if ('~#' in lineout):
          commented = False

      else:

        if not linejoin:

          wcount  = len(lineout) - len(lineout.lstrip(' '))

          # If the previous line began an indentation, add the new
          # indentation level to the block (so long as the new indentation
          # level is greater than the previous one)
          if newindent == True:
            if wcount > stack[-1]:
              stack.append(wcount)
              newindent = False
            else:
              raise IndentationError("Error on line {}".format(i))

          # If the indentation level is greater than expected, throw an error
          if wcount > stack[-1]:

            if debug:
              print "=== ERROR ==="
              print "proc. line: '{}'".format(lineout)
              print "wcount:     {}".format(wcount)
              print "stack[-1]:  {}".format(stack[-1])
              print "newindent:  {}".format(wcount)          

            raise IndentationError("Error on line {}".format(i))

          else:

            # If the indentation level is less than the current level, return
            # to a previous indentation block. Throw an error if you return to
            # an indentation level that doesn't exist
            while(wcount < stack[-1]):
              lineout = "}" + lineout
              stack.pop()

            if wcount != stack[-1]:

              if debug:
                print "=== ERROR ==="
                print "proc. line: '{}'".format(lineout)                
                print "wcount:    {}".format(wcount)
                print "stack[-1]: {}".format(stack[-1])
                print "newindent: {}".format(wcount)  

              raise IndentationError("Error on line {}".format(i))

        # Given that the indentation level is correct, check for the start
        # of a new code block (where a line ends with a ':') and insert a 
        # '{'. At the end of a line, add a semicolon ';' unless if there is
        # a linejoin character '\'.
        if lineout[-1] == ':':
          lineout = lineout + '{\n'
          newindent = True

        elif lineout[-1] == '\\':
          linejoin = True

        else:
          lineout = lineout + ';\n'
          linejoin = False
        
        output.write(lineout)

  output.write("}")

  if debug:
    print output.getvalue()

  return output

def usage():
  print"""
  python preprocessor.py [input.sht]
  """

if __name__ == "__main__":

  if len(sys.argv) != 2:
    usage()
    sys.exit(2)

  try:
    f_in = open(sys.argv[1],"r")
  except IOError:
    sys.stderr.write("ERROR: Cannot read input file %s.\n" % sys.argv[1])
    sys.exit(1)

  name_ext = os.path.basename(f_in.name)

  if name_ext.lower().endswith((".sht",".sheet")):
    fname = os.path.splitext(name_ext)[0]
  else:
    raise NameError('Input must have Sheets file extension')

  out_str = process(f_in)

  desired_path = "./tests/processed_out/"

  f_out = open(desired_path+fname+".proc.sht", 'a')
  f_out.write(out_str.getvalue())