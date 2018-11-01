# these lines import a number of packages

# this makes division of integers return a decimal number 
from __future__ import division
# this includes functions for interface with the (operating) system
import os
import sys

# Get subject ID (name?) from command line
subject = raw_input('Enter subject ID here: ')
# print the response to the command line
print 'Subject ID is: ', subject

# Set up data file output.  
filename = subject + '.txt'
filepath = os.path.join('data', filename)
FILE = open(filepath, 'w')

FILE.write('Subject: %s\n' % subject)

print 'thank you for your participation!'

# close the file and exit.
FILE.close()
sys.exit(0)
