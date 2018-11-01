from __future__ import division
import os
import sys
# import pygame (for making a display)
import pygame
from pygame.locals import *

subject = raw_input('Enter subject ID here: ')
print 'Subject ID is: ', subject
filename = subject + '.csv'
filepath = os.path.join('data', filename)
FILE = open(filepath, 'w')
FILE.write('Subject: %s\n' % subject)

# initialize pygame
pygame.init()
# set pygame to catch events to monitor for keystrokes
pygame.event.set_grab(1)
# create a screen object that will display stuff
# this is a 32 bit window, 
# to use fullscreen, replace 32 with pygame.FULLSCREEN
SCREEN = pygame.display.set_mode((800,600), 32) #, pygame.FULLSCREEN
# create a font object to render text
FONT = pygame.font.Font(None, 28)

# fill screen buffer with some weird color in (R, G, B) 
SCREEN.fill((86, 130, 160))
# make an image of text, antialiasing on, in color (R,G,B)
textimg = FONT.render('Hello World', 1, (0,0,0))
# put the text image on the screen buffer 10 pixels over, 20 down from the top left
SCREEN.blit(textimg, (10, 20))
# flip screen buffer to the display.
pygame.display.flip()

# wait for a key
wait = True
while wait:
	# look at all events in the queue
	for event in pygame.event.get():
		# if space is pressed, stop waiting.
		if (event.type == KEYDOWN and event.key == K_SPACE):
			wait = False
			
print 'thank you for your participation!'
FILE.close()
sys.exit(0)
