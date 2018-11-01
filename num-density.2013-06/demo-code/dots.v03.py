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

pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((800,600), 32)
FONT = pygame.font.Font(None, 28)

SCREEN.fill((86, 130, 160))
textimg = FONT.render('How many circles are there?', 1, (0,0,0))
SCREEN.blit(textimg, (10, 10))
# draw a few circles to the screen buffer
pygame.draw.circle(SCREEN, (0,0,0), (40,40), 10)
pygame.draw.circle(SCREEN, (0,0,0), (80,40), 10)
pygame.draw.circle(SCREEN, (0,0,0), (100,40), 10)
pygame.draw.circle(SCREEN, (0,0,0), (120,40), 10)
pygame.draw.circle(SCREEN, (0,0,0), (140,40), 10)
pygame.draw.circle(SCREEN, (0,0,0), (160,40), 10)
pygame.display.flip()

# get a typed response by monitoring for key strokes.
response = ''
responded = False
while not responded:
	for event in pygame.event.get():
		if event.type == KEYDOWN:
			# if the keystroke is in range 0 - 9, append to response
			if event.unicode in map(str, range(0,10)):
				response += event.unicode
			# if key press is return, end loop
			elif event.key == K_RETURN:
				responded = True
# write response to a file.
FILE.write(str(response)+'\n')
print 'thank you for your participation!'
FILE.close()
sys.exit(0)
