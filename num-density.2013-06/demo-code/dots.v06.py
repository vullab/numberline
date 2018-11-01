from __future__ import division
import os
import sys
import pygame
from pygame.locals import *
# import random library to draw random numbers
import random
import math

subject = raw_input('Enter subject ID here: ')
print 'Subject ID is: ', subject
filename = subject + '.csv'
filepath = os.path.join('data', filename)
FILE = open(filepath, 'w')
FILE.write('Subject: %s\n' % subject)

WX = 800
WY = 600
bgcol = (255,255,255)
R = 10 	
N_mu = 749
N_min = 748
N_max = 750

pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((WX,WY), 32)
FONT = pygame.font.Font(None, 28)

SCREEN.fill(bgcol)
textimg = FONT.render('How many circles are there?', 1, (0,0,0))
SCREEN.blit(textimg, (WX/2-100, 1))

# n = min(N_max,int(random.expovariate(1/(N_mu-N_min))+N_min))
n = 750
a = (WY-20-20)/WX
nx = (n/a)**(0.5)
spacex = WX/nx
ny = math.ceil(nx*a)
nx = math.ceil(nx)
spacey = (WY-20-20)/ny
print((nx,ny,spacex,spacey))
locs = range(0,int(nx*ny))
random.shuffle(locs)
dot_centers = []
for i in range(0,n):
	yi = math.floor(locs[i]/nx)
	xi = locs[i]%nx
	xoff = random.randint(0,int(spacex-R-R))-math.ceil(spacex-R-R)/2
	yoff = random.randint(0,int(spacey-R-R))-math.ceil(spacey-R-R)/2
	dot_centers.append((int(xoff+spacex/2+xi*spacex), int(yoff+spacey/2+yi*spacey+20)))
	pygame.draw.circle(SCREEN, (0,0,0), dot_centers[-1], R)
pygame.display.flip()

response = ''
responded = False
updated = False
while not responded:
	if not updated:
		textimg = FONT.render('How many circles? : '+str(response), 1, (0,0,0))
		pygame.draw.rect(SCREEN, bgcol, pygame.Rect(0, WY-20, WX, 20), 0)
		SCREEN.blit(textimg, (WX/2-100, WY-20))
		pygame.display.update()
		updated = True
	for event in pygame.event.get():
		if event.type == KEYDOWN:
			if event.unicode in map(str, range(0,10)):
				response += event.unicode
				updated = False
			elif event.key == K_BACKSPACE:
				response = response[:-1]
				updated = False
			elif event.key == K_RETURN:
				responded = True
# write response to a file.
FILE.write(str(response)+'\n')
print 'thank you for your participation!'
FILE.close()
sys.exit(0)
