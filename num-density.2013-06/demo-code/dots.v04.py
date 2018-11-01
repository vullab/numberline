from __future__ import division
import os
import sys
import pygame
from pygame.locals import *
# import random library to draw random numbers
import random

subject = raw_input('Enter subject ID here: ')
print 'Subject ID is: ', subject
filename = subject + '.csv'
filepath = os.path.join('data', filename)
FILE = open(filepath, 'w')
FILE.write('Subject: %s\n' % subject)

# here we use a variable to set screen size
WX = 800
WY = 600
pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((WX,WY), 32)
FONT = pygame.font.Font(None, 28)

SCREEN.fill((86, 130, 160))
textimg = FONT.render('How many circles are there?', 1, (0,0,0))
SCREEN.blit(textimg, (WX/2-100, 1))
R = 10 		# circle radius
N_mu = 20	# average number of circles
N_min = 5	# minimum number of circles
N_max = 750	# maximum number of circles
# pick a random number of circles from an offset geometric distribution
# since we have an exponential in random package, we can just round to get an integer
# we draw an integer between N_min and N_max from a geometric.
n = min(N_max,int(random.expovariate(1/(N_mu-N_min))+N_min))
for i in range(0,n):
	# pick a random coordinate to draw the circle...
	x = random.randint(R,WX-R)
	y = random.randint(R+20,WY-R-20)
	pygame.draw.circle(SCREEN, (0,0,0), (x,y), R)
pygame.display.flip()

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
