from __future__ import division
import os
import sys
import pygame
from pygame.locals import *
import random
import math

subject = raw_input('Enter subject ID here: ')
print 'Subject ID is: ', subject
filename = subject + '.csv'
filepath = os.path.join('data', filename)
FILE = open(filepath, 'w')
FILE.write('Subject: %s\n' % subject)
FILE.write('trial\tvary\tn\tresponse\n')

WX = 800
WY = 600
bgcol = (255,255,255)
N_mu = 749
N_min = 10
N_max = 750
fixedRdot = 9
fixedRspace = 9
ntrials = 50
scale = 1.25

pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((WX,WY), 32)
FONT = pygame.font.Font(None, 28)

def getMaxN(I):
	return(sum(range(0,I)*6)+1)

def getMinI(n,I=1):
	if(n <= (I-1)*6):
		return(I)
	else:
		return(getMinI(n-(I-1)*6, I+1))
		
def getCoord(i):
	ring = getMinI(i-1)
	nring = max(1, (ring-1)*6)
	j = (i-1)/nring
	th1 = math.pi/3*math.floor(j*6)
	th2 = math.pi/3*math.ceil(j*6)
	q = (j*6)%1
	return(((ring-1)*(math.cos(th1)*q + (1-q)*math.cos(th2)), (ring-1)*(math.sin(th1)*q + (1-q)*math.sin(th2))))

def waitForKey():
	pressed = False
	while(not pressed):
		for event in pygame.event.get():
			if event.type == KEYDOWN:
				if event.key == K_SPACE:
					pressed = 1
				elif event.key == K_ESCAPE:
					pressed = -1
	return(pressed)

def getTypedNumber():
	SCREEN.fill(bgcol)
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
	return(response)

def drawDots(n, Rdot, Rspace):
	center = (WX/2,WY/2)
	I = getMinI(n*scale)
	maxn = getMaxN(I)
	usei = range(1,maxn+1)
	random.shuffle(usei)
	usei = usei[0:n]
	for i in usei:
		xy = getCoord(i)
		txy = (int(xy[0]*Rspace*2+center[0]), int(xy[1]*Rspace*2+center[1]))
		pygame.draw.circle(SCREEN, (0,0,0), txy, Rdot)
# 
# SCREEN.fill(bgcol)
# textimg = FONT.render('How many circles are there?', 1, (0,0,0))
# SCREEN.blit(textimg, (WX/2-100, 1))
# 
# n = min(N_max,int(random.expovariate(1/(N_mu-N_min))+N_min))

for t in range(0,ntrials):
	n = min(N_max,int(random.expovariate(1/(N_mu-N_min))+N_min))
	
	vary = random.choice(['area', 'space', 'size'])
	if vary=='area':
		# show dots while varying *area* at fixed spacing, size
		Rdot = fixedRdot
		Rspace = fixedRspace
	elif vary=='space':
		# show dots while varying *spacing* at a fixed area, size
		I = getMinI(n*scale)
		Rspace = WY/2/math.sin(math.pi/3)/I/2
		Rdot = fixedRdot
	elif vary=='size':
		# show dots while varying size, fixed, area, spacing
		I = getMinI(n*scale)
		Rdot = int(WY/2/math.sin(math.pi/3)/I/2)
		Rspace = Rdot
	else:
		print('UNRECOGNIZED OPTION %s'%vary)
	SCREEN.fill(bgcol)
	drawDots(n,Rdot,Rspace)
	pygame.display.flip()
	pygame.time.wait(disptime)
	response = getTypedNumber()
	FILE.write('%d\t%s\t%d\t%s\n'%(trial, vary, n, response))


# write response to a file.
FILE.write(str(response)+'\n')
print 'thank you for your participation!'
FILE.close()
sys.exit(0)
