from __future__ import division
import os
import sys
import pygame
from pygame.locals import *
import random
import math

vary = "size"
n = 50

def getMaxN(I):
	return(sum(range(0,I)*6)+1)

def getMinI(n,I=1):
	if(n <= (I-1)*6):
		return(I)
	else:
		return(getMinI(n-(I-1)*6, I+1))

WX = 1024
WY = 768
bgcol = (255,255,255)
dotCol = (0, 0, 255)
N_mu = 100
N_min = 10
N_max = 750
ntrials = 1000
ttime = 50 # in minutes
scale = 1.25
disptime = 500
feedbackTrials = range(0,25)
fixedRdot = int(WY/2/math.sin(math.pi/3)/getMinI(N_max*scale)/2)
fixedRspace = fixedRdot
score = 0

pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((WX,WY))
FONT = pygame.font.Font(None, 28)
BIGFONT = pygame.font.Font(None, 64)
BIGFONT.set_bold(True)

def scoreFunction(x,n):
	x = x+0.1
	return int(max(0,10-20*(math.log(x)-math.log(n))**2))

def getCoord(i):
	ring = getMinI(i-1)
	nring = max(1, (ring-1)*6)
	j = (i-1)/nring
	th1 = math.pi/3*math.floor(j*6)
	th2 = math.pi/3*math.ceil(j*6)
	q = (j*6)%1
	return(((ring-1)*(math.cos(th1)*q + (1-q)*math.cos(th2)), (ring-1)*(math.sin(th1)*q + (1-q)*math.sin(th2))))

def quit(signal=False):
	if(signal):
		print('\nUser quit with message:\n"'+str(signal)+'"')
	print('\n********************\nThank you for your participation!') # erikb added
	sys.exit(0)

def waitForKey():
	pressed = False
	while(not pressed):
		for event in pygame.event.get():
			if event.type == KEYDOWN:
				if event.key == K_SPACE:
					pressed = 1
				elif event.key == K_ESCAPE:
					quit('I pressed ESCAPE!')
	return(pressed)

def dotCenters(n, Rdot, Rspace):
	center = (WX/2,WY/2)
	I = getMinI(n*scale)
	maxn = getMaxN(I)
	usei = range(1,maxn+1)
	random.shuffle(usei)
	usei = usei[0:n]
	dots = []
	for i in usei:
		xy = getCoord(i)
		txy = (int(xy[0]*Rspace*2+center[0]), int(xy[1]*Rspace*2+center[1]))
		dots.append(txy)
	return(dots)

def drawDots(dots, Rdot):
	for txy in dots:
		pygame.draw.circle(SCREEN, dotCol, txy, Rdot)

def drawFeedback(dots, Rdot, feedback):
	drawDots(dots,Rdot)
	textimg = BIGFONT.render(str(feedback), 1, (255,0,0))
	sz = BIGFONT.size(str(feedback))
	SCREEN.blit(textimg, (int(WX/2-sz[0]/2), int(WY/2-sz[1]/2)))

def drawInstructions(text):
	textimg = FONT.render(text, 1, (0,0,0))
	sz = FONT.size(str(text))
	SCREEN.blit(textimg, (int(WX/2-sz[0]/2), int(WY/2-sz[1]/2)))
	textimg = FONT.render('(press SPACE to continue)', 1, (0,0,0))
	sz = FONT.size(str('(press SPACE to continue)'))
	SCREEN.blit(textimg, (int(WX/2-sz[0]/2), int(WY-sz[1])))

def instructionScreen(text):
	SCREEN.fill(bgcol)
	drawInstructions(text)
	pygame.display.flip()
	key = waitForKey()

starttime = pygame.time.get_ticks()

if vary=='area':
	# show dots while varying *area* at fixed spacing, size
	I = getMinI(n*scale)
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
dots = dotCenters(n, Rdot, Rspace)
drawDots(dots,Rdot)
pygame.display.flip()
waitForKey()

quit()
