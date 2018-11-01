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
FILE.write('trial\tvary\tn\tresponse\tRdot\tRspace\tNrings\tFeedback\n')

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
N_mu = 100
N_min = 10
N_max = 750
ntrials = 1000
ttime = 10 # in minutes
scale = 1.25
disptime = 500
feedbackTrials = range(0,25)
fixedRdot = int(WY/2/math.sin(math.pi/3)/getMinI(N_max*scale)/2)
Rspace = Rdot

pygame.init()
pygame.event.set_grab(1)
SCREEN = pygame.display.set_mode((WX,WY), 32)
FONT = pygame.font.Font(None, 28)
BIGFONT = pygame.font.Font(None, 64)
BIGFONT.set_bold(True)

		
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
	print '\n********************\nThank you for your participation!'
	FILE.close()
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
				elif event.key == K_ESCAPE:
					quit('I pressed ESCAPE!')
	return(response)

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
		pygame.draw.circle(SCREEN, (0,0,0), txy, Rdot)
		
def drawFeedback(dots, Rdot, feedback):
	drawDots(dots,Rdot)
	textimg = BIGFONT.render(str(feedback), 1, (255,0,0))
	sz = BIGFONT.size(str(feedback))
	SCREEN.blit(textimg, (int(WX/2-sz[0]/2), int(WY/2-sz[1]/2)))

def drawInstructions(text):
	textimg = FONT.render(text, 1, (0,0,0))
	sz = FONT.size(str(text))
	SCREEN.blit(textimg, (int(WX/2-sz[0]/2), int(WY/2-sz[1]/2)))

starttime = pygame.time.get_ticks()

for t in range(0,ntrials):
	if t in feedbackTrials:
		feedback = True
	n = min(N_max,int(random.expovariate(1/(N_mu-N_min))+N_min))
	
	vary = random.choice(['area', 'space', 'size'])
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
	pygame.time.wait(disptime)
	response = getTypedNumber()
	if(feedback):
		feedbackn = n
		SCREEN.fill(bgcol)
		drawFeedback(dots, Rdot, feedbackn)
		pygame.display.flip()
		pygame.time.wait(disptime)
	else:
		feedbackn = 0
	
	SCREEN.fill(bgcol)
	drawInstructions('Press space for the next trial.')
	pygame.display.flip()
	key = waitForKey()
	elapsedtime = pygame.time.get_ticks()-starttime
	FILE.write('%d\t%d\t%s\t%d\t%s\t%d\t%0.2f\t%d\t%d\n'%(t+1, elapsedtime, vary, n, response, Rdot, Rspace, I, feedbackn))
	if elapsedtime < ttime*60*1000:
		break
quit()

