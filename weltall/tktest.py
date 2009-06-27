import Tkinter
import Canvas
import random
from Tkconstants import *
from math import *

class Weltall:

	canvasSize=800
	canvasMid=canvasSize/2
	dot=100.0*1000
	canvas=None

	def __init__(self,myRoot=None):
		if self.canvas==None:	
			self.canvas=Tkinter.Canvas(myRoot,width=self.canvasSize, height=self.canvasSize, bg='white') 
			self.canvas.pack(expand=YES,fill=BOTH)

	def drawObject (self,x,y,radius):
		myX=x/self.dot+self.canvasMid
		myY=y/self.dot+self.canvasMid
		print myX,myY
		self.canvas.create_oval(myX-radius,myY-radius,myX+radius,myY+radius,width=2,fill='green')

	def update (self):
		self.canvas.update_idletasks()
		

class Sputnik:
	sX=0
	sY=0
	vX=0
	vY=0
	a=0
	me=6*10**24
	G=6.67428*10**-11

	def __init__(self,x=10000*1000,y=0*1000,vx=0.0,vy=0.0):
		self.sX=x
		self.sY=y
		self.vX=vx
		self.vY=vy

	def move (self,deltaVx=0.0,deltaVy=0.0):
		r=sqrt(self.sX**2+self.sY**2)
		gt=self.G*self.me/r**2

		if self.sX>0.0: signx=-1
		if self.sX<0.0: signx=1
		if self.sX==0: sign=0

		if self.sY<0: signy=1
		if self.sY==0: signy=0
		if self.sY>0: signy=-1

		sXnew=self.sX+(self.vX*1)+signx*0.5*gt+0.5*deltaVx
		sYnew=self.sY+(self.vY*1)+signy*0.5*gt+0.5*deltaVy
	
#		print self.sX,sXnew,self.sY,sYnew, r, signx, signy, self.vX, self.vY
	
		rNew=sqrt(sXnew**2+sYnew**2)
		gtNew=self.G*self.me/rNew**2

		vXnew=self.vX+(deltaVx+signx*(gt+gtNew)/2)*1
		vYnew=self.vY+(deltaVy+signy*(gt+gtNew)/2)*1

		self.sX=sXnew
		self.sY=sYnew
		self.vX=vXnew
		self.vY=vYnew

	def draw (self, universe):
		universe.drawObject(self.sX,self.sY,1)	
	

if __name__ == "__main__":
	root = Tkinter.Tk()
	root.title("Funktion im Kopf das Weltall")
	
	space = Weltall(root)

	astra = Sputnik(10000*1000,0,0,10000.0)

	i=0
	while True:
		astra.move()
		astra.draw(space)
		i=i+1

		if (i % 100) == 0:
			space.update()


#		posX=random.random()*x
#		posY=random.random()*y
#		space.drawObject (posX,posY,1)	



