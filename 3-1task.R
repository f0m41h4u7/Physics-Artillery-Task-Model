#################################################################
#This task is to calculate area and trajectory
#Input parameters are v0 and angle
#Returns a 3d plot with parabolas at all directions
#Output is the whole distance flewn by the bullet
#################################################################
library(rgl)

epsilon <- 0.05 #accuracy of calculating
g <- 9.81 #gravity acceleration

######## input values ########
v0=11.1
alpha=pi/4
print(paste("v0=11.1 m/s, angle=pi/4, accuracy=0.05"))

######## setting x, y, z axes ########
x <- vector(length=12) 
y <- vector(length=12) 
z <- vector(length=12) 
	
wholeTime <- 2*v0*sin(alpha)/g #whole time of flight
distance <- wholeTime*cos(alpha) #whole distance
	
plot3d(x, y, z, xlim=c(-10, 10), ylim=c(-10, 10), zlim=c(-10, 10),  add = FALSE)

step <- epsilon*wholeTime
cnt=1

######## calculating parabolas on all sirections from 0 radians to 2pi ########
for(beta in seq(from=0, to=2*pi, by=pi/12))
{
	######## calculating one parabola ######## 
	for(t in seq(from=0.01, to=wholeTime, by=step)) 
        {
		x[cnt] <- (v0*cos(alpha)*t)*cos(beta)
               	z[cnt] <- (v0*sin(alpha)*t-g*t*t/2)
		y[cnt] <- (v0*cos(alpha)*t)*sin(beta)
		cnt <- cnt+1
	}
	
}
rgl.lines(x, y, z, xlim=c(-10, 10), ylim=c(-10, 10), zlim=c(-10, 10),  col="blue", add = TRUE)
	
print(paste("affeceted area: ", distance, "m"))
rgl.postscript("plot.pdf",fmt="pdf")
