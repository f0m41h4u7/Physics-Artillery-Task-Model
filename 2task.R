#################################################################
#This task is to calculate v0 and angle, depending on x and y
#################################################################

epsilon <- 0.05 #accuracy of calculating
g <- 9.81 #gravity acceleration

x <- 2.2135
y <- 2.1217

x_axes <- vector(length=4) #horizontal axes
y_axes <- vector(length=4) #vertical axes 

plot(x_axes, y_axes, col="blue", ylim=c(0, 13), xlim=c(0, 13))
currentParameters = paste("x = ", x, "m, y = ", y, "m")
title(main=currentParameters, col.main="red", font.main=4)
points(x, y, type="o",  pch=20, lty=2, col="red")

for(alpha in seq(from=0.02, to=1.2, by=(epsilon*1.2))) #change angles from 15 grad to 75 grad
{
	for(v0 in seq(from=0.1, to=20, by=(epsilon*20)))
	{
		tmp <- (x*tan(alpha) - x*x*g/(2*v0*v0*cos(alpha)*cos(alpha)))
		if(isTRUE(all.equal(tmp, y, tolerance=0.05))) 
		{
			wholeTime <- 2*v0*sin(alpha)/g #whole time of flight
			step <- epsilon*wholeTime

	               	print(v0)
        	       	cnt=1
              		for(t in seq(from=0.01, to=(wholeTime+step), by=step)) #calculating a parabola with this angle and speed
	               	{
        	               	x_axes[cnt] <- v0*cos(alpha)*t
                	       	y_axes[cnt] <- (v0*sin(alpha)*t-g*t*t/2)
                       		cnt <- cnt+1
	               	}
        	       	lines(x_axes, y_axes, type="o",  pch=20, lty=1, col="blue")
       		}
	}
}
points(x, y, type="o",  pch=20, lty=2, col="red")
