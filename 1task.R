#################################################################
#This task is to calculate area, depending on v0 and direction
#Looks like it's gonna be many parabolas
#################################################################

epsilon <- 0.05 #accuracy of calculating
g <- 9.81 #gravity acceleration

v0 <- c(8.8, 8.9, 9.3, 9.7, 10.2, 10.9, 11.1) #speed is in meters per second 

for(i in 1:7)
{
	x <- vector(length=12) #horizontal axes
        y <- vector(length=12) #vertical axes 
	
	plot(x, y, col="blue", ylim=c(0, 12), xlim=c(0, 12))
        currentParameters = paste("v0=", v0[i], "m/s, angle: 15-75")
        title(main=currentParameters, col.main="red", font.main=4)	

	for(alpha in seq(from=0.26, to=1.309, by=(epsilon*1.309))) #change angles from 15grad to 75 grad
	{
		wholeTime <- 2*v0[i]*sin(alpha)/g #whole time of flight
	        step <-  epsilon*wholeTime

        	cnt=1
	        for(t in seq(from=0.01, to=wholeTime, by=step)) {
        	        x[cnt] <- v0[i]*cos(alpha)*t
                	y[cnt] <- (v0[i]*sin(alpha)*t-g*t*t/2)
			cnt <- cnt+1
	        }
        	lines(x, y, type="o",  pch=20, lty=1, col="blue")
	}
}
