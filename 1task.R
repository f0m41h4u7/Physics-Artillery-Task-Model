#################################################################
#This task is to calculate area, depending on v0 and direction
#Looks like it's gonna be many parabolas
#################################################################

epsilon <- 0.05 #accuracy of calculating
g <- 9.81 #gravity acceleration
v0 <- 11.1

x <- vector(length=12) #horizontal axes
y <- vector(length=12) #vertical axes 
	
plot(x, y, col="blue", ylim=c(0, 12), xlim=c(0, 12))
currentParameters = paste("v0=", v0, "m/s, angle: 15-75")
title(main=currentParameters, col.main="red", font.main=4)	
legend(1, 12, c("numeric method","analytic method"), cex=0.8, col=c("blue","red"), lty=2:1)

for(alpha in seq(from=0.26, to=1.309, by=(epsilon*1.309))) #change angles from 15grad to 75 grad
{
	wholeTime <- 2*v0*sin(alpha)/g #whole time of flight
        step <-  epsilon*wholeTime

       	cnt=1
        for(t in seq(from=0.01, to=wholeTime, by=step)) 
	{
       	        x[cnt] <- v0*cos(alpha)*t
               	y[cnt] <- (v0*sin(alpha)*t-g*t*t/2)
		cnt <- cnt+1
        }
       	lines(x, y, type="o",  pch=20, lty=1, col="blue")
	lines(x, (x*tan(alpha)-g*x*x/(2*v0*v0*cos(alpha)*cos(alpha))), col="red")
}
