#################################################################
# This task is to calculate trajectory
# Same as 4st, but without enviroment resistance
#################################################################

g <- 9.81 #gravity acceleration
epsilon <- 0.05 #accuracy of calculating
v0 <- 11.1
alpha <- pi/4

x <- vector(length=2) #horizontal axes
y <- vector(length=2) #vertical axes

plot(x, y, col="blue", ylim=c(0, 13), xlim=c(0, 13))
legend(1, 13, c(paste("v0 = ", v0, " m/s"), "angle = pi/4"))

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
