#################################################################
# This task is to calculate trajectory
# Same as 1st, but with enviroment resistance
#################################################################

g <- 9.81 #gravity acceleration
v0 <- 11.1
alpha <- pi/4
k <- 0.8 #enviroment resistance coefficient
m <- 0.1 #mass in kilos

x <- vector(length=100) #horizontal axes
y <- vector(length=100) #vertical axes

plot(x, y, col="blue", ylim=c(0, 12), xlim=c(0, 12))
currentParameters = paste("v0=", v0, "m/s, angle: 15-75")
title(main=currentParameters, col.main="red", font.main=4)
#legend(1, 12, c("numeric method","analytic method"), cex=0.8, col=c("blue","red"), lty=2:1)

v_x <- vector(length=100) #projections on x
v_y <- vector(length=100) #projections on y
v_x[1] <- v0*cos(alpha)
v_y[1] <- v0*sin(alpha)

wholeTime <- 2*v0*sin(alpha)/g #whole time of flight
dt <- wholeTime/50 #step 
h <- 0.05

x[1] <- 0
y[1] <- 0

for(cnt in 2:50)
{
	f_v1_x <- (-k * v_x[cnt-1] * v_x[cnt-1] * dt / m) 
	f_v1_y <- ((-k * v_y[cnt-1] * v_y[cnt-1] - m * g) * dt / m)

	v_x_tmp <- v_x[cnt-1] + h * f_v1_x
	v_y_tmp <- v_y[cnt-1] + h * f_v1_y

	f_x_tmp <- (-k * v_x_tmp * v_x_tmp * dt / m)
        f_y_tmp <- ((-k * v_y_tmp * v_y_tmp - m * g) * dt / m)

	v_x[cnt] <- v_x[cnt-1] + h * (f_v1_x + f_x_tmp) / 2
	v_y[cnt] <- v_y[cnt-1] + h * (f_v1_y + f_y_tmp) / 2	

	x[cnt] <- x[cnt-1] + v_x[cnt-1]*dt
	y[cnt] <- y[cnt-1] + v_y[cnt-1]*dt
}

lines(x, y, type="o",  pch=20, lty=1, col="blue")
