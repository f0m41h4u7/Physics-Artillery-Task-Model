#################################################################
# This task is to calculate trajectory
# With enviroment resistance
#################################################################

g <- 9.81 #gravity acceleration
v0 <- 11.1
alpha <- pi/4
k <- 0.3  #enviroment resistance coefficient
m <- 2  #mass in kilos

x <- vector(length=50) #horizontal axes
y <- vector(length=50) #vertical axes

plot(x, y, col="blue", ylim=c(0, 13), xlim=c(0, 13))
#currentParameters = paste("v0=", v0, "m/s, angle=", alpha, "radians, m=", m, "kg, k=", k)
#title(main="", col.main="red", font.main=4)
legend(1, 13, c(paste("v0 = ", v0, " m/s"), "angle = pi/4", paste("m = ", m, " kg"), paste("k = ", k)), cex=0.8)

v_x <- vector(length=50) #projections on x
v_y <- vector(length=50) #projections on y
v_x[1] <- v0*cos(alpha)
v_y[1] <- v0*sin(alpha)

wholeTime <- 2*v0*sin(alpha)/g #whole time of flight
dt <- wholeTime/50 #step 
h <- 0.05

x[1] <- 0
y[1] <- 0

for(cnt in seq(2, 51, 1))
{
	#f_v1_x <- (-k * v_x[cnt-1] * v_x[cnt-1] * dt / m) 
	#f_v1_y <- (-(g + k * v_y[cnt-1] * v_y[cnt-1] / m) * dt)

	#v_x_tmp <- (v_x[cnt-1] + h * f_v1_x)
	#v_y_tmp <- (v_y[cnt-1] + h * f_v1_y)

	#f_x_tmp <- (-k * v_x_tmp * v_x_tmp * dt / m)
        #f_y_tmp <- (-(g + k * v_y_tmp * v_y_tmp / m) * dt)

	#v_x[cnt] <- (v_x[cnt-1] + h * (f_v1_x + f_x_tmp) / 2)
	#v_y[cnt] <- (v_y[cnt-1] + h * (f_v1_y + f_y_tmp) / 2)
	
	v_x[cnt] <- (v_x[cnt-1] - k * v_x[cnt-1] * v_x[cnt-1] * dt / m)
	v_y[cnt] <- (v_y[cnt-1] - (g + k * v_y[cnt-1] * v_y[cnt-1] / m) * dt)

	x[cnt] <- (x[cnt-1] + v_x[cnt-1]*dt)
	y[cnt] <- (y[cnt-1] + v_y[cnt-1]*dt)
}

lines(x, y, type="o",  pch=20, lty=1, col="blue")
