


# Let's create a joint probability table matrix

# This is the example from slides.

x <- c(-2, 0, 2, 3)
y <- c( 3,  6)

joint_prob <- c(0.27, 0, 0.08, 0.04, 0.16, 0.10, 0, 0.35)

joint_table <- matrix(joint_prob, ncol = 4) #it fills rows first
joint_table

sum(joint_table)


# plot it

x_data <- sort(rep(x, times = 2))
y_data <- rep(y, times = 4)



#install.packages("plot3D")
library("plot3D")

scatter3D(x_data, y_data, joint_prob, phi = 7, bty = "g",  type = "h", col = "#0072B2",
          ticktype = "detailed", pch = 20, cex = 1.5, xlim = c(-3, 4), ylim = c(2, 7))

#--------------------------------------------


# marginal - x
f_x <- apply(joint_table, 2, sum)
f_x

# marginal expectation  of x
E_x <- sum(x*f_x)
E_x


# marginal variance of x

V_x <- sum((x - E_x)^2 * f_x)
Sd_x <- sqrt(V_x )

V_x
Sd_x
#-------------------------------------

# marginal - y
f_y <- apply(joint_table, 1, sum)
f_y

# marginal expectation of y
E_y <- sum(y*f_y)

E_y

# marginal variance of x

V_y <- sum((y - E_y)^2 * f_y)
Sd_y <- sqrt(V_y )

V_y
Sd_y

#---------------------------------

# covariance


# we need probabilities in this sequence

c(outer(y, x))

# sp let's do this

cov_xy <- sum(c(outer((y - E_y), (x - E_x))) * joint_prob) 
cov_xy

corr_xy <- (cov_xy) / (Sd_x*Sd_y)
corr_xy

