



# Let's create a joint probability table matrix

# This is the example from slides (this is the drug example)

x <- c(1, 2, 3, 4)
y <- c(1, 0)

joint_prob <- c(0.12, 0.15, 0.08, 0.17, 0.15, 0.10, 0.16, 0.07)

joint_table <- matrix(joint_prob, ncol = 4) #it fills rows first
joint_table

sum(joint_table)


# plot it

x_data <- sort(rep(x, times = 2))
y_data <- rep(y, times = 4)

data.frame(x_data, y_data)

#install.packages("plot3D")
library("plot3D")

scatter3D(x_data, y_data, joint_prob, phi = 7, bty = "g",  type = "h", col = "#0072B2",
          ticktype = "detailed", pch = 20, cex = 1.5, xlim = c(0, 5), ylim = c(-1, 2))

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

#---------------- conditional PMFs when conditioning on X

# Y|X = 1

f_Y_givenx1 =  joint_table[, 1] / f_x[1]
f_Y_givenx1

E_Y_givenx1 = sum(y*f_Y_givenx1)
E_Y_givenx1

V_Y_givenx1 = sum((y-E_Y_givenx1)^2*f_Y_givenx1)
V_Y_givenx1

# Y|X = 2

f_Y_givenx2 =  joint_table[, 2] / f_x[2]
f_Y_givenx2

E_Y_givenx2 = sum(y*f_Y_givenx2)
E_Y_givenx2

V_Y_givenx2 = sum((y-E_Y_givenx2)^2*f_Y_givenx2)
V_Y_givenx2


# Y|X = 3

f_Y_givenx3 =  joint_table[, 3] / f_x[3]
f_Y_givenx3

E_Y_givenx3 = sum(y*f_Y_givenx3)
E_Y_givenx3

V_Y_givenx3 = sum((y-E_Y_givenx3)^2*f_Y_givenx3)
V_Y_givenx3



# Y|X = 4

f_Y_givenx4 =  joint_table[, 4] / f_x[4]
f_Y_givenx4

E_Y_givenx4 = sum(y*f_Y_givenx4)
E_Y_givenx4

V_Y_givenx4 = sum((y-E_Y_givenx4)^2*f_Y_givenx4)
V_Y_givenx4


# Law of iterated expectations
sum(c(E_Y_givenx1, E_Y_givenx2, E_Y_givenx3, E_Y_givenx4)*f_x)



# law of conditional variance

term1 <- sum(c(V_Y_givenx1, V_Y_givenx2, V_Y_givenx3, V_Y_givenx4)*f_x)
term2 <- sum(c( (E_Y_givenx1-E_y)^2, (E_Y_givenx2-E_y)^2, (E_Y_givenx3-E_y)^2, (E_Y_givenx4-E_y)^2)*f_x)

term1 + term2


#--------------------the conditional pmf for y


# X|Y = 0

f_X_giveny0 =  joint_table[2, ] / f_y[2]
f_X_giveny0

E_X_giveny0 = sum(x*f_X_giveny0)
E_X_giveny0

#V_Y_givenx1 = sum((y-E_Y_givenx1)^2*f_Y_givenx1)
#V_Y_givenx1

# X|Y = 1

f_X_giveny1 =  joint_table[1, ] / f_y[1]
f_X_giveny1

E_X_giveny1 = sum(x*f_X_giveny1)
E_X_giveny1

V_Y_givenx2 = sum((y-E_Y_givenx2)^2*f_Y_givenx2)
V_Y_givenx2



