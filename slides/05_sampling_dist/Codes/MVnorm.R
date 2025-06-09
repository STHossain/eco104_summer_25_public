

cone <- function(x, y){
  sqrt(x ^ 2 + y ^ 2)
}

# prepare variables.
x <- y <- seq(-1, 1, length = 30)
z <- outer(x, y, cone)

# plot the 3D surface
persp(x, y, z)


# plot the contour

library(MASS)

#make this example reproducible
set.seed(0)

#simulate bivariate normal distribution
bivariate_data <- as.data.frame(mvrnorm(n=100,
                                        mu=c(0, 0),
                                        Sigma=matrix(c(5, 3, 4, 4), ncol=2)))

#view first six rows of bivariate dataset
head(bivariate_data)

install.packages("mnormt")
library(mnormt)

#make this example reproducible
set.seed(0)

#create bivariate normal distribution
x     <- seq(-3, 3, 0.1) 
y     <- seq(-3, 3, 0.1)
mu    <- c(0, 0)
sigma <- matrix(c(1, 0, 
                  0, 1), nrow=2)
sigma
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

#create contour plot
contour(x, y, z)
persp(x, y, z, theta=10, phi=25, expand=0.6, ticktype='detailed')



