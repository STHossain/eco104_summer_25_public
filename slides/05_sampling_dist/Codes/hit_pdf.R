

par(mfrow = c(2, 3))

set.seed(1234)
n <- 50
x <- rnorm(n, 10, .5)
x
min(x)
max(x)
hist(x, breaks = 5, freq = FALSE)
hist(x, breaks = 5, plot = FALSE)




n <- 100
x <- rnorm(n, 10, .5)
hist(x, breaks = 20, plot = FALSE)$breaks
hist(x, breaks = 20,  freq = FALSE)



n <- 500
x <- rnorm(n, 10, .5)
hist(x, breaks = 100, freq = FALSE)





n <- 1000
x <- rnorm(n, 10, .5)
hist(x, breaks = 100, freq = FALSE)





n <- 10000
x <- rnorm(n, 10, .5)
hist(x, breaks = 100, freq = FALSE)




n <- 50000
x <- rnorm(n, 10, .5)
hist(x, breaks = 100, freq = FALSE)






