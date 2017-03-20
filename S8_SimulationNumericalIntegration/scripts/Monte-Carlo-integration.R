##########################################
##                                      ##
##    MORE  MONTE  CARLO  INTEGRATION   ##
##                                      ##
##########################################

# Here we cover two simulation-based approaches
# to integration (1) Hit-and-miss; and the 
# (2) Improved Monte Carlo Integration method.

# Moving into two dimensions (negative numbers
# for y), must subtract area under curve for
# negative values of y.

## HIT-AND-MISS METHOD (SEE SLIDES)

# But we want I which = |A| + c(b - a)
# So if we can estimate |A| then we can
# estimate I.

# To estimate |A| imagine throwing
# darts randomly (and uniformly).
# On average the proportion that land under
# the curve will be given by the area of A
# A is the shaded area under curve.

# Then assume both X and Y are uniform.
# Have interval a, b along x, assume have y
# interval f(x) = c, d within x = a, b

# program spuRs/resources/scripts/hit_miss.r
hit_miss <- function(ftn, a, b, f.min, 
                     f.max, n) {
  # Monte-Carlo integration using the 
  # hit and miss method
  # ftn is a function of one variable x.
  # [a, b] is the range of integration
  # f.min and f.max are y bounds on 
  # ftn over the range [a, b]
  # that is f.min <= ftn(x) <= f.max 
  # for all x in [a, b]
  # n is the number of samples used 
  # in the estimation
  # that is the number of calls made 
  # to the function ftn
  Z.sum <- 0
  for (i in 1:n) {
    # Get Uniform for X
    X <- runif(1, a, b)
    # Get Uniform for Y
    Y <- runif(1, f.min, f.max)
    # How many time is ftn(x)
    # greater than or equal to Y?
    Z <- (ftn(X) >= Y)
    Z.sum <- Z.sum + Z
    # cat("X =", X, "Y =", Y, "Z =", Z, 
    # "Z.sum =", Z.sum, "\n")
  }
  # Compute the 2-dimensional integral
  I <- (b-a)*f.min+(Z.sum/n)*(b-a)*(f.max-f.min)
  # return it
  return(I)
}

# Use a specific quadratic function to estimate
# over interval 0 to 1. We know the integral is 
# is -1.0833
f <- function(x) x^3 - 7*x^2 + 1

hit_miss(f, 0, 1, -6, 2, 10)

hit_miss(f, 0, 1, -6, 2, 100)

hit_miss(f, 0, 1, -6, 2, 1000)

hit_miss(f, 0, 1, -6, 2, 10000)

hit_miss(f, 0, 1, -6, 2, 100000)

hit_miss(f, 0, 1, -6, 2, 1000000)

# We see that the number of repetitions must be
# very large just to get it right to two
# decimal places

## Vectorized version

hit_miss2 <- function(ftn, a, b, c, d, n) {
  # Monte-Carlo integration using the 
  # hit & miss method
  # vectorized version
  X <- runif(n, a, b)
  Y <- runif(n, c, d)
  # vectorized with sapply
  Z <- (Y <= sapply(X, ftn))
  # vectorized with cumsum()
  I <- (b - a)*c + (cumsum(Z)/(1:n))*(b - a)*(d - c)
  # have added a line to plot the
  # successive approximations to the integral
  plot(1:n, I, type = "l")
  return(I[n])
}

hit_miss2(f, 0, 1, -6, 2, 10000)
# this straight line is the value of I
lines(c(1, 10000), c(-13/12, -13/12))

## (IMPROVED) MONTE-CARLO INTEGRATION

# Hit-and-Miss converges very slowly. This is
# the technique people usually refer to as
# "Monte-Carlo Integration".

# When we say one Monte-Carlo technique is
# 'better', we mean that, using the same number
# of function calls, it has a smaller variance.

# SInce the estimates that we are making are based
# on random samples, they are themselves random
# samples.

# We consider the same integral I over interval
# a, b for f(x)dx.

mc.integral <- function(ftn, a, b, n) {
  # Monte-Carlo integral of ftn over [a, b] 
  # using a sample of size n
  u <- runif(n, a, b)
  x <- sapply(u, ftn)
  return(mean(x)*(b-a))
}

mc.integral(f, 0, 1, 100)
mc.integral(f, 0, 1, 1000)
mc.integral(f, 0, 1, 10000)
mc.integral(f, 0, 1, 100000)
mc.integral(f, 0, 1, 1000000)