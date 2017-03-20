##########################################
######  NUMERICAL INTEGRATION       ######
##########################################

#####   SEE  SLIDES  BEFORE  CONTINUING

## CHAPTER 11 is about approximating the 
## area under a continuous function within 
## interval a, b (is the concept of the
## integral in calculus). All three approaches
## divide up the interval into n segments 
## that each have width h and then approximate
## the area of each n segments and add these
## areas together for total area.

## Three approaches to do this:
# 1) Trapezoidal rule
# 2) Simpson's rule
# 3) Adaptive quadrature

# 1) Trapezoidal rule - approximate area with a series of
#    trapezoids...'straight line rule' or polynomial of
#    degree 1...works reasonably well...we also demonstrate 
#    that as n increases, the approximation improves. Also:
# 1a)Trapezoid rule gives exact result if f is a constant
#    or a linear function, otherwise there is some error.
# 1b)PGMs: trapezoid.r

# 2) Simpson's rule - subdivide interval[a,b] into n 
#    even number of subintervals. On each consecutive
#    pair of subintervals (the 'even' n's) approximate
#    behavior of f(x) by a parabola (polynomial of
#    degree 2) instead of straight line. In the
#    summation of the interval areas, simpson's rule:
# 2a)Weights f(x-sub-i) by 2x if i even (not 0 or n)
#    or by 4x if i is odd
# 2b)Provides exact estimate (no error) if f(x) is
#    quadratic (see quadratic function definition in
#    Wikipedia), also exact if f(x) is cubic.
# 2c)Generally gives superior results to trapezoidal.
# 2d)PGMs: simpson_n.r; Phi.r ("Big Phi" estimates
#    cumulative normal cdf); phi.r ("little phi"
#    estimates normal values of y given x);
#    simpson_test.r tests convergence to exact
#    interal when n increases; simpson.r - uses
#    tolerance level instead of partition size n.

# 3) Adaptive Quadrature - 

##########################################################
#####               TRAPEZOIDAL  RULE                #####
##########################################################

#####   SEE  SLIDES  BEFORE  CONTINUING

# Here is an implementation of the Trapezoidal rule
# We use it to estimate the integral from 0 to 1
# of 4x^3 dx = 1

# program spuRs/resources/scripts/trapezoid.r

trapezoid <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using the trapezoid rule with n subdivisions.
  # ftn is a function of a single variable;
  # we assume a < b and n is a positive integer;
  # h is width of each interval;
  h <- (b-a)/n
  # x.vec is vector from a to b that also marks
  # beginning and end of each subinterval:
  x.vec <- seq(a, b, by = h)
  # Given ftn is f(x), f.vec computes values of y
  # for each x-sub-i marking the intervals.
  # sapply below could be replaced with ftn(x.vec):
  f.vec <- sapply(x.vec, ftn)
  # Implements trapezoid rule, adds up the areas:
  T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
  return(T)
}
source("trapezoid.r")
# Note ftn6 below is vectorized (given vector as input
# it will return a vector as output). Thus, in trapezoid
# sapply(x.vec,ftn) could be replaced by ftn(x.vec). The
# only advantage of using sapply is that it will work
# even if ftn is not vectorized.
ftn6 <- function(x) return(4 * x^3)
trapezoid(ftn6, 0, 1, n = 20)
trapezoid(ftn6, 0, 1, n = 40)
trapezoid(ftn6, 0, 1, n = 60)
trapezoid(ftn6, 0, 1, n = 80)
trapezoid(ftn6, 0, 1, n = 100)

# The trapezoid rule gives exact results if f is constant
# or a linear function, otherwise there will be an error,
# corresponding to the extent that our trapezoidal approximation
# overshoots or undershoots the actual graph of f.

##########################################################
#####                SIMPSON'S  RULE                 #####
##########################################################

#####   SEE  SLIDES  BEFORE  CONTINUING

# Here is Simpson's Rule implemented in R:
#program spuRs/resources/scripts/simpson_n.r

simpson_n <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with n subdivisions
  #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive even integer
  
  # assures n is even, uses n or 4 (4 is minimum
  # number of intervals for simpson's rule),
  # which ever is greater:
  n <- max(c(2*(n %/% 2), 4))
  # h is width of intervals
  h <- (b-a)/n
  # odd x-sub-i (.02) segments from a (0) to b (1):
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  # even .02 segments from 0 to 1:
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  # calculating y values for each odd x-sub-i value:
  f.vec1 <- sapply(x.vec1, ftn)
  # calculating y values for each even x-sub-i value:
  f.vec2 <- sapply(x.vec2, ftn)
  # implements Simpson's rule (p. 190) and adds them up
  # Note: f(x-sub-i) for i odd are all weighted 4,
  # because appear in 4 intervals;
  # f(x-sub-i) for i even (except 0 and n) weighted 2
  # because appear in 2 intervals;
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
  return(S)
}
source("simpson_n.r")
ftn6 <- function(x) return(4 * x^3)
simpson_n(ftn6, 0, 1, 20)

# Important points:
# 1) Simpson's rule gives exact results if f(x) quadratic
# function since approximates each piece with parabola;
# 2) Also exact if f(x) is cubic
# 3) Generally better approx than trapezoidal rule.

### EXAMPLE: PGM Phi.r
## This is area under a normal curve
#### SEE SLIDES BEFORE CONTINUING

# Implements the distribution function of a normal
# or Gaussian random variable

# program spuRs/resources/scripts/Phi.r
# estimate and plot the normal cdf Phi

rm(list = ls()) # clear the workspace
source("simpson_n.r")                  # Make sure this is set correctly
phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))
Phi <- function(z) {
  if (z < 0) {
    # if z is negative, compute one half
    # (.5-total area) minus area from z to 0
    return(0.5 - simpson_n(phi, z, 0))
  } else {
    # if z is positive, compute one half
    # plus area from z to 0
    return(0.5 + simpson_n(phi, 0, z))
  }
}

z <- seq(-5, 5, by = 0.1)
# z goes from -5 to +5:
# "little phi" is normal curve, values of y for values of x:
phi.z <- sapply(z, phi) # y values for normal curve as f(x)
# z goes from -5 to +5:
# "big Phi" is cumulative distribution function:
Phi.z <- sapply(z, Phi) # cumulative area under curve as
                        # go from -5 to +5
# Draws cumulative normal dist func:
plot(z, Phi.z, type = "l", ylab = "", main = "phi(z) and Phi(z)")
lines(z, phi.z)    # draws normal curve

# getwd()
# source("Phi.r")
