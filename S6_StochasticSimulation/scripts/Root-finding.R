##############################################
######    CHAPTER 10 ROOT FINDING       ######
##############################################

# Chapters 10, 11 and 12 discuss Root-finding, Numerical Integration,
# and Optimization, respectively. These are numerical algorithms
# for solving some common applied mathematical problems.

# Chapter 10 focuses on Root-finding, and covers (1) fixed-point
# iteration; (2) the Newton-Raphson method; (3) the secant method;
# and (4) the bisection method.

# Suppose you have some continuous function f. A root of f is a 
# solution to the equation f(x) = 0. That is, a root is a number
# a such that f(a) = 0.

# If you draw a graph of the function, say y = f(x), a solution
# of f(x) = 0 is the x-coordinate of the point at which the
# plotted curve crosses the x-axis.

################################################################
#####                FIXED POINT ITERATION                 #####
################################################################

# Let g be a continuous function. A fixed point of g is a number a
# such the g(a) = a. That is, a is a solution of the equation
# g(x) = x. Graphically, a fixed point is where the graph y = g(x)
# of the function crosses the line y = x.

# Can show that the computational problem of finding fixed points
# can be reduced to the problem of finding roots and vice versa.
# For example, consider the function f(x) = c(g(x)-x), where c is
# a non-zero constant. Then, substituting, one clearly has f(a) = 0
# if and only if g(a) = a.

# The 'fixed point method' is an iterative method for solving
# g(x) = x. It generates a sequence of points x0, x1, x2,...
# that converges to some point a such that g(a) = a.

# We start with an initial guess x0, and we generate the next
# guess using x1 = g(x0) and then repeat. This yields the
# following first-order recurrence relation: x'sub'(n+1)=g(xsubn).

# Once we begin the algorithm, it either converges or diverges on
# the fixed point.

### FIXED POINT ALGORITHM EXAMPLE: fixedpoint.r

# Code implements fixed-point algorithm in a function fixedpoint.
# To use it, you first need to create a function, ftn(x) that
# returns g(x). So fixedpoint(ftn, x0, tol = 1e-9, max.iter=100)
# has 4 inputs:

# 1) ftn is the name of a function that takes a single numeric
#    input and returns a single numeric result.

# 2) x0 is the starting point (initial guess) for the algorithm.

# 3) tol is such that the algorithm will stop if
#    |xsubn - xsub(n-1)| is less than or = to tol,
#    default 10 to the minus 9th powe (10^-9).

# 4) max.iter is such that the algorithm will stop when
#    n = max.iter, with default 100.

# Because it might diverge (not converge), we count how many
# iterations have been performed and stop when we exceed
# some maximum.

# So here is our code that implements the fixed point algorithm:

# program spuRs/resources/scripts/fixedpoint.r
# loadable spuRs function

fixedpoint <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  # applies the fixed-point algorithm to find x such that ftn(x) == x
  # we assume that ftn is a function of a single variable
  #
  # x0 is the initial guess at the fixed point
  # the algorithm terminates when successive iterations are
  # within distance tol of each other,
  # or the number of iterations exceeds max.iter
  
  # do first iteration
  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  cat("At iteration 1 value of x is:", xnew, "\n")
  ?abs
  # continue iterating until stopping conditions are met
  while ((abs(xnew-xold) > tol) && (iter < max.iter)) {
    xold <- xnew;
    xnew <- ftn(xold);
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }
  
  # output depends on success of algorithm
  if (abs(xnew-xold) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(xnew)
  }
}

# Accompanying fixedpoint.r in spuRs (but not in the book)
# is the program "fixedpoint_show.r". The spuRs documentation
# (see your folder for today) describes this program as
# "applies the fixed point algorithm to find x such that 
# ftn(x) == x, and plots the process"

# program spuRs/resources/scripts/fixedpoint_show.r
# loadable spuRs function

fixedpoint_show <- function(ftn, x0, xmin = x0-1, xmax = x0+1) {
  # applies fixed-point method to find x such that ftn(x) == x
  # x0 is the starting point
  # subsequent iterations are plotted in the range [xmin, xmax]
  
  # plot the function
  x <- seq(xmin, xmax, (xmax - xmin)/200)
  fx <- sapply(x, ftn)
  plot(x, fx, type = "l", xlab = "x", ylab = "f(x)",
       main = "fixed point f(x) = x", col = "blue", lwd = 2)
  lines(c(xmin, xmax), c(xmin, xmax), col = "blue")
  
  # do first iteration
  xold <- x0
  xnew <- ftn(xold)
  lines(c(xold, xold, xnew), c(xold, xnew, xnew), col = "red")
  lines(c(xnew, xnew), c(xnew, 0), lty = 2, col = "red")
  
  # continue iterating while user types "y"
  cat("last x value", xnew, " ")
  continue <- readline("continue (y or n)? ") == "y"
  while (continue) {
    xold <- xnew;
    xnew <- ftn(xold);
    lines(c(xold, xold, xnew), c(xold, xnew, xnew), col = "red")
    lines(c(xnew, xnew), c(xnew, 0), lty = 2, col = "red")
    cat("last x value", xnew, " ")
    continue <- readline("continue (y or n)? ") == "y"
  }
  
  return(xnew)
}


# EXAMPLE DOMAIN: Finding the root of f(x)=log(x)-exp(-x)
# We consider three approaches to solving the equation 
# f(x) = log(x)-exp(-x) = 0.

# 1) Put one term on each side of the equation and exponentiate
#    both sides to get x = exp(exp(-x)) = g1(x)
#    e.g. log(x) = exp(-x) then exponentiate both sides.

# 2) Subtract each side from x to get x = x - log(x)+exp(-x) = g2(x)
#    e.g. (x - (log(x)-exp(-x))) = (x-0)

# 3) Add x to both sides to get x = x + log(x)-exp(-x) = g3(x))
#    e.g. (x + (log(x)-exp(-x))) = x

##################################################################
##     Apply fixed-point method to g1 - 1st of three ftn()s     ##
##################################################################

# Sequence appears to converge but it takes 14 iterations for
# successive guesses to agree to 6 decimal places.
source("fixedpoint.r")
source("fixedpoint_show.r")
ftn1 <- function(x) return(exp(exp(-x)))
fixedpoint(ftn1, 2, tol = 1e-06)
fixedpoint_show(ftn1, 2)

##################################################################
##     Apply fixed-point method to g2 - 2nd of three ftn()s     ##
##################################################################

# Using g2, we find sequence appears to converge and it takes
# only 6 iterations:
ftn2 <- function(x) return(x - log(x) + exp(-x))
fixedpoint(ftn2, 2, tol = 1e-06)
fixedpoint_show(ftn2, 2)

##################################################################
##     Apply fixed-point method to g3 - 3rd  of three ftn()s    ##
##################################################################

# Using g3, we find the sequence does not appear to converge at all.
ftn3 <- function(x) return(x + log(x) - exp(-x))
fixedpoint(ftn3, 2, tol = 1e-06)
fixedpoint_show(ftn3, 2)

# This example illustrates that as a method for finding roots,
# the fixed-point method has disadvantages:

# One needs to convert the problem into fixed-point form, but there
# are many ways to do this, each of which will have different
# convergence properties and some of which will not converge at all.

# Fixed-point method is also relatively slow, in that the error is 
# usually divided by a constant factor at each iteration. Both of
# our next two algorithms, the Newton-Raphson method and the secant
# method, converge more quickly because they make informed guesses
# as to where to find a better approximation to the root.

################################################################
#####                NEWTON-RAPHSON METHOD                 #####
################################################################

# Suppose our function f is differentiable with continuous
# derivative f-prime and a root a. Let xsub0 be a member of set R.
# Think of xsub0 as our current 'guess' at a. Now the straight 
# line through the point (xsub0, f(xsub0)) is the best straight
# line approximation to the function f(x) at the point xsub0
# (note: this is the meaning of the derivative). The equation to
# this straight line is given by:

# f-prime(xsub0) = f(xsub0)-y / xsub0-x

# This straight line crosses the x-axis at a point xsub1, which
# should be a better approximation than xsub0 to a. To find xsub1
# we observe f-prime(xsub0) = f(xsub0)-0 / xsub0-xsub1 and so
# xsub1 (better guess of a) = xsub0-f(xsub0) / f-prime(xsub0)

# In other words, next best guess xsub1 is obtained from the
# current guess xsub0 by subtracting a correction term
# f(xsub0)/f-prime(xsub0) (see figure 10.3)

## LOOK AT SLIDES FOR CHAPTER 10 BEFORE CONTINUING

# Code implements Newton-Raphson method in function newtonraphson.r.
# To use it, you first need to create a function, ftn(x) that
# returns g(x). So fixedpoint(ftn, x0, tol = 1e-9, max.iter=100)
# has 4 inputs:

# 1) ftn is the name of a function that takes a single numeric
#    input and returns a numeric vector of length 2.

# fixedpoint returned a single numeric value.

# 2) x0 is the starting point (initial guess) for the algorithm.

# 3) tol is such that the algorithm will stop if
#    |f(xsubn)| is less than or = to tol,
#    default 10 to the minus 9th power (10^-9).

# fixed point compared current guess to previous guess 
# as it computer the difference between the two as compared
# to the tolerance so it had to store the previous iteration's
# guesstimate.

# 4) max.iter is such that the algorithm will stop when
#    n = max.iter, with default 100.

# Because it might diverge (not converge), we count how many
# iterations have been performed and stop when we exceed
# some maximum.

# So here is our code that implements the fixed point algorithm:

# program spuRs/resources/scripts/newtonraphson.r
# loadable spuRs function

newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  # Newton_Raphson algorithm for solving ftn(x)[1] == 0
  # we assume that ftn is a function of a single variable that returns
  # the function value and the first derivative as a vector of length 2
  #
  # x0 is the initial guess at the root
  # the algorithm terminates when the function value is within distance
  # tol of 0, or the number of iterations exceeds max.iter
  
  # initialise
  x <- x0
  fx <- ftn(x)
  iter <-  0
  
  # continue iterating until stopping conditions are met
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <-  iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  
  # output depends on success of algorithm
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(x)
  }
}

# When applied to the function log x - exp(-x) with derivative 1/x+exp(-x)
# we get impressively fast convergence:
source("newtonraphson.r")
ftn4 <- function(x) {
  fx <- log(x) - exp(-x)
  dfx <- 1/x + exp(-x)
  return(c(fx, dfx))
}

source("newtonraphson.r")
newtonraphson(ftn4, 2, 1e-06)

# We also have this new newtonraphson_show function:

newtonraphson_show <- function(ftn, x0, xmin = x0-1, xmax = x0+1) {
  # applies Newton-Raphson to find x such that ftn(x)[1] == 0
  # x0 is the starting point
  # subsequent iterations are plotted in the range [xmin, xmax]
  
  # plot the function
  x <- seq(xmin, xmax, (xmax - xmin)/200)
  fx <- c()
  for (i in 1:length(x)) {
    fx[i] <- ftn(x[i])[1]
  }
  plot(x, fx, type = "l", xlab = "x", ylab = "f(x)",
       main = "zero f(x) = 0", col = "blue", lwd = 2)
  lines(c(xmin, xmax), c(0, 0), col = "blue")
  
  # do first iteration
  xold <- x0
  f.xold <- ftn(xold)
  xnew <- xold - f.xold[1]/f.xold[2]
  lines(c(xold, xold, xnew), c(0, f.xold[1], 0), col = "red")
  
  # continue iterating while user types "y"
  cat("last x value", xnew, " ")
  continue <- readline("continue (y or n)? ") == "y"
  while (continue) {
    xold <- xnew;
    f.xold <- ftn(xold)
    xnew <- xold - f.xold[1]/f.xold[2]
    lines(c(xold, xold, xnew), c(0, f.xold[1], 0), col = "red")
    cat("last x value", xnew, " ")
    continue <- readline("continue (y or n)? ") == "y"
  }
  
  return(xnew)
}

source("newtonraphson_show.r")
newtonraphson_show(ftn4, 2, 1e-06)

##################################################################
#####         WE SKIP SECANT METHOD AS AUTHORS               #####
#####         DO NOT PROVIDE CODE TO IMPLEMENT               #####
##################################################################

################################################################
#####                   BISECTION METHOD                   #####
################################################################

###   PLEASE VIEW REMAINING CHAPTER 10 SLIDES BEFORE CONTINUING

# Note that the bisection method cannot find a root a if the
# function f just touches the x-axis at a, that is, if the x-axis
# is a tangent to the function at a. However, the Newton-Raphson
# method will still work in this case.

# The most popular current root-finding methods use root-bracketing
# to get close to a root, then switch over to Newton-Raphson or
# secant method when it seems safe to do so. This strategy combines
# the safety of bisection with the speed of the secant method.

# Here is an implementation of bisection method in R. Because this
# algorithm makes certain assumptions about xsub1 and xsubr, we
# check that these assumptions hold before the algorithm runs.

# Also, because the algorithm is guaranteed to converge (provided
# that the initial conditions are met), we do not need to put a
# bound on the maximum number of iterations.

# Note code has a number of return statement. A function terminates
# the first time a return statement is executed (encountered). So
# here, if we detect a problem with the inputs then we print an
# error message and immediately return(NULL), so that the remainder
# of the function is not executed.

# program spuRs/resources/scripts/bisection.r
# loadable spuRs function

bisection <- function(ftn, x.l, x.r, tol = 1e-9) {
  # applies the bisection algorithm to find x such that ftn(x) == 0
  # we assume that ftn is a function of a single variable
  #
  # x.l and x.r must bracket the fixed point, that is
  # x.l < x.r and ftn(x.l) * ftn(x.r) < 0
  #
  # the algorithm iteratively refines x.l and x.r and terminates when
  # x.r - x.l <= tol
  
  # check inputs
  if (x.l >= x.r) {
    cat("error: x.l >= x.r \n")
    return(NULL)
  } 
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    return(x.l)
  } else if (f.r == 0) {
    return(x.r)
  } else if (f.l * f.r > 0) {
    cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }
  
  # successively refine x.l and x.r
  n <- 0
  while ((x.r - x.l) > tol) {
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      return(x.m)
    } else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    } else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
    cat("at iteration", n, "the root lies between", x.l, "and", x.r, "\n")
  }
  
  # return (approximate) root
  return((x.l + x.r)/2)
}

# Here we fire it up. Notice how slow the function is compared to
# Newton-Raphson

source("bisection.r")
ftn5 <- function(x) return(log(x) - exp(-x))
bisection(ftn5, 1, 2, tol = 1e-06)