#######################################################
###       MONTE CARLO METHODS IN INFERENCE          ###        ###
#######################################################

# Monte Carlo Methods may refer to any method in
# statistical inference or numerical analysis where
# simulation is used.

# Here we just look at inference.
# Can use MC to estimate parameters of sampling
# distribution of a statistic, mean squared error (MSE),
# percentiles, or other measures of interest.

# In statistical inference there is uncertainty
# in an estimate. The methods we are going to look at
# now use repeated sampling from a given probability
# model, known as parametric bootstrap, to investigate.

# We simulate the stochastic process that generated
# the data, repeatedly drawing samples under identical
# conditions.

# Other MC methods known as nonparametric use repeated
# sampling from an observed sample

### MONTE CARLO METHODS FOR ESTIMATION

## Let's begin with simply estimating a probability.
## Sometimes this is referred to as computing an
## expectation of a random variable.

## If you have a random variable X with a density function
## f(x) and we want to compute the expectation of a 
## function h(x) which models the probability of f(X),
## (or the area under the curve of f(x)), then h(x) can
## be expressed as the integral of f(x).

## Sam and Annie from 'Sleepless in Seattle'

## Let A and S represent Sam's and Annie's arrival
## times at the Empire State Building, where we measure
## the arrival time as the number of hours after noon.

## We assume A and S are independent and uniformly
## distributed and that Annie arrive somewhere between
## 10:30 and midnight and Sam arrives somewhere between
## 10:00 and 11:30PM.

## Our Questions are:
# 1) What is the probability that Annie arrives before Sam?
# 2) What is expected difference in arrival times?

## FIRST QUESTION
# We simulate a large number of values from distribution
# of (A,S) (say, 1000) where A and S are independent:

sam = runif(1000, 10, 11.5)
annie = runif(1000, 10.5, 12)

# We want the probability P(A < S) which is estimated by
# the proportion of simulated pairs (a,s) where a is smaller
# than s

prob = sum(annie < sam) / 1000 # sum() is vectorized
prob

# [1] 0.224
# Estimated probability that Annie arrives before Sam
# is 0.224

# shaded region shows area A < S

plot(sam,annie)
polygon(c(10.5, 11.5, 11.5, 10.5),
        c(10.5, 10.5, 11.5, 10.5), density = 10, angle = 135)

# standard error of this estimate is
sqrt(prob * (1 - prob) / 1000)
# [1] 0.01318423

## ESTIMATING AN EXPECTATION: Question #2
# What is the expected difference in the arrival times?

# Annie more likely to arrive later, so we model E(A-S)
difference = annie - sam

# Monte Carlo estimate is mean of these differences
mc.est = mean(difference)
mc.est
# [1] 0.485206

# Estimated standard error is sd of diffs divided by
# square root of simulation sample size:
se.est = sd(difference) / sqrt(1000)
c(mc.est, se.est)
# [1] 0.4852060 0.0194314

# So we estimate that Annie will arrive 0.485 hours
# after Sam arrives. Since standard error is only 
# 0.02 hours, we can be 95% confident that the true 
# difference is within 0.04 hours of this estimate

#### GENERAL CASE with standard Normal distributions

# Suppose the X1, X2 are iid from a standard normal 
# distribution. We want to estimate the mean difference
# E|X1 - X2| (expected value of absolute difference)

### Example of Basic Monte Carlo estimation

# This one is easy. We generate a large number of
# random samples of size 2 from a standard normal
# distribution, then compute the replicate pairs' mean
# differences, and then the mean of those differences.

m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}
est <- mean(g)
est

# [1] 1.098206
# this time we got 1.0982
# second time got 1.142562

# can prove by integration that it equals 2/sqrt(pi)
# or 1.128379 and that the variance is 2-4/(pi) and 
# the standard error of the estimate is

sqrt(sum((g - mean(g))^2)) / m

# or [1] 0.02709247 is this example

### Example of Estimating the MSE of a trimmed mean

# A trimmed mean can be used to estimate the center
# of a continuous symmetric distribution that is
# not normal

# You get a trimmed mean by averaging all but the
# largest and smallest sample observations.

# Here the center is 0, we implement by writing a for
# loop (could also use replicate() function)

n <- 20   # vector is 20 long
m <- 1000 # go for 1000 replications
tmean <- numeric(m) # initialize numeric vector of 1000
for (i in 1:m) {  # loop through 1000 times
  x <- sort(rnorm(n)) # sort n is vector of 20
  tmean[i] <- sum(x[2:(n-1)]) / (n-2) # trim off 
}                                     # first & last
mse <- mean(tmean^2) # determine mse
mse
sqrt(sum((tmean - mean(tmean))^2)) / m    #se

# [1] 0.04978369 here mse is 0.049

# true mse Var(X)/n = 1/20 or 0.05 here

# standard error (est) is [1] 0.007055548

### Estimate MSE of trimmed median

# actually, median is a trimmed mean
# So here we repeat for the median

n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
  x <- sort(rnorm(n))
  tmean[i] <- median(x) # only difference in this line
}
mse <- mean(tmean^2)
mse
sqrt(sum((tmean - mean(tmean))^2)) / m    #se

# here [1] 0.07411576 is median (est)

# and standard error (est) [1] 0.008608699

##### ESTIMATING A CONFIDENCE LEVEL

# Often need to evaluate cdf of sampling distribution
# of a statistic, when density function is unknown.

# For example, often assume that sampled population is
# normally distributed. If population non-normal, true
# distribution of the estimator may be unknown / intractable.

# This is a problem of integration, when you can generate
# the distribution g(X) but the true function g(x) is
# unknown.

### Example of Confidence interval for variance

# Consider confidence interval for variance, is very
# sensitive to mild departures from normality. we can
# use MC methods to estimate true confidence level when
# the normal theory confidence interval for variance is
# applied to non-normal data.

# Here we calculate 95% Upper Confidence Limit (UCL)
# for random sample size n=20 from a 
# n(0, var=4) distribution (sigma-squared is 4 in this case)

n <- 20      # sample size
alpha <- .05  # alpha when want 95% UCL
x <- rnorm(n, mean=0, sd=2) # normal, variance = 4
# qchisq() is quantile function for chi-sq dist w/ df
(UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1))

# Do it several times, Upper Confidence Limit
# (UCL) all contain sigma-squared = 4

# If we do this  a large number of times, approximately
# 95% of the intervals should contain sigma-squared (4)
# assuming sampled population is normal with a
# variance sigma-squared.

### Example of MC estimate of confidence level

# Simulation experiment: Repeat large number of times,
# compute the proportion of intervals that contain
# the target parameter

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = { # we do above 1000 times
  x <- rnorm(n, mean = 0, sd = 2)
  (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
# count the number of intervals that contain sigma^2=4
sum(UCL > 4)

# [1] 949

#or compute the mean to get the confidence level
mean(UCL > 4)

# [1] 0.949

# result is that 949 intervals satisfied (UCL > 4) so
# empirical confidence level is 94.9%, very close to
# theoretical value of 95%.

# Can show that standard error of the estimate
# at 95% UCL is exactly 0.00689

# Could also accomplish call above like this

# Use calcCI as function without replicate
calcCI <- function(n, alpha0) { # as a function
  y <- rnorm(n, mean = 0, sd = 2)
  return((n-1) * var(y) / qchisq(alpha, df = n-1))
} 

# put relicate, n, alpha here where call the function
UCL <- replicate(1000, expr = calcCI(n=20, alpha = .05))

# count the number of intervals that contain sigma^2=4
sum(UCL > 4)
mean(UCL > 4)

### Example of Empirical confidence level

# What happens is sampled population is non-normal?
# Suppose have sampled pop of chi-Sq(2) which has a
# variance of 4, but is distinctly non-normal.

# We simply repeat the simulation, replacing N(0,4)
# samples with Chi-Sq(2) samples

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
  x <- rchisq(n, df = 2) # do not use rnorm() to sample
  (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
sum(UCL > 4)
# [1] 794
mean(UCL > 4)
# [1] 0.794

# In this experiment, only 794 of the intervals contained
# the population variance, which is far less than the 95%
# coverage under normality.

## NOTE: all examples above are 'parametric' in sense that
## distribution of sampled population is specified. MC
## approach here is called 'parametric bootstrap'

## Is in contrast to the 'ordinary bootstrap' where samples
## are generated from observed samples where distribution 
## is NOT specified, is sometimes called 'non-parametric'
## and includes bootstrapping and jackknifing, which can
## also be used to estimate the bias and the standard
## error of an estimate.

#######################################################
#####                  NEXT  ARE                  #####
#####               HYPOTHESIS  TESTS             #####
#######################################################