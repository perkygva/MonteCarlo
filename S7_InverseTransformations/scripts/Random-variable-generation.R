####################################################
#####                                          #####
#####       RANDOM  VARIABLE  GENERATION       #####
#####                                          #####
####################################################

# Monte Carlo methods mostly rely on the possibility
# of producing (with a computer) a supposedly endless 
# flow of random variables for well-known or
# new distributions. Such a simulation is, in turn, 
# based on the production of uniform random variables
# on the interval (0; 1). 

# The uniform distribution U[0;1] provides the basic 
# probabilistic representation of randomness on 
# computer and the generators for all other
# distributions require a sequence of uniform 
# variables to be simulated.

#####   UNIFORM  SIMULATION

# Basic uniform generator is runif(), only required
# entry is number of values to be generated.
# Optional parameters are min and max, bounds of
# the interval. Default is min=0 and max=1

runif(100, min=2, max=5) # 100 rs
        # uniformly distributed between 2 and 5

# We can check the properties

Nsim=10^4    # number of random numbers
x=runif(Nsim)
x1=x[-Nsim]  # vectors to plot
x2=x[-1]     # adjacent pairs
par(mfrow=c(1,3))  # put three plots on a frame

# histogram:
hist(x)

# scatterplot:
plot(x1,x2) # plot of (x_sub_i, x_sub_i+1)

# 
acf(x) # estimated autocorrelation function...random
       # variable generators suffer from residual
       # autocorrelation

# Plots indicate runif() is acceptable for causal
# evaluation as producing a uniform distribution

#### THE INVERSE TRANSFORM (SEE SLIDES)

# TALK TO SLIDE #2, then show example below

# Here we create histograms of exponential rv
# using the inverse transform (right) and the R
# command rexp (left), with the Exp(1) density on top

# EXAMPLE #1: Uniform --> Exponential

Nsim=10^4 #number of random variables
U=runif(Nsim)
X=-log(U) #transforms of uniforms
# plot of rexp() is indistinguishable
Y=rexp(Nsim) #exponentials from R
par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Exp from Uniform")
hist(Y,freq=F,main="Exp from R")

# If we compare the output from probability inverse
# transform with output from rexp(), the fits to their
# exponential limit are not distinguishable.

# CONSEQUENTLY THE GENERATION OF UNIFORM RANDOM
# VARIABLES CAN BE A KEY DETERMINANT OF BEHAVIOR
# OF SIMULATION METHODS FOR OTHER PROBABILITY
# DISTRIBUTIONS SINCE THOSE DISTRIBUTIONS CAN
# BE REPRESENTED AS A DETERMINISTIC TRANSFORMATION
# OF UNIFORM RANDOM VARIABLES.

#### GENERAL TRANSFORMATION METHODS (using other
# distributions, not uniform, as starting point).

# So now we have some tools: When any distribution
# with known density f can be linked to another
# distribution that is easy to simulate, this
# relationship can be exploited to create an
# algorithm to simulate variables from f.

# Example: Previously we generated an exponential
# random variable from a uniform. Now we illustrate
# generating random variables from an exponential.

# Xi's are iid Exp(1) random variables
# We generate chi-square with 6 dof
test1 <- function(){
  U=runif(3*10^4)
  U=matrix(data=U,nrow=3) # U is 3 x 1000 matrix for sums
  X=-log(U) # uniform to exponential
  X=2* apply(X,2,sum) # sum up to get chi squares
}

# test1 performs same R function test2 below

test2 <- function(){
  rchisq(10^4,df=6)
}

# test1 is not as efficient as calling rchisq:
system.time(test1());system.time(test2())

# Many other derivations of standard distributions
# are possible with this approach using
# existing probabilistic properties, but not all.
# For example, cannot get a standard normal using
# a tranform from the uniform distribution.

## A Normal Generator for continuous distributions

# Without explaining in detail, algorithms to generate
# standard normal from uniform using CLT are crudely
# approximate.

# Can use Box-Muller algorithm to achieve a normal
# random variable simulation w/ a transform (is exact).
# But, we don't talk about it here.

## Discrete Distributions

# We have an "all-purpose" algorithm to generate any
# discrete random variables using inverse transform
# principle.

# To generate X ~ P.theta with integers, can calculate
# probabilities: 
# p-sub-0 = P.theta(X<=0), 
# p-sub-1 = P.theta(X<=1),
# p-sub-2 = P.theta(X<=2), ...,

### BY:

# generating U ~ U[0,1] and approximating X as
# probability = k if p(k-1) < U < p(k).

##### FOR EXAMPLE To generate X ~ Bin(10,.3)
## LOOK AT BINOMIAL DISTRIBUTION IN WIKIPEDIA
# Probabilities are pbinom(k,10,.3), that is
# p0=0.028, p1=0.149, p2=0.382, ..., p10=1.

##### OR FOR EXAMPLE TO GENERATE X ~ Pois(7) we
## (LOOK AT POISSON IN WIKIPEDIA)
# use p0=0.0009, p1=0.0073, p2=0.0296, ..., where
# we stop the sequence when it comes within a
# reasonable tolerance of 1, p20 = 0.999985

## Here is R code to generate Poisson rv for
## large values of lambda (see Wikipedia).
# Sequence t contains integer values in the
# range around the mean

test3 <- function(){
Nsim=10^4; lambda=100 # lambda is 100
spread=3*sqrt(lambda)
# will go from 0 to 1 or from
# lambda - 3 to lambda + 3
t=round(seq(max(0,lambda-spread),lambda+spread,1))
# ppois is probability mass function for Poisson
prob=ppois(t, lambda)
# create X and fill with zeros:
X=rep(0,Nsim)
for (i in 1:Nsim){
  # one uniform sample over and over:
  u=runif(1)
  # key line: checks to see what interval the
  # uniform random variable fell in and assigns
  # the correct Poisson value to X
  X[i]=t[1]+sum(prob<u) 
  }
}

test3() # emulates rpois()

test4 <- function(n, lambda){ 
  rpois(n, lambda)
  }

Nsim = 10^4
lambda = 100
test4(Nsim,lambda) # simply calls rpois()

# Compare them, rpois is much faster:
# test3 is not as efficient as calling rpois:
system.time(test3());system.time(test4(Nsim,lambda))

#####   ACCEPT-REJECT METHODS

# So have many distributions where inverse
# or general transforms fail. What to do?

# Use indirect method: Generate 'candidate'
# random variable and accept it subject to
# passing a test.

## Must know:

# Functional form of the density f (target
# density) up to a multiplicative constant M

# We simulate with a simpler density g
# (candidate density) to generate rv.

# Constraints on candidate density g:

# 1) f and g have compatible supports
# (e.g. g(x) > 0 when f(x) > 0)

# 2) There is a constant M with 
# f(x)/g(x) l.t.e.t. M for all x.

# If these constraints are met, can test with
# an equality (SEE SLIDES). If inequality is 
# met, accept Y; otherwise, discard both
# Y and U and start over

# R implementation is of accept/reject 
# algorithm is straightforward. If randg()
# is a function that randomly generates
# from g() (like rnorm() or rpois())

# this code produces a single generation 
# y from f:
u=runif(1)*M
y=randg(1)
while (u>f(y)/g(y)){
  u=runif(1)*M
  y=randg(1)}

# Can prove that cdf of accepted random
# variable Y is exactly the cdf of X

# Accept-Reject Example: Beta Random Variable
# SEE WIKIPEDIA BETA DISTRIBUTION

# Cannot generate Beta rvs with general transforms
# But can do it with accept-reject method
# Use as instrumental distribution U[0,1]
# where both alpha and beta > 1 (note generic
# rbeta() function in R does not impose this
# restriction).

# Upper bound M is maximum of beta density...
# can obtain with optimize() function

?optimize
?dbeta # distribution function for beta

# This gives us M 
# optimize(f=function(x){dbeta(x,2.7,6.3)},
#         + interval=c(0,1),max=T)$objective
# [1] 2.669744

# Since the candidate density g is equal to one
# the proposed value Y is accepted if
# M x U < f(Y), that is, if M x U is under
# the beta density f at that realization.

# So alternative R code for Accept-Reject 
# with alpha=2.7 and beta=6.3 is:

Nsim=2500
a=2.7;b=6.3
M=2.67
u=runif(Nsim,max=M) #uniform over (0,M)
y=runif(Nsim) #generation from g
x=y[u<dbeta(y,a,b)] #accepted subsample