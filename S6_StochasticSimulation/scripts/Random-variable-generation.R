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
# Optional parameters are min and max, bounds of interval
# Default is min=0 and max=1

runif(100, min=2, max=5) # produces 100 random variables
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

#### THE INVERSE TRANSFORM (SEE SLIDES FIRST)

# Here we create histograms of exponential random variables
# using the inverse transform (right) and using the R
# command rexp (left), with the Exp(1) density on top

Nsim=10^4 #number of random variables
U=runif(Nsim)
X=-log(U) #transforms of uniforms
Y=rexp(Nsim) #exponentials from R
par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Exp from Uniform")
hist(Y,freq=F,main="Exp from R")

# If we compare the output from probability inverse
# transform with output from rexp(), the fits to their
# exponential limit are not distinguishable.

#### GENERAL TRANSFORMATION METHODS

U=runif(3*10^4)
U=matrix(data=U,nrow=3) # matrix for sums
X=-log(U) # uniform to exponential
X=2* apply(X,2,sum) # sum up to get chi squares

system.time(test1());system.time(test2())

## A Normal Generator

sadmvn(low=c(1,2,3),upp=c(10,11,12),mean=rep(0,3),var=B)

## Discrete Distributions

Nsim=10^4; lambda=100
spread=3*sqrt(lambda)
t=round(seq(max(0,lambda-spread),lambda+spread,1))
prob=ppois(t, lambda)
X=rep(0,Nsim)
for (i in 1:Nsim){
  u=runif(1)
  X[i]=t[1]+sum(prob<u) }

# Example of negative binomial random variable

Nsim=10^4
n=6;p=.3
y=rgamma(Nsim,n,rate=p/(1-p))
x=rpois(Nsim,y)
hist(x,main="",freq=F,col="grey",breaks=40)
lines(1:50,dnbinom(1:50,n,p),lwd=2,col="sienna")

#####   ACCEPT-REJECT METHODS (SEE SLIDES)

u=runif(1)*M
y=randg(1)
while (u>f(y)/g(y)){
  u=runif(1)*M
  y=randg(1)}

# Accept-Reject Example

optimize(f=function(x){dbeta(x,2.7,6.3)},
         interval=c(0,1),max=T)$objective

Nsim=2500
a=2.7;b=6.3
M=2.67
u=runif(Nsim,max=M) #uniform over (0,M)
y=runif(Nsim) #generation from g
x=y[u<dbeta(y,a,b)] #accepted subsample

# Another example

x=NULL
while (length(x)<Nsim){
  y=runif(Nsim*M)
  x=c(x,y[runif(Nsim*M)*M<dbeta(y,a,b)])}
x=x[1:Nsim]

# Continuation of two examples ago

optimize(f=function(x){dbeta(x,2.7,6.3)/dbeta(x,2,6)},
         max=T,interval=c(0,1))$objective

