################################################
#####                                      #####
#####       CASE  STUDIES: EPIDEMICS       #####
#####                                      #####
################################################

# Capstone: Case Studies that demonstrate
# some of these simulation techniques

# Epidemiology: The study of the spread of disease.
# Math/Statistical models of how disease spreads.

## Susceptible, Infected, and Removed (SIR) Model
# Individuals are one of these three types:
# Susceptible: Not yet caught disease
# Infected: Currently have the disease
# Removed: Have had the disease and recovered
# and are now immune.

# We measure time in discrete steps. At each
# step, each infected can infect susceptibles
# or can recover (or die), at which point the
# infected is removed.

# At time t, have some # of S, I and R:
# S(t) = number of susceptibles at time t;
# I(t) = number of infected at time t;
# R(t) = number of removed at time t;

# At each time t, each I (Infected) has:
# 1. prob a of infecting a susceptible; and
# 2. prob b of being removed

# At time 0 (start):
# S(0) = N; # so total pop is N + 1 and fixed
# I(0) = 1;
# R(0) = 0 (zero);

# Total population is fixed at N + 1. That is
# S(t) + I(t) + R(t) = N + 1 for all t;

# Four rules / assumptions / rv derivations:

# Each time step t, the chance a susceptible
# remains uninfected is (1-a)^I(t).

# If susceptible fails to become infected,
# this implies S(t+1)~binom(S(t),(1-a)^I(t))

# Since each infected has chance b of being
# removed: R(t+1)~R(t)+binom(I(t),b)

# From 2 and 3 above we get I(t+1) from total
# pop: I(t+1)=N+1-R(t+1)-S(t+1)

# Now we have enough information to write a
# simulation of an SIR process:

# program spuRs/resources/scripts/SIRsim.r
# returns a (T+1 x 3) matrix of values for S, I, R
SIRsim <- function(a, b, N, T) { #T = no. time periods
  # Simulate an SIR epidemic
  # a is infection rate, b is removal rate
  # N initial susceptibles, 1 initial infected, 
  # simulation length T
  # T=100 (Demo what does)
  # N=50
  # SIRsim() returns a matrix size (T+1)*3
  # with columns S, I, R respectively
  S <- rep(0, T+1)
  I <- rep(0, T+1)
  R <- rep(0, T+1)
  S[1] <- N
  I[1] <- 1
  R[1] <- 0
  for (i in 1:T) {
    # from rule #2 above:
    S[i+1] <- rbinom(1, S[i], (1 - a)^I[i])
    # from rule #3 above:
    R[i+1] <- R[i] + rbinom(1, I[i], b)
    # from rule #4 above:
    I[i+1] <- N + 1 - R[i+1] - S[i+1]
  }
  # returns a matrix
  return(matrix(c(S, I, R), ncol = 3))
}

# set parameter values
N <- 1000 # population
T <- 100   # time periods
a <- 0.0005 # prob of infection
b <- 0.1 # prob of removal

# creat initial matrix of values for
# S and I and R

# show matrix of S, I and R's after 10 t's
# begin with population of 1,000, also = S
# will be different each time due to rbinom()

# can run them one at a time
(N*a);b
Z <- SIRsim(a,b,N,T)
colnames(Z) <- c("S", "I", "R")
# t(Z)
par(mfrow=c(3,1)) 
plot(Z[,"S"], type = "n",
     xlab = "generation", 
     ylab = "total population")
lines(Z[,"S"], col="blue")
plot(Z[,"I"], type = "n",
     xlab = "generation", 
     ylab = "number infected")
lines(Z[,"I"], col="red")
plot(Z[,"R"], type = "n",
     xlab = "generation", 
     ylab = "total removed")
lines(Z[,"R"])
par(mfrow=c(1,1))

# can run many
par(mfrow=c(1,1))
plot(Z[,"S"], type = "n",
     xlab = "generation", 
     ylab = "POP / INF / REM")
for (i in 1:20){
  Z <- SIRsim(a,b,N,T)
  colnames(Z) <- c("S", "I", "R")
  lines(Z[,"S"], col="blue")
  lines(Z[,"I"], col="red")
  lines(Z[,"R"])
}

# Want to know how a and b affect the size
# of an epidemic so we estimate E S[T] for
# different values of a and b, here
# a in the set [0.0001,0.001] and b in the
# set [0.1,0.5]. Then we plot on a 3-D
# graph

# program spuRs/resources/scripts/SIR_grid.r
# discrete SIR epidemic model
#
# initial susceptible population N
# initial infected population 1
# infection probability a
# removal probability b
#
# estimates expected final population size 
# for different values of
# the infection probability a
# and removal probability b
# we observe a change in behaviour about 
# the line Na = b

# (Na is the expected number of new infected 
# at time 1 and b is the
# expected number of infected 
# who are removed at time 1)
SIR <- function(a, b, N, T) {
  # simulates SIR epidemic model from 
  # time 0 to T
  # returns number of susceptibles, 
  # infected and removed at time T
  S <- N
  I <- 1
  R <- 0
  for (i in 1:T) {
    S <- rbinom(1, S, (1 - a)^I)
    R <- R + rbinom(1, I, b)
    I <- N + 1 - S - R
  }
  return(c(S, I, R))
}

# set parameter values
N <- 1000
T <- 100
# multiple values of a and b:
a <- seq(0.0001, 0.001, by = 0.0001) # 10 values
b <- seq(0.1, 0.5, by = 0.05) # 10 values

a;b;N;T # check values
(N*a);b
# simulate spread once for 10 times periods
SIR(a,b,N,T) # single value of a and b

# sample size for estimating E S[T]
# set sample size to 400
n.reps <- 400 
# file to save simulation results
f.name <- "SIR_grid.dat" 

# estimate E S[T] for each combination 
# of a and b
a
b

write(c("a", "b", "S_T"), file = f.name, 
      ncolumns = 3)
for (i in 1:length(a)) {
  for (j in 1:length(b)) {
    S.sum <- 0
    for (k in 1:n.reps) {
      S.sum <- S.sum + SIR(a[i], b[j], 
                           N, T)[1]
    }
    write(c(a[i], b[j], S.sum/n.reps), 
          file = f.name,
          ncolumns = 3, append = TRUE)
  }
}

# plot estimates in 3D
g <- read.table(f.name, header = TRUE)
library(lattice)
print(wireframe(S_T ~ a*b, data = g, 
                scales = list(arrows = FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab = "a", ylab = "b", 
                zlab = "E S[T]"))

# Can observe change in behavior about the line
# Na = B. Na is expected number of new infected
# at time 1 and B is the expected number of 
# infected who are remove at time 1.

# In the early stages of an epidemic:
# When Na > B --> BIG EPIDEMIC...implies
# expected number of new infected at time
# 1 is greater than expected number infected
# who are removed at time 1.

# When Na =< B --> EPIDEMIC SHRINKS

# To see why this occurs, we look at class of
# models called 'branching processes"

##### BRANCHING PROCESSES

# Branching processes alter the spatial
# restrictions and implications of SIR.
# Branching process ignores finite population
# restriction and is a good model for early
# stages of an epidemic.

# Branching processes are described in terms
# of births and population growth rather than
# infection. If Z-sub-n is the size of the
# population at generation/time n, at each
# step every individual independently gives
# birth to a random number of offspring, with
# distribution X, then dies

# If Z at time 0 is 1, then we can say
# Z at time n+1 = X-sub-n,1+...+X-sub-n, sub-z-n
# where X sub-n,i is the i-th family size
# in generation n

# Program spuRs/resources/scripts/bp.r
# branching process simulation.
# Uses ... for passing a variable number
# on inputs to a function:
bp <- function(gen, rv.sim, ...) {
  # population of a branching process 
  # from generation 0 to gen
  # rv.sim(n, ...) simulates 
  # n rv's from the offspring distribution
  # Z[i] is population at generation i-1; 
  # Z[1] = 1
  Z <- rep(0, gen+1)
  Z[1] <- 1
  for (i in 1:gen) {
    if (Z[i] > 0) {
      Z[i+1] <- sum(rv.sim(Z[i], ...))
    }
  }
  return(Z)
}

bp.plot <- function(gen, rv.sim, ..., 
                    reps = 1, 
                    logplot = TRUE) {
  # simulates and plots the 
  # population of a branching process
  # from generation 0 to gen; rv.sim(n, ...) 
  # simulates n rv's
  # from the offspring distribution
  # the plot is repeated reps times
  # if logplot = TRUE then the population 
  # is plotted on a log scale
  # Z[i,j] is population at generation 
  # j-1 in the i-th repeat
  Z <- matrix(0, nrow = reps, ncol = gen+1)
  for (i in 1:reps) {
    Z[i,] <- bp(gen, rv.sim, ...)
  }
  if (logplot) {
    Z <- log(Z)
  }
  
  plot(c(0, gen), c(0, max(Z)), type = "n", 
       xlab = "generation",
       ylab = if (logplot) "log population" else "population")
  for (i in 1:reps) {
    lines(0:gen, Z[i,])
  }
  return(invisible(Z))
}
?rbinom
# So we generate some sample output where
# X ~ binom(2, 0.6). We have 20 simulations
# over 20 generations. Note in many the pop-
# ulation has died out, in others is growing
# exponentially

bp.plot(20,rbinom,2,0.6,reps=20,logplot=F)

# Want to know relationship between offspring
# distribution X and growth of the process.

# Can write a function to plot expected 
# population at time T and expected family
# size.

# program spuRs/resources/scripts/bp_grid.r
bp.sim <- function(gen, rv.sim, ...) {
  # population of a branching process 
  # at generation gen
  # rv.sim(n, ...) simulates n rv's 
  # from the offspring distribution
  Z <- 1
  for (i in 1:gen) {
    if (Z > 0) {
      Z <- sum(rv.sim(Z, ...))
    }
  }
  return(Z)
}

# set parameter values
gen <- 50 # set T = 50
size <- 2
prob <- seq(0.3, 0.6, by = 0.01)
# sample size for estimating E Z
n.reps <- 100 
# estimate E Z for each value of prob
mu <- rep(0, length(prob))
Z.mean <- rep(0, length(prob))

for (i in 1:length(prob)) {
  Z.sum <- 0
  for (k in 1:n.reps) {
    Z.sum <- Z.sum + bp.sim(gen, rbinom, size, prob[i])
  }
  mu[i] <- size*prob[i]
  Z.mean[i] <- Z.sum/n.reps
}

# plot estimates
# note that values of log(0) 
# (= -infinity) are not plotted
plot(mu, log(Z.mean), type = "o",
     xlab = "E family size", 
     ylab = paste("log pop at gen", gen))


## Forest Fire Model
# Forest fire model incorporates spatial
# interactions. Like SIR, we suppose that 
# we have a population made up of susceptible
# (unburnt), infected (on fire), and removed
# (burnt out) individuals.

# The difference is that the individuals are
# placed on a grid and an infected individual
# can only infect a susceptible person if they
# are neighbors.

# At each step an infected individual has
# probability a of infecting each of its eight
# (each corner) susceptible neighbors.

# So for a susceptible individual, the proba-
# bility of remaining uninfected is (1-a)^x
# where x is the number of infected neighbors.

# After having a chance to infect its
# neighbors, an individual is removed with
# probability b.

# If you play around with this you will see
# that there is still a threshold below
# which the fire rarely gets going but above
# which there is a chance that is can grow
# quite fast and large.

# Is a balance between how fast new infections
# appear and how fast infected individuals are
# removed.

# program: spuRs/resources/scripts/forest_fire.r
# forest fire simulation
rm(list = ls())
neighbours <- function(A, i, j) {
  # calculate number of neighbours 
  # of A[i,j] that are infected
  # we have to check for the 
  # edge of the grid
  nbrs <- 0
  # sum across row i - 1
  if (i > 1) {
    if (j > 1) nbrs <- nbrs + (A[i-1, j-1] == 1)
    nbrs <- nbrs + (A[i-1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i-1, j+1] == 1)
  }
  
  # sum across row i
  if (j > 1) nbrs <- nbrs + (A[i, j-1] == 1)
  nbrs <- nbrs + (A[i, j] == 1)
  if (j < ncol(A)) nbrs <- nbrs + (A[i, j+1] == 1)
  
  # sum across row i + 1
  if (i < nrow(A)) {
    if (j > 1) nbrs <- nbrs + (A[i+1, j-1] == 1)
    nbrs <- nbrs + (A[i+1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i+1, j+1] == 1)
  }
  return(nbrs)
}

forest.fire.plot <- function(X) {
  # plot infected and removed individuals
  for (i in 1:nrow(X)) {
    for (j in 1:ncol(X)) {
      if (X[i,j] == 1) points(i, j, col = "red", pch = 19)
      else if (X[i,j] == 0) points(i, j, col = "grey", pch = 19)
    }
  }
}

forest.fire <- function(X, a, b, 
                        pausing = FALSE) {
  # simulate forest fire epidemic model
  # X[i, j] = 2 for susceptible; 
  # 1 for infected; 0 for removed
  # set up plot
  plot(c(1,nrow(X)), c(1,ncol(X)), 
       type = "n", xlab = "", ylab = "")
  forest.fire.plot(X)
  # main loop
  burning <- TRUE
  while (burning) {
    burning <- FALSE
    # check if pausing between updates
    if (pausing) {
      input <- readline("hit any key to continue")
    }
    # update
    B <- X
    for (i in 1:nrow(X)) {
      for (j in 1:ncol(X)) {
        if (X[i, j] == 2) {
          if (runif(1) > (1 - a)^neighbours(X, i, j)) {
            B[i, j] <- 1
          }
        } else if (X[i, j] == 1) {
          burning <- TRUE
          if (runif(1) < b) {
            B[i, j] <- 0
          }
        }
      }
    }
    X <- B
    # plot
    forest.fire.plot(X)
  }
  return(X)
}

# spark
set.seed(3)
X <- matrix(2, 21, 21)
X[11, 11] <- 1
# big fires
#X <- forest.fire(X, .1, .2, TRUE)
X <- forest.fire(X, .2, .4, TRUE)
# medium fires
#X <- forest.fire(X, .07, .2, TRUE)
#X <- forest.fire(X, .1, .4, TRUE)
# small fires
#X <- forest.fire(X, .05, .2, TRUE)
#X <- forest.fire(X, .07, .4, TRUE)
    
X <- matrix(2, 21, 21)
X[21,] <- 1
