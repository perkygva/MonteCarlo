##############################################
#####   RANDOM VARIABLE EXERCISE 2       #####
##############################################

# Define these R functions:

Pois1<-function(s0,lam0){
  spread=3*sqrt(lam0)
  t=round(seq(max(0,lam0-spread),lam0+spread,1))
  prob=ppois(t,lam0)
  X=rep(0,s0)
  for (i in 1:s0){
    u=runif(1)
    X[i]=max(t[1],0)+sum(prob<u)-1
  }
  return(X)
}

# and

Pois2<-function(s0,lam0){
  X=rep(0,s0)
  for (i in 1:s0){
    sum=0;k=1
    sum=sum+rexp(1,lam0)
    while (sum<1){ sum=sum+rexp(1,lam0);k=k+1}
    X[i]=k
  }
  return(X)
}

# and then run the commands

nsim=10000
lambda=3.4
system.time(Pois1(nsim,lambda))
#  user system elapsed
# 0.010  0.000   0.010
system.time(Pois2(nsim,lambda))
# user system elapsed
# 0.27   0.00    0.26
system.time(rpois(nsim,lambda))
# user system elapsed
#    0      0       0

# for other values of nsim and lambda 
# You will see that rpois is by far the
# best.