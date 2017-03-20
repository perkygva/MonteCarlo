########################################
#####         BOOTSTRAP            #####
########################################

# Bootstrap methods (Efron) are a class of
# nonparametric Monte Carlo methods that
# estimate the distribution of a population
# by resampling.

# The resampling methods use an observed
# sample as a finite population, randomly
# sampling from it over and over to make
# inferences about population characteristics.

# Bootstrap methods are often used when the
# distribution of the target population if
# not specified or otherwise unknown.

# Can estimate properties such as bias and/or
# standard error from a bootstrap

### Example: Bootstrap estimate of 
### standard error

# Efron and Tibshirani note that 50-200
# replicates is enough to estimate standard
# error

library(bootstrap)    #for the law data
# contains average LSAT and GPA for
# 15 law schools. The data is a random
# sample from universe of 82 law schools

# estimate the correlation between LSAT
# and GPA scores, and compute the bootstrap
# estimate of the standard error of the
# sample correlation
law
# random sample of 15
print(cor(law$LSAT, law$GPA))
# all 82 law schools
print(cor(law82$LSAT, law82$GPA))

#set up the bootstrap
B <- 200            #number of replicates
n <- nrow(law)      #sample size of 15
R <- numeric(B)     #200-cell storage for replicates

#bootstrap estimate of standard error of R
#we do this 200 times
for (b in 1:B) {
  #randomly select the indices w/ replacement
  i <- sample(1:n, size = n, replace = TRUE)
  # i is vector of those indices
  LSAT <- law$LSAT[i]
  GPA <- law$GPA[i]
  R[b] <- cor(LSAT, GPA) # 200 correlations of 15
  # R contains correlations between 0 and 1
}
#output
print(se.R <- sd(R)) # we get 0.1429
hist(R, prob = TRUE)

# normal theory estimate for standard
# error of R is 0.115

### Example: Bootstrap estimate of 
### standard error: boot function

# develop a function to compute
# correlation between two columns
r <- function(x, i) {
  #want correlation of columns 1 and 2
  cor(x[i,1], x[i,2])
}

# we save the results obj and print
library(boot)       #for boot function
obj <- boot(data = law, 
            statistic = r, 
            R = 2000)
obj
str(obj)
?boot
# bootstrap estimate of standard error
# is 0.13
y <- obj$t
sd(y)

# same estimate

### Example: Bootstrap estimate of bias

#sample estimate for n=15
#test statistic for bias
theta.hat <- cor(law$LSAT, law$GPA)
theta.hat

#bootstrap estimate of bias
B <- 2000   #larger for estimating bias
n <- nrow(law)
theta.b <- numeric(B)

for (b in 1:B) {
  i <- sample(1:n, 
              size = n, 
              replace = TRUE)
  LSAT <- law$LSAT[i]
  GPA <- law$GPA[i]
  theta.b[b] <- cor(LSAT, GPA)
}
bias <- mean(theta.b - theta.hat)
bias
