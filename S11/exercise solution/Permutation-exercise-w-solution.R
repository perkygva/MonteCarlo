############################################
#####    PERMUTATION TEST EXERCISE     #####
#####         WITH A SOLUTION          #####    
############################################

# Implement the bivariate Spearman rank 
# correlation test for independence as a 
# permutation test. The Spearman rank 
# correlation test statistic can be obtained 
# using the R function cor() with method = 
# "spearman". Compare the achieved significance 
# level of the permutation test with the p-value 
# reported by cor.test() on the two different 
# samples of data that you generate as per these 
# instructions:

# Use the mvrnorm() function to generate two 
# correlated pairs of samples to test. In the 
# first example, the two samples are bivariate 
# normal; in the second, lognormal. The p-values 
# for cor.test() and spear.man() (a function 
# you create yourself) should be approximately 
# equal in both cases:
  

#--------------------------------------------
# write function for the permutation reps:
spear.perm <- function (x,y){ # input vectors
  stest <- cor.test(x, y, method="spearman")
  n <- length(x)
  rs <- replicate(R, expr = {
    k <- sample(1:n)
    cor.test(x, y[k], method = "spearman")$estimate
  })
  rs1 <- c(stest$estimate, rs)
  pval <- mean(as.integer(stest$estimate <= rs1))
  return(list(rho.s = stest$estimate, 
              p.value = pval))
}

library(MASS)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 30
R <- 499

# samples are bivariate normal:
x <- mvrnorm(n, mu, Sigma)

# exact estimate:
cor.test(x[,1], x[,2], method = 'spearman')

# estimate from your simulated function:
spear.perm(x[,1], x[,2])

# samples are lognormal
x <- exp(mvrnorm(n, mu, Sigma))

# exact estimate
cor.test(x[,1], x[,2], method = 'spearman')

# estimate from your simulated function:
spear.perm(x[,1], x[,2])

# p-values for both tests are both
# significant and close in value