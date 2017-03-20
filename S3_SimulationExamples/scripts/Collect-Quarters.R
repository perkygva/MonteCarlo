#######################################################
#####           COLLECTING  STATE  QUARTERS      ######
#######################################################

# In 1999, the United States launched the 50 State Quarters 
# program where each of the 50 states was honored with a 
# special quarter. Suppose you purchase 100 "state" quarters 
# where each quarter is equally likely to feature one of 
# the 50 states.

## Write a function using the sample() function to 
## simulate the purchase of 100 quarters and record 
## the number of unique quarters that are purchased.

collect.quarters=function(n){
  sample.q=sample(1:50, n, replace=TRUE)
  length(unique(sample.q))
}

collect.quarters(100)

## Using the replicate() function, repeat this process 
## for 1000 purchases. Construct a table of the number 
## of unique quarters you obtain in these 1000 simulations. 
## Use this table to estimate the probability that you 
## obtain at least 45 unique quarters.

N = replicate(1000, collect.quarters(100))
table(N)
mean(N >= 45)

## Use the output from 2) to find the expected number 
## of unique quarters.

mean(N)

## Suppose you are able to complete your quarter set 
## by purchasing state quarters from a coin shop for 
## $2 for each quarter. Revise your function to compute 
## the total (random) cost of completing the quarter set. 
## Using the replicate() function, repeat the quarter-
## purchasing process 1000 times and compute the expected 
## cost of completing your set.

collect.quarters2=function(n){
  sample.q=sample(1:50, n, replace=TRUE)
  U = length(unique(sample.q))
  ifelse(U <= 50, 2 * (50 - U), 0)
}

C = replicate(1000, collect.quarters2(100))
mean(C)
