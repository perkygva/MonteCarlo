#################################################
#####    CHECKING HATS EXERCISE SOLUTION    #####
#################################################

# Suppose that men in the old days wore only two 
# types of hats, black and grey, and that hats of 
# a particular type are indistinguishable. Assume 
# 20 men with hats visit the restaurant and half 
# of the men are wearing each type of hat. The 
# hats are randomly mixed, and we are interested
# in the number of men who leave the restaurant
# with the correct hat.

# 1) Modify the function scramble.hats() to compute 
# the number of correct matches in this setting. 
# (The only change is the definition of the vector 
# hats; if one represents a black hat and a grey hat 
# using a 1 and 2, respectively, then hats consist 
# of ten 1's and ten 2's.
                                                                                                   
scramble.hats2 = function(){
  hats = c(rep(1, 10), rep(2, 10))
  mixed.hats = sample(hats)
  sum(hats == mixed.hats)
}

# 2) Using the function replicate(), repeat this 
# simulation for 1000 trials. Store the number of 
# matches for the 1000 experiments in the vector 
# matches.

matches = replicate(1000, scramble.hats2())

# 3) From the simulated values, approximate the 
# probability that 10 or more men receive the 
# correct hats. Also, find the expected number 
# of correct matches. Then, plot a histogram of 
# the non-parametric distribution of correct 
# matches.

mean(matches >= 10)
# [1] 0.683  on this simulation, it may vary

mean(matches)
# [1] 10.004 on this simulation, it may vary

hist(matches) # see plot in pdf
