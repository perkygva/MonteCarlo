############################################################
#####             LATE  TO  CLASS  EXERCISE            #####
############################################################

# Suppose the travel times for a particular student from 
# home to school are normally distributed with mean 20 
# minutes and standard deviation 4 minutes. Each day 
# during a five-day school week she leaves home 30 minutes 
# before class. For each of the following problems, write 
# a short Monte Carlo simulation function to compute the 
# probability or expectation of interest.

# 1) Find the expected total traveling time of the student 
# to school for a five-day week. Find the simulation 
# estimate and give the standard error for the simulation 
# estimate.

mc.sim1 = function(){
  mu = 20; sigma = 4
  sum(rnorm(5, mu, sigma))
}

y = replicate(1000, mc.sim1())

estimate = mean(y)
se.estimate = sd(y) / sqrt(1000)

# 2) Find the probability that the student is late for 
# at least one class in the five-day week. Find the 
# simulation estimate of the probability and the 
# corresponding standard error.

mc.sim2 = function(){
  mu = 20; sigma = 4
  times = rnorm(5, mu, sigma)
  ifelse(all(times <= 30), "on.time", "late")
}

y = replicate(1000, mc.sim2())

phat = mean(y == "late")
se.phat = sqrt(phat * (1 - phat) / 1000)

# On average, what will be the longest travel time 
# to school during the five-day week? Again find the 
# simulation estimate and the standard error.


mc.sim3 = function(){
  mu = 20; sigma = 4
  times = rnorm(5, mu, sigma)
  max(times)
}

y = replicate(1000, mc.sim3())

estimate = mean(y)
se.estimate = sd(y) / sqrt(1000)

