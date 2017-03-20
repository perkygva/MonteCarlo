############################################################
#####             UTLEY  SWITCHES  EXERCISE            #####
############################################################

# MORE  'STREAKINESS'  EXERCISES

# With the baseball example, the focus was on the length 
# of the longest streak in a binary sequence. Another way 
# to measure streakiness in a sequence is the number of 
# switches from 0 to 1 or from 1 to 0. For example, if 
# the binary sequence is given by:
  
#   0  1	0	0	0	1	0	0	1

# Then there are three switches from 0 to 1 and two switches 
# from 1 to 0 and so the total number of switches is equal 
# to five. If y is a vector containing the binary sequence, 
# then the R expression

sum(abs(diff(y)))

# will compute the number of switches.

# 1) Construct a function switches() that computes the 
# number of switches for a binary vector y. Test this 
# function by finding the number of switches in Chase 
# Utley's game hitting sequence for the 2006 season.

switches = function(y){
  sum(abs(diff(y)))
}

utley = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/utley2006.txt",
                   header=TRUE, sep="\t")

utley.y = as.numeric(utley$H > 0)

switches(utley.y)

# 2) By making a slight change to the function random.streak(), 
# construct a function that computes the number of switches 
# for a random permutation of the 1's and 0's in y.

random.streak=function(y){
  mixed.up.y = sample(y)
  switches(mixed.up.y)
}
# 3) Use the replicate() function to repeat the random 
# permutation in 2) above for 10,000 simulations. 
# Construct a histogram of the number of switches for 
# these 10,000 random sequences. Is the number of switches 
# in Utley's sequence consistent with the values generated 
# from these random sequences? Using the number of switches 
# statistic, did Utley display unusually streaky behavior 
# during this season?

s = replicate(10000, random.streak(utley.y))
hist(s)
abline(v = switches(utley.y), lwd=3)

# No, Utley's values are not consistent using the switches 
# statistic. He did not display unusually streaky behavior 
# by this statistic. As visible in the plot below, a large 
# area is to the left of his "switches line".
