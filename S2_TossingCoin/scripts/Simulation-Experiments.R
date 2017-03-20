##################################################
######         SIMULATION EXPERIMENTS       ######
##################################################

# Simulation provides a straightforward way of 
# approximating probabilities.One simulates a 
# particular random experiment a large number 
# of times, and a probability of an outcome is 
# approximated by the relative frequency of the
# outcome in the repeated experiments. 

# The use of simulation experiments to better
# understand probability patterns is called the 
# Monte Carlo method. We focus on two particular R 
# functions that simplify the process of programming
# simulation experiments. The sample function will 
# take samples with or without replacement from a set, 
# and the replicate function is helpful in repeating
# a particular simulation experiment. 

## Simulating a Game of Chance of coin tossing

# Peter and Paul play a simple game involving repeated 
# tosses of a fair coin. In a given toss, if heads is 
# observed, Peter wins $1 from Paul; otherwise if
# tails is tossed, Peter gives $1 to Paul. If Peter 
# starts with zero dollars, we are interested in his
# fortune as the game is played for 50 tosses.

# We can simulate this game using the R sample() function. 
# Peter's winning on a particular toss will be $1 or ???$1 
# with equal probability. His winnings on 50 repeated
# tosses can be considered to be a sample of size 50 
# selected with replacement from the set {$1, ???$1}.

options(width=60)
sample(c(-1, 1), size=50, replace=TRUE)

#  [1] -1 -1  1 -1  1  1 -1  1 -1  1  1  1 -1  1 -1  1 -1  1
# [19] -1  1 -1 -1  1  1 -1  1 -1  1 -1 -1  1  1  1  1  1 -1
# [37]  1 -1 -1  1 -1  1 -1 -1  1  1  1 -1 -1  1

# In the outcome represented above, Peter lost the first two
# tosses, won the third, lost the fourth, and so on....

## Exploring Cumulative Winnings

# Suppose Peter is interested in his cumulative winnings as 
# he plays this game. We store his individual toss winnings 
# in the variable win. The function cumsum will compute the 
# cumulative winnings of the individual values and the
# cumulative values are stored in cum.win.

# just adds the individual wins/losses up:

win = sample(c(-1, 1), size=50, replace=TRUE); win
cum.win = cumsum(win)
cum.win

# We extend this and plot the sequence of cumulative
# winnings for 4 games:

par(mfrow=c(2, 2))
for(j in 1:4){
  win = sample(c(-1, 1), size=50, replace=TRUE)
  plot(cumsum(win), type="l" ,ylim=c(-15, 15))
  abline(h=0)}

# Is evidently much variability. How do we address:
# 1) What is prob of Peter breaking even after 50 games?
# 2) What likely number of tosses Peter will be winning?
# 3) What will be value of Peter's best fortune?

# First question can be calculated exactly, can only
# approximate 2) and 3) using Monte Carlo experiment.

## R Function to Implement a Monte Carlo Experiment

# One can obtain approximate answers to these questions 
# by a Monte Carlo experiment. In this type of experiment, 
# one simulates the random process & computes the statistic 
# or statistics of interest. By repeating the random
# process many times, one obtains a collection of the 
# statistics. One then uses the collection to approximate 
# probabilities or expectations that answer the questions.

# Let's first consider Peter's fortune F at the end of 
# the game. We write a function peter.paul that simulates 
# the fortunes for the 50 tosses and computes F that is 
# equal to the sum of the individual fortunes. To make this
# function more general, we define n to be the number of 
# tosses and let the default value of n be 50.

peter.paul=function(n=50){
  win = sample(c(-1, 1), size=n, replace=TRUE)
  sum(win)
}

peter.paul()

# In this game, Peter finished with a fortune of ?? dollars. 
# To repeat this for 1000 games, we use the replicate function 
# with arguments 1000, the number of games, and the name of
# the function peter.paul() to repeat. The output of replicate 
# is the vector of fortunes for the 1000 games that we assign
# to the variable F.

F = replicate(1000, peter.paul())

## Summarizing the Monte Carlo Results

# Since Peter's fortune is integer-valued, a convenient way 
# of summarizing the collection of values of F using table().

table(F)

# We can display the frequencies of F using plot() applied 
# to the table output.

par(mfrow=c(1, 1))
plot(table(F))

# Is more or less symmetric centered about 0. Why don't any
# games end with an "odd" fortune?

# What is Peter's chance of breaking even? It is the ratio 
# (approximated) of Peter finishing with 0 out of the 1000

# But that answer can also be determined exactly. He breaks
# even if there are exactly n/2 heads in a binomial
# experiment of n trials with a probability of success
# equal to 0.50. So if n = 50

dbinom(25, size=50, prob=0.5) # equals 0.1122752

# Note how close this exact number is to our approximated
# answer from the Monte Carlo simulation

## Modiying the Experiment to Learn About
## New Statistics

# We can add additional lines of code to our function 
# peter.paul to compute several statistics of interest 
# in our experiment. To answer our questions, we focus
# on the final fortune F, the number of times Peter is 
# in the lead L, and the maximum cumulative winning M. 

# In the function, we define the vector of cumulative 
# winnings cum.win. Here the output of the function is 
# a vector consisting of the values of F, L, and M. 

# By naming the components (using, for example, 
# F=sum(win)), we get more attractive output.

peter.paul=function(n=50){
  win=sample(c(-1, 1), size=n, replace=TRUE)
  cum.win = cumsum(win)
  c(F=sum(win), L=sum(cum.win > 0), M=max(cum.win))
}

# We simulate the game once

peter.paul()

# F   L   M
# 2  41   5

# In this game, Peter's final fortune was $2, he was 
# in the lead for 41 plays, and his maximum total winning 
# during the game was $5. To repeat this game 1000 times,
# we again use replicate and store output in variable S.

# Since the output of peter.paul is a vector, S will be a 
# matrix of 3 rows and 1000 columns, where the rows correspond 
# to the simulated draws of F, L, and M. We can verify the 
# dimension of the matrix of S using the dim() function.

S = replicate(1000, peter.paul())
dim(S)

# How many times Peter in lead? The likely answer to this
# question (we do not know for sure) is in the "L" row:

times.in.lead = S["L", ]

# We tabulate the simulated values using the table function, 
# and the prop.table function will find the corresponding 
# relative frequencies. We plot the result in a line graph:

plot(prop.table(table(times.in.lead)))

# The pattern of this graph for L isn't what most people 
# expect. Since it is a fair game, one might think it would 
# be most likely for Peter to be ahead in 25 out of 50 tosses. 

# Instead, the most likely values are the extreme
# values L = 0,1,50, and the remaining values of L appear 
# equally likely. So actually it is relatively common for 
# Peter to always be losing or always winning in this game.

# Last, let's consider the distribution of M, Peter's
# maximum winning during the 50 plays. We store the 1000
# simulated values of M in the variable maximum.lead, 
# tablulate the values using table(), and then plot them

maximum.lead = S["M", ]
plot(table(maximum.lead))

# From the plot, we can see that it is most likely for Peter
# to have maximum winnings of ?, but values between ? and ?
# are all relatively likely.

# To compute the approximate probability that Peter will
# have a maximum winning of 10 or more, we find the number
# of values of maximum.lead that are 10 or greater and
# divide this sum by the number of simulation iterations
# 1000:

sum(maximum.lead >= 10) / 1000

# Since this probability is only about ?? (16%), it is
# relatively rare for Peter to have a maximum winning
# of 10 or more.

## Random Permutations: The Hat Problem

# A man wears a top hat to a restaurant, checks his hat
# at the door. Suppose n men check their hats and hats
# are returned in a random fashion. How many men will
# receive their own hats?

## Using sample() to Simulate an Experiment

# n = 10 men. Store their hats in vector hats. 
# Use sample() function to permute randomly, store in
# vector mixed.hats

n = 10
hats = 1:n

mixed.hats = sample(hats)

hats
# [1] 1 2 3  4 5 6 7 8 9 10

mixed.hats
# [1] 4 6 3 10 1 8 7 2 5  9

# In this simulation, the 3rd and 7th hats were returned
# to their correct owners.

## Comparing Two Permutations of a Sample

# But we want to be able to manipulate the output so we
# can compute number of correct hats returned.

# We use the logical expression to test for equality
# between the elements in the two vectors
hats == mixed.hats

# [1] FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE

# 'TRUE' corresponds to a match, 'FALSE' does not. Summing
# the 'TRUEs' (note that TRUE = 1) gives us number of matches.

correct = sum(hats == mixed.hats)
correct

# We want to write a function to perform this mixed-hats
# simulation. We call the function scramble.hats() and it
# has a single argument n, the number of hats.

## Writing a Function to Perform Simulation

scramble.hats = function(n){
  hats = 1:n
  mixed.hats = sample(n)
  sum(hats == mixed.hats)
}

# The function returns the number of correct matches
# in each (separate) simulation.

scramble.hats(30)

# [1] 0

# Note when we increased the number of hats to 30, we ended
# up with no correct matches.....with 10 hats we had 2 matches.

## Repeating the Simulation

# Let X-sub-n denote the random variable defined as the number
# of correct matches for given number of hats n. To obtain
# probability distribution of X-sub-n, we can simulate this
# experiment a large number of time and tabulate results.

# Here it is 1000 times with ten hats:
matches = replicate(1000, scramble.hats(10))

# Frequency table of correct matches with 10 hats:
table(matches)

# Dividing table frequencies by 1000 yields relative freqs:
table(matches) / 1000

# We see it is most likely to get 0 or 1 matches.

# We find the expected value of X-sub-n:
mean(matches)

# [1] 0.987

# So, on average, 0.0987 of the ten men will receive his 
# correct hat. It can be shown that theoretical mean is
# E(X-sub-n) = 1 with n = 10.

# Now we want to see how the probability of no (0) correct
# matches, P(X-sub-n = 0), changes as a function of the
# number of men n.

# We write a new function, prop.no.matches() that has
# single argument n. The function simulates this experiment
# (with n men) 10,000 times and finds the proportion of
# experiments where no men receive the correct hats:

prop.no.matches = function(n){
  matches = replicate(10000, scramble.hats(n))
  sum(matches == 0) / 10000
}

# We test this function for n = 20 hats
prop.no.matches(20)

# [1] 0.3724 is the chance that no men received correct hats.

# Now we use sapply() to apply the function to a vector
# of n men (or hats) to compute the relative probabilities
# from 2 to 20 hats.
many.probs = sapply(2:20, prop.no.matches)

# We plot the estimated probabilities as a function of n
# and add a horizontal line at 0.357
plot(2:20, many.probs,
     xlab="Number of Men", ylab="Prob(no matches)")
abline(h=0.3724)

# We see that probability of no matches is large at n = 2,
# small at n = 3, and then appears to stabilize about the 
# value of 0.37 as n becomes larger.

## The Collector's Problem: Baseball Cards

# You collect baseball cards (like 'Topps'). They are sold in
# packs of ten that are characterized by overlapping sets so
# you had motivation to keep buying packs to 'find' a complete
# set of players.

# Let's simulate this....let's say we want a complete set
# of cards of ten great baseball players that we represent
# with the following character vector:

cards = c("Mantle", "Aaron", "Gehrig", "Ruth", "Schmidt",
          "Mays", "Cobb", "DiMaggio", "Williams", "Foxx")

## Simulating Experiment using sample() function

# Assume each purchased card is equally likely to turn up one
# of the ten players above. Assuming independent draws, a 
# purchase of 20 cards can be considered a random sample taken
# with replacement from this vector.

samp.cards = sample(cards, size=20, replace=TRUE)
samp.cards

# We get duplicates, of course, but did we get a complete set?

# The unique() function return unique values
unique(samp.cards)

# We do it twice, first time we get 8, second time 9.
length(unique(samp.cards))

## Writing a Function to Perform the Simulation

# Let's write a function to perform this simulation.
# Suppose a complete set consists of n cards, we purchase
# m cards, and we want to know the probability that we
# purchase a complete set.

# But let's say that a complete set consists of 586 cards,
# not 10 players. So, say, if we purchased 3000 cards, what
# would be the chance that we get a complete set?

# We write function collector() to simulate this process. The
# complete set is represented by the vector consisting of the 
# integers 1 through n. We use sample() to take the sample
# of m cards. We use ifelse() to check if we have purchased
# a complete set...if the number of unique cards in our
# sample is equal to n, it returns "yes", otherwise "no".

collector=function(n,m){
  samp.cards = sample(1:n, size=m, replace=TRUE)
  ifelse(length(unique(samp.cards)) == n, "yes", "no")
}

collector(586, 3000)

# First time, we get a "no"

# So now we use replicate() to repeat the process for 100
# experiments and we summarize outcomes with table():
table(replicate(100, collector(586, 3000)))

# They are mostly "no's"
# Estimated prob of getting a complete set is remote.

## Buying an Optimal Number of Cards

# We try to make this more interesting.
# What if the person collecting has a strategy of buying
# small sets to improve his chances of getting a complete
# set? Further, let's say packs of cards cost 5 cents each
# but can also buy individual cards from a deal for 25 cents.

# So the card collector will "fill in the holes" at 25 cents
# a pop.

# On average, what is the cost of this plan? Is there an 
# optimal number of cards purchased that will minimize the
# expected cost?

# We write function collect2() to simulate. Is one
# argument, n.purchased, number of cards purchased. We
# assume cost of a "random" card is cost.rcard = 0.05
# and cost of card from dealer is cost.ncard = 0.25.

# We sample n.purchased cards with replacement from the
# complete set represented by integers 1 thru 586.

# We compute number of random unique cards we collect n.cards
# and number we haven't collected n.missed which we buy
# from dealer.

# Therefore, the random total cost will be:

# COST = cost.rcard x n.purchased+cost.ncard x n.missed

collect2 = function(n.purchased){
  cost.rcard = 0.05
  cost.ncard = 0.25
  samp.cards = sample(1:586, size=n.purchased, replace=TRUE)
  n.cards = length(unique(samp.cards))
  n.missed = 586 - n.cards
  n.purchased * cost.rcard + n.missed * cost.ncard
}

# He decides to purchase 800 cards. What is expected cost?

costs = replicate(500, collect2(800))
summary(costs)

# We repeat 500 times and store vector of total costs in 'costs'
# The summary() function shows distribution of random costs.

# > summary(costs)
# Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
# 71.25   76.00   77.50   77.36    78.75   84.25 

# So cost of buying 800 cards has 50% chance of being
# somewhere between $76.00 and $78.75

# Expected cost is sample mean of $77.36

# So we write new function expected.cost() that takes 100
# samples of cards, each of size n.purchased and computes
# the average total cost. Note that replicate() does the
# repeated simulations and mean() finds expected cost 
# from vector of total costs.
expected.cost = function(n.purchased)
  mean(replicate(100, collect2(n.purchased)))

# Since what we want is to see how total cost varies as
# function of number purchased, we define a vector N as
# numbers of cards to buy from 500 to 1500
N=500:1500

# sapply() operates expected.cost() against N.
# We plot expected cost against number of cards
# purchased. Grid is overlaid to help locate minimum.
ECOST = sapply(N, expected.cost)
plot(N, ECOST, xlab="Cards Purchased",
     ylab="Expected Cost in Dollars")
grid(col="black")

# Looks like expected cost is minimized at around 950 cards.
# So optimal strategy is to buy 950 cards at cost of $76.50.

# If we bought 1200 cards, expected cost would be around $79.

## Patterns of Dependence in a Sequence

# Sports fans love athletes who exhibit patterns of extreme
# performance. Records are kept of extreme winning or losing
# patterns, such a getting a base hit in baseball....
# ... a "hitting streak"

# In the 2006 baseball season, Chase Utley of the Philadelphia
# Phillies had a hitting streak of 35 games, one of the best
# in baseball history.

# But how "significant" was this streak. Utley was a good
# hitter anyway, so it might be expected.

# As another example, long runs of heads and tails can be
# observed flipping a coin.

# We investigate with Monte Carlo simulation:

## Writing a Function to Compute Streaks

# We represent hitting success or failure with 0's and 1's

y = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1)

# What are the lengths of the hitting streaks?

# Add 0's to front and end to bracket it
# 'where' is vector of TRUE's (if 0) and FALSE's (if 1)
where = (c(0, y, 0) == 0)
n = length(y)
# loc.zeros records locations in sequence where 0's occur
# at 0  8  9  11  16
loc.zeros = (0:(n+1))[where]
loc.zeros

# So no base hit in games 0, 8, 9, 11 and 16

# We compute length of streaks:
streak.lengths = diff(loc.zeros) - 1
# We take out the zeros:
streak.lengths = streak.lengths[streak.lengths > 0]
streak.lengths

# [1] 7 1 4

# Write function longest.streak by taking the max()
# out of the above vector:
longest.streak=function(y){
  where = c(0, y, 0) == 0
  n = length(y)
  loc.zeros = (0:(n+1))[where]
  streak.lengths = diff(loc.zeros) - 1
  streak.lengths = streak.lengths[streak.lengths > 0]
  max(streak.lengths)
}

# Make sure "utley2006.txt" is in path of default directory
file.exists("utley2006.txt")

# read it in
dat = read.table("utley2006.txt", header=TRUE, sep="\t") # tab sep
head(dat)
# are counting 2's, 3's etc as 1's:
utley = as.numeric(dat$H > 0) # Make sure H is numeric
# apply longest.streak() function to this vector
longest.streak(utley)

# So Utley's longest streak in 2006 was 35 games.

## Writing a Function to Simulate Hitting Data

# We apply Monte Carlo to understand "significance"
# Utley played in 160 games and hit in 116 of them

# What if 116 "hit" games were randomly distributed?
# The what would be length of longest sequence?


# Write function random.streak() with binary sequence y as
# argument
random.streak=function(y){
  # first it randomly permutes y and stores in mixed.up.y
  mixed.up.y = sample(y)
  # then find longest streak of 1's in vector mixed.up.y
  longest.streak(mixed.up.y)
}

# replicate random.streak 100000, store in L
L = replicate(100000, random.streak(utley))
# tabulate values in L and plot
plot(table(L))
# superimpose line for 2006 season:
abline(v=35, lwd=3)
# add text label in plot at 38, 10000
text(38, 10000, "Utley")

# We see long hitting streaks from 8 to 18 are quite common
# With a hitting streak of 11 most likely

# Utley's 35 streak is in far right tail, indicating it is 
# very rare. Can measure "how extreme" by estimating probability
# that a random sequence would be like this (35 or more)

# Probability....only 7 out of 100000 = 0.00007...VERY RARE !!
