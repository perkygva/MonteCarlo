##################################################
###    CHAPTER 11 - SIMULATION EXPERIMENTS     ###
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
# starts with zero dollars, we are interested in his change
# in fortune as the game is played for 50 tosses.

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
# cumulative winnings of the individual values and the cumulative
# values are stored in cum.win.

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
# 2) What are likely number of tosses where Peter will be winning?
# 3) What will be value of Peter's best fortune?

# First question can be calculated exactly, can only
# approximate 2) and 3) using Monte Carlo experiment.

## R Function to Implement a Monte Carlo Experiment

# One can obtain approximate answers to these questions 
# by a Monte Carlo experiment. In this type of experiment, 
# one simulates the random process and computes the statistic 
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
# is the vector of fortunes for the 1000 games that we assign to
# the variable F.

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
# we again use replicate and store the output in the variable S.

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
# Peter to always be losing or always bewinning in this game.

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

##############################################################
#                                                            #
#       WE WILL STOP HERE ON OCT 1 IF WE GET THIS FAR        #
#                                                            #
##############################################################

## Random Permutations: The Hat Problem

## Using sample() to Simulate an Experiment

n = 10
hats = 1:n

mixed.hats = sample(hats)

hats

mixed.hats

## Comparing Two Permutations of a Sample

hats == mixed.hats

correct = sum(hats == mixed.hats)
correct

correct = sum(hats = mixed.hats)
correct

## Writing a Function to Perform Simulation

scramble.hats = function(n){
  hats = 1:n
  mixed.hats = sample(n)
  sum(hats == mixed.hats)
}

scramble.hats(30)

## Repeating the Simulation

matches = replicate(1000, scramble.hats(10))

table(matches)

table(matches) / 1000

mean(matches)

prop.no.matches = function(n){
  matches = replicate(10000, scramble.hats(n))
  sum(matches == 0) / 10000
}

prop.no.matches(20)

many.probs = sapply(2:20, prop.no.matches)
plot(2:20, many.probs,
     xlab="Number of Men", ylab="Prob(no matches)")
abline(h=0.37)

## The Collector's Problem: Baseball Cards

cards = c("Mantle", "Aaron", "Gehrig", "Ruth", "Schmidt",
          "Mays", "Cobb", "DiMaggio", "Williams", "Foxx")

## Simulating Experiment using sample() function

samp.cards = sample(cards, size=20, replace=TRUE)
samp.cards

unique(samp.cards)

length(unique(samp.cards))


## Writing a Function to Perform the Simulation

collector=function(n,m){
  samp.cards = sample(1:n, size=m, replace=TRUE)
  ifelse(length(unique(samp.cards)) == n, "yes", "no")
}

collector(586, 3000)

table(replicate(100, collector(586, 3000)))

## Buying an Optimal Number of Cards

collect2 = function(n.purchased){
  cost.rcard = 0.05
  cost.ncard = 0.25
  samp.cards = sample(1:586, size=n.purchased, replace=TRUE)
  n.cards = length(unique(samp.cards))
  n.missed = 586 - n.cards
  n.purchased * cost.rcard + n.missed * cost.ncard
}

costs = replicate(500, collect2(800))
summary(costs)

expected.cost = function(n.purchased)
  mean(replicate(100, collect2(n.purchased)))

N=500:1500
ECOST = sapply(N, expected.cost)
plot(N, ECOST, xlab="Cards Purchased",
     ylab="Expected Cost in Dollars")
grid(col="black")

## Patterns of Dependence in a Sequence


## Writing a Function to Compute Streaks

y = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1)

where = (c(0, y, 0) == 0)
n = length(y)
loc.zeros = (0:(n+1))[where]
loc.zeros

streak.lengths = diff(loc.zeros) - 1
streak.lengths = streak.lengths[streak.lengths > 0]
streak.lengths

longest.streak=function(y){
  where = c(0, y, 0) == 0
  n = length(y)
  loc.zeros = (0:(n+1))[where]
  streak.lengths = diff(loc.zeros) - 1
  streak.lengths = streak.lengths[streak.lengths > 0]
  max(streak.lengths)
}

dat = read.table("utley2006.txt", header=TRUE, sep="\t")
utley = as.numeric(dat$H > 0)
longest.streak(utley)

## Writing a Function to Simulate Hitting Data

random.streak=function(y){
  mixed.up.y = sample(y)
  longest.streak(mixed.up.y)
}

L = replicate(100000, random.streak(utley))
plot(table(L))
abline(v=35, lwd=3)
text(38, 10000, "Utley")
