#########################################
#####       PERMUTATION TESTS       #####
#########################################

# Permutation tests are based on resampling
# without replacement.

# Saw one use with the streakiness baseball
# example.

# Are often applied as non-parametric test
# of general hypothesis:  H0: F = G
# versus alt hyp H1: F not equal to G,
# where F and G are two unspecified distri-
# butions.

# Under null hypothese, two samples from F
# and G, and the pooled sample, are all random
# samples from the same distribution F.

# Replicates of a two-sample test statistic
# that compares the distributions are generated
# by resampling without replacement from the
# pooled sample.

# Can do tests of independence, association,
# location, common scale, etc.
    
### Example: Permutation distribution 
### of a statistic

help(chickwts)

# newly hatched chicks are randomly allocated
# into six groups, each group given a different
# feed supplement. We look at their weight in
# grams after six weeks along with feed types:

require(stats); require(graphics)
# this plot from documentation:
# 'notches' indicate sig differences
boxplot(weight ~ feed, data = chickwts, 
        col = "lightgray",
        varwidth = TRUE, 
        notch = TRUE, 
        main = "chickwt data",
        ylab = "Weight at six weeks (gm)")
# suggests is a significant difference by feed:
anova(fm1 <- lm(weight ~ feed, 
                data = chickwts))
?par
opar <- par(mfrow = c(2, 2), 
            # adjust outer margins
            oma = c(0, 0, 1.1, 0),
            # adjusts lines in margins
            mar = c(4.1, 4.1, 2.1, 1.1))
# plots look ok:
plot(fm1) # residual plots for anova
# reset graphical parameters back:
par(opar)

str(chickwts)
attach(chickwts)

# regular boxplot
boxplot(formula(chickwts))

# We are interested in the differences
# in weight resulting from soybean
# and linseed groups:

# get sorted weights for soybeen-eaters:
x <- sort(as.vector(weight[feed == "soybean"]))
x;length(x)
# get sorted weights for linseed-eaters:
y <- sort(as.vector(weight[feed == "linseed"]))
y;length(y)

# vectors are different lengths

detach(chickwts)

# question is: Are means different?

mean(x) # can observe it is 246.43
mean(y) # can observe it is 218.75

# Could do a two-sample t-test but
# in this case, the population distri-
# butions for the weights are unknown.

# The significance level can be computed
# from the permutation distribution without
# knowing distributional assumptions

# sample sizes are n=14 and m=12 so
# there are a total of (n+m)!/(n!)(m!)
# 9,657,700 different partitions of the
# pooled sample into two subsets of
# sizes 14 and 12. We can simulate
# without replicating 9M.

R <- 999            # number of replicates
z <- c(x, y)        # pooled sample
z
K <- 1:26           # sequence 1 to 26
reps <- numeric(R)  # storage for replicates
reps # vector of 999 zeros
?t.test
t0 <- t.test(x, y)$statistic
t0 # t-stat on original sample is 1.325

for (i in 1:R) {
    # generate 14 indices k for first sample
    k <- sample(K, size = 14, replace = FALSE)
    x1 <- z[k]
    # y gets the remaining elements of z
    y1 <- z[-k]      # complement of x1
    # reps will hold 999 different t-stats
    reps[i] <- t.test(x1, y1)$statistic
    }
# how many of those are greater than the
# t-stat from the original two sets?:
p <- mean(c(t0, reps) >= t0)
p # prop replicates as large as test stat

# for two-tailed test (p=.176) cannot
# reject the null hypothesis that means
# are not different.
?hist
hist(reps, main = "", 
     freq = FALSE, 
     xlab = "T (p = 0.176)") # two-tailed
points(t0, 0, cex = 1, pch = 16) # observed T


### Example: Permutation distribution 
### of the K-S statistic   

# What if we wanted to test for any type of
# distributional difference in the two groups. 

# Could use Kolmogorov-Smirnov statistic D
# is the maximum absolute difference between
# the ecdf's of the two samples

?ks.test

example(ks.test)

attach(chickwts)

# We are interested in the differences
# in weight resulting from soybean
# and linseed groups:

# get sorted weights for soybeen-eaters:
x <- sort(as.vector(weight[feed == "soybean"]))
x;length(x)
# get sorted weights for linseed-eaters:
y <- sort(as.vector(weight[feed == "linseed"]))
y;length(y)

# vectors are different lengths

detach(chickwts)

# continues previous example
# note similarity in syntax, only changing
# the statistic-checking function
R <- 999             # number of replicates
z <- c(x, y)         # pooled sample
K <- 1:26
D <- numeric(R)      # storage for replicates
options(warn = -1) # warn of ties, should have none
D0 <- ks.test(x, y, exact = FALSE)$statistic
for (i in 1:R) {
    # generate indices k for the first sample
    k <- sample(K, size = 14, replace = FALSE)
    x1 <- z[k]
    y1 <- z[-k]      # complement of x1
    D[i] <- ks.test(x1, y1, exact = FALSE)$statistic
    }
p <- mean(c(D0, D) >= D0)
options(warn = 0)
p # does not appear to be significant differences
  # in the ecdfs (i.e. distributions)

# can draw a histogram
hist(D, main = "", 
     freq = FALSE, 
     xlab = "D (p = 0.46)",
    breaks = "scott")
points(D0, 0, cex = 1, pch = 16)  # observed D


### Previous example continued: Here we are
### checking for differences in chick weights
### for the sunflower and linseed groups

attach(chickwts)
x <- sort(as.vector(weight[feed == "sunflower"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
# take a look at distributional summaries
summary(cbind(x, y))
options(warn = -1) # should not have ties
D0 <- ks.test(x, y, exact = FALSE)$statistic
for (i in 1:R) {
    #generate indices k for the first sample
    k <- sample(K, size = 14, 
                replace = FALSE)
    x1 <- z[k]
    y1 <- z[-k]      #complement of x1
    D[i] <- ks.test(x1, y1, 
                    exact = FALSE)$statistic
    }
p <- mean(c(D0, D) >= D0)
options(warn = 0)
p

# in this case (sunflower and linseed), they do seem
# to be significantly different which is consistent
# with out notched boxed plots