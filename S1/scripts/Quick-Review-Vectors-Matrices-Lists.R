####################################################
###    QUICK REVIEW: VECTORS, MATRICES, LISTS    ###
####################################################

##########     VECTORS

## Creating a Vector

# Vectors can be created in several ways. We have already seen two methods,
# the combine function c and the colon operator ':' for creating vectors
# with given values. For example, to create a vector of integers 1 through
# 9 and assign it to i or j, we can use either of the following.

i = c(1,2,3,4,5,6,7,8,9)
j = 1:9

class(i)
class(j)
mode(i)
mode(j)

# Note i is a numeric class whereas j is an integer class
# But the mode of both i and j is numeric.

# To create a vector without specifying any of its elements, we only need to
# specify the type of vector and its length. For example,

y = numeric(10)
y

# creates a numeric vector y of length 10, and

a = character(4)
a

## Sequences

# There are several functions in R that generate sequences of special types and
# patterns. In addition to ':', there is a sequence function seq() and a repeat
# rep() function. We often use these functions to create vectors.

# The seq() function generates 'regular' sequences that are not necessarily
# integers. The basic syntax is seq(from, to, by), where by is the size of
# the increment. Instead of by, we could specify the length of the sequence.
# To create a sequence of numbers 0,0.1,0.2, . . . ,1, which has 11 regularly
# spaced elements, either command below works.

seq(0, 1, .1)

# [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

seq(0, 1, length=11)

# [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

# The rep function generates vectors by repeating a given pattern a given
# number of times. The rep function is easily explained by the results of
# a few examples:

rep(1, 5)

# [1] 1 1 1 1 1

rep(1:2, 5)

# [1] 1 2 1 2 1 2 1 2 1 2

rep(1:3, times=1:3)

# [1] 1 2 2 3 3 3

rep(1:2, each=2)

# [1] 1 1 2 2

rep(c("a", "b", "c"), 2)

# [1] "a" "b" "c" "a" "b" "c"

## Extracting and Replacing Elements of Vectors

# If x is a vector, x[i] is the ith element of x. 
# This syntax is used both for assignment and extracting values. 
# If i happens to be a vector of positive integers (i1, . . . , ik), 
# then x[i] is the vector containing xi1, . . . , xik, provided
# these are valid indices for x.

# A few examples to illustrate extracting elements from a vector are:
  
x = letters[1:8] # letters of the alphabet a to h
x

x[4] # fourth element

# [1] "d"

x[4:5] # elements 4 to 5

# [1] "d" "e"

i = c(1, 5, 7)

x[i] #elements 1, 5, 7

# [1] "a" "e" "g"

# Examples illustrating assignment are:

x = seq(0, 20, 2)
x[4] = NA #assigns a missing value
x
# [1] 0 2 4 NA 8 10 12 14 16 18 20

x[4:5] = c(6, 7) #assigns two values
x
# [1] 0 2 4 6 7 10 12 14 16 18 20

i = c(3, 5, 7)
x[i] = 0 #assigns 3 zeros, at positions i
x
# [1] 0 2 0 6 0 10 0 14 16 18 20

# Sometimes it is easier to specify what to "leave out" rather than what to
# include. In this case, we can use negative indices. An expression x[-2], 
# for example, is evaluated as all elements of x except the second one. 
# The negative sign can also be used with a vector of indices, 
# as shown below:

x = seq(0, 20, 5)
x
# [1] 0 5 10 15 20

i = c(1, 5)
y = x[-i] #leave out the first and last element
y
# [1] 5 10 15

##########     sort() AND order() FUNCTIONS

# The sort function sorts the values of a vector in ascending order 
# (by default) or descending order. At times, one wants to sort several 
# variables according to the order of one of the variables. This can be 
# accomplished with the order function.

# The order() function: Suppose that we have the following data
# for temperatures and ozone levels:

temps = c(67, 72, 74, 62)
ozone = c(41, 36, 12, 18)

# and wish to sort the pairs (ozone, temps) in increasing order of ozone. 
# The expression order(ozone) is a vector of indices that can be used to 
# rearrange ozone into ascending order. This is the same order required 
# for temps, so this order is what we need as an index vector for temps:

oo = order(ozone)
oo
# [1] 3 4 2 1

Ozone = sort(ozone) #same as ozone[oo]
Temps = temps[oo]

Ozone
# [1] 12 18 36 41

Temps
# [1] 74 62 72 67

# In this example we used sort to sort the values of ozone; however,
# it is not really necessary to sort (again) because order(ozone) contains 
# the information for sorting ozone. In this example, sort(ozone) is 
# equivalent to ozone[oo].

##########     MATRICES

## Creating a Matrix

# A matrix can be created using the matrix function. The basic syntax is
# matrix(x, nrow, ncol), where x is a constant or a vector of values, nrow
# is the number of rows and ncol is the number of columns. For example, to
# create a 4 by 4 matrix of 0's, we use

matrix(0, 4, 4)
#     [,1] [,2] [,3] [,4]
# [1,]   0    0    0    0
# [2,]   0    0    0    0
# [3,]   0    0    0    0
# [4,]   0    0    0    0

# To create a matrix with specified elements, supply those elements in a 
# vector as the first argument to matrix.

X = matrix(1:16, 4, 4)
X
#     [,1] [,2] [,3] [,4]
# [1,]   1    5    9   13
# [2,]   2    6   10   14
# [3,]   3    7   11   15
# [4,]   4    8   12   16

# The number of rows, number of columns, and dimension of a matrix that
# is already in the R workspace is returned by the functions nrow, 
# ncol, and dim, respectively. 

nrow(X)
# [1] 4

ncol(X)
# [1] 4

dim(X)
# [1] 4 4

# Notice that when we supply the vector x=1:16 as the entries of the matrix
# X, the matrix was filled in column by column. We can change this pattern
# with the optional argument byrow=TRUE.

matrix(1:16, 4, 4)

A = matrix(1:16, 4, 4, byrow=TRUE)
A

# The row and column labels of the matrix also indicate how to extract
# each row or column from the matrix. To extract all of row 2 we use A[2,].
# To extract all of column 4 we use A[,4]. To extract the element in row 2,
# column 4, we use A[2, 4].

A[2,]

A[,4]

A[2,4]

# A submatrix can be extracted by specifying a vector of row indices
# and/or a vector of column indices. For example, to extract the submatrix
# with the last two rows and columns of A we use

A[3:4, 3:4]

# To omit a few rows or columns we can use negative indices, in the same
# way that we did for vectors. To omit just the third row, we would use:

A[-3, ]

# The rows and columns of matrices can be named using the rownames
# or colnames. For example, we can create names for A as follows.

rownames(A) = letters[1:4]
colnames(A) = c("FR", "SO", "JR", "SR")
A

# Now one can optionally extract elements by name. To extract
# the column labeled "JR" we could use A[, 3] or

A[, "JR"]

## Arithmetic on Matrices

# The basic arithmetic operators (+ - * / ^) on matrices apply the
# operations elementwise, analogous to vectorized operations. This 
# means that if A=(aij) and B = (bij) are matrices with the same 
# dimension, then A*B is a matrix of the products aijbij . Multiplying 
# a matrix A above with itself using the * operator squares every 
# element of the matrix.

A = matrix(1:16, 4, 4, byrow=TRUE)
A

A * A

# The exponentiation operator is also applied to each entry of a 
# matrix. The R expression A^2 is evaluated as the matrix of 
# squared elements a^2(ij), not the
# matrix product AA.

A^2 # not the matrix product

# Matrix multiplication is obtained by the operator %*%. To obtain
# the square of matrix A (using matrix multiplication) we need A %*% A.

A %*% A # the matrix product

# Many of the one-variable functions in R will be applied to individual
# elements of a matrix, also. For example, log(A) returns a matrix with
# the natural logarithm log(aij) as its entries.

log(A)

# The apply function can be used to apply a function to rows or
# columns of a matrix. For example, we obtain the vector of column
# minimums and the vector of column maximums of A by

apply(A, MARGIN=1, FUN="min")

apply(A, MARGIN=2, FUN="max")

# Row means and column means can be computed using apply or by

rowMeans(A)

colMeans(A)

# The sweep function can be used to sweep out a statistic from a
# matrix. For example, to subtract the minimum of the matrix and 
# divide the result by its maximum we can use

m = min(A)
A1 = sweep(A, MARGIN=1:2, STATS=m, FUN="-") #subtract min
M = max(A1)
A2 = sweep(A1, 1:2, M, "/") #divide by max
A2

# Here we specified MARGIN=1:2 indicating all entries of the 
# matrix. The default function is subtraction, so the "-" 
# argument could have been omitted in the first application 
# of sweep. The column mean can be subtracted from each column by

sweep(A, 2, colMeans(A))

##########     LISTS

# One limitation of vectors, matrices, and arrays is that each 
# of these types of objects may only contain one type of data. 
# For example, a vector may contain all numeric data or all 
# character data. A list is a special type of object that can 
# contain data of multiple types.

# Objects in a list can have names; if they do, they can be 
# referenced using the $ operator. Objects can also be referenced 
# using double square brackets [[ ]] by name or by position.

# EXAMPLE: Creating a List. 
# Horsekicks.R

# This data set appears in several books; see e.g.
# Larsen and Marx [30, p. 287]. In the late 19th 
# century, Prussian officers collected data on deaths 
# of soldiers in 10 calvary corps recording fatalities
# due to horsekicks over a 20 year period. The 200 
# values are summarized in the table below:
  
#                   CORPS-YEARS IN WHICH
# NUMBER DEATHS (k)   k DEATHS OCCURRED
#                0                  109
#                1                   65
#                2                   22
#                3                    3
#                4                    1
#                            TOTAL  200

# To enter this data, we use the combine function c():

k <- c(0, 1, 2, 3, 4)
x <- c(109, 65, 22, 3, 1)

# We can plot with a bar plot using optional argument
# names.arg which assigns labels for the bars

barplot(x, names.arg=k, main = "Frequency Distribution for Prussian Horsekick Data")

# We can compute the relative frequency distribution
# of the observed data in x. The sample proportion of
# 1's is 65/20= 0.545. Or more generally, the expression
# x/sum(x) devides every element of the vector x
# by the sum of the vector (200).

# This creates a vector the same length of x containing
# the sample proportions of the death counts 0 to 4

p <- x / sum(x)
p

# We can also determine the center of this distribution
# by its sample mean which is

r <- sum(p * k)
r

# And an estimate of the variance:
v <- sum(x * (k - r)^2) / 199
v

# The evidently equal sample mean and variance suggest that
# a Poisson counting distribution (as opposed to a binomial,
# geometric, negative binomial, etc.) might fit the data. So
# we compute a Poisson density function:
f <- dpois(k, r)
f

How well does the Poisson model fit the horsekick data? 
In a sample of size 200, the expected counts are 200f(k). 
Truncating the fraction using floor() we have

floor(200*f) # expected counts

x # observed counts

# for k =0,1,2,3,4, respectively. The expected and observed 
# counts are in close agreement, so the Poisson model appears 
# to be a good one for this data. One can alternately compare 
# the Poisson probabilities (stored in vector f) with the sample 
# proportions (stored in vector p). To summarize our comparison
# of the probabilities in a matrix we can use rbind or cbind. 
# Both functions bind vectors together to form matrices; with 
# rbind the vectors become rows, and with cbind the vectors 
# become columns. Here we use cbind to construct a matrix with 
# columns k, p, and f.

cbind(k, p, f)

# It appears that the observed proportions p are very close 
# to the Poisson(0.61) probabilities in f.

# We can put all of that in one script, Horsekicks.r
# (supplied in class materials) which we put in C:/Rx folder

# Prussian horsekick data
# Horsekicks.R
k = c(0, 1, 2, 3, 4)
x = c(109, 65, 22, 3, 1)
p = x / sum(x) #relative frequencies
print(p)
r = sum(k * p) #mean
v = sum(x * (k - r)^2) / 199 #variance
print(r)
print(v)
f = dpois(k, r)
print(cbind(k, p, f))

# And call it as a batch file:
source("C:/Rx/horsekicks.R")

# OK, now let's extend this example of Horsekicks.....

# Now suppose that we would like to store the data
# (k and x), sample mean r and sample variance v, 
# all in one object. The data are vectors x and k, 
# both of length 5, but the mean and variance are each
# length 1. This data cannot be combined in a matrix, 
# but it can be combined in a list. This type of list 
# can be created as

mylist <- list(k=k, count=x, mean=r, var=v)

# The contents of the list can be displayed like any other 
# object, by typing the name of the object or by using print()

mylist

# Names of list components can be displayed using names():

names(mylist)

# All of the components of this list have names, so they can
# be referenced by either name or position. Examples follow.

mylist$count # by name

mylist[[2]] # by position

mylist["mean"] # by name

# A compact way to describe the contents of a list is to use 
# the str function. It is a general way to describe any object 
# (str is an abbreviation for structure).

str(mylist)

# The str function is particularly convenient when the list is 
# too large to read on one screen or the object is a large data 
# set. Many functions in R return values that are lists. An 
# interesting example is the histogram function, which displays 
# a histogram; its return value (a list) is
# discussed in the next example.

# EXAMPLE: Old Faithful. One of the many data sets included
# in the R installation is faithful. This data records waiting 
# time between eruptions and the duration of the eruption for 
# the Old Faithful geyser in Yellowstone National Park, Wyoming, 
# USA. We are interested in the second variable waiting, the 
# waiting time in minutes to the next eruption.

# We construct a frequency histogram of the times between eruptions
# using the hist() function. The heights of the bars correspond to
# the counts for each of the bins. The histogram has an interesting shape.

H <- hist(faithful$waiting)

# Is not normal, appears bimodal.

# Usually only want to see the graphical output of hist().
# But note other info stored in returned object H

str(H)

# Object H is a list

names(H)

# The endpoints of the intervals (the breaks) are

H$breaks

# The frequencies of each interval (the bin counts) are

H$counts

########## SAMPLING FROM A DATA FRAME

# To draw a random sample of observations from a data frame, 
# use the sample function to sample the row indices or the 
# row labels, and extract these rows.

# Refer to the USArrests data. To obtain a random sample 
# of five states

i <- sample(1:50, size=5, replace=FALSE)
i

USArrests[i, ]

# alternatively:

USArrests[sample(1:50, size=5, replace=FALSE), ]

# Alternately, we could have sampled the row labels:

samplerows <- sample(rownames(USArrests), size=5, replace=FALSE)
samplerows

USArrests[samplerows, ]

# alternatively:

USArrests[sample(rownames(USArrests), size=5, replace=FALSE), ]