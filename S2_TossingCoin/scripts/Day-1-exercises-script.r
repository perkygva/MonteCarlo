# QUESTION 1

x <- 123
a <- 1.1
b <- 1.2

# a
(z <- x^(a^b))
# is equivalent to:
(z <- x^a^b)

# b
(z <- (x^a)^b)

# c
(z <- 3*x^3 + 2*x^2 + 6*x + 1)
# is equivalent to:
(z <- (3*x + 2)*(x^2 + 2) - 3)

# d
(z <- z + 1)

# QUESTION 2

# a
c(1:8, 7:1)

# b
rep(1:5, 1:5)

# c
matrix(1, 3, 3) - diag(3)

# d
matrix(c(0,0,7, 2,5,0, 3,0,0), 3, 3)

# QUESTION 3

x <- 1:100
idx <- (x %% 2 != 0) & (x %% 3 != 0) & (x %% 7 != 0)
x[idx]

# QUESTION 4

(queue <- c("S", "R", "A", "L"))
# a
(queue <- c(queue, "B"))
# b
(queue <- queue[-1])
# c
(queue <- c("P", queue))
# d
(queue <- queue[1:(length(queue)-1)])
# e
(queue <- queue[queue != "A"])
# f
which(queue == "R")

