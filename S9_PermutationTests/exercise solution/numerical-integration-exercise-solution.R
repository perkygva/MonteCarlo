########################################################
#####                                              #####
#####   NUMERICAL INTEGRATION EXERCISE SOLUTION    #####
#####                                              #####
########################################################
# Need to have "simpson_n.r" and "newtonraphson.r" 
# in the directory path so can call them with source().
# HERE IS ONE SOLUTION:
getwd()
rm(list=ls())

source("simpson_n.r")
source("newtonraphson.r")

phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))

Phi <- function(z) {
  if (z < 0) {
    return(0.5 - simpson_n(phi, z, 0))
  } else {
    return(0.5 + simpson_n(phi, 0, z))
  }
}

newtonraphson(function(z) c(Phi(z) - 0.5, phi(z)), 0)
newtonraphson(function(z) c(Phi(z) - 0.95, phi(z)), 0)
newtonraphson(function(z) c(Phi(z) - 0.975, phi(z)), 0)
newtonraphson(function(z) c(Phi(z) - 0.99, phi(z)), 0)
