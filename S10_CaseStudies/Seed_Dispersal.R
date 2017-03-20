#####################################################################
######                                                         ######
######      OBJECT ORIENTED PROGRAMMING IN S3 and S4           ######
######                                                         ######
#####################################################################

# OOP, have classes, create and manipulate objects of those classes.
# An object is a variable with a structure, like Boolean, vector, 
# dataframe, or list.

# The class of the object describes the category of the object.

# R supports OOP through so-called 'old-style' (S3) and 'new-style' 
# (S4) classes. A user can define a new class of objects (variables
# with specific structures) and functions that apply to those
# objects. Also, existing functions can be augmented to apply to
# new classes.

# Here we discuss S3 classes, particularly classes, generic functions
# and attributes.

# Class is not the same as mode. Mode is more primitive and refers
# to how R stores the object in memory.

# GENERIC FUNCTIONS

# In R, a generic function is a function that examines the class of
# its first argument, and then chooses another function appropriate
# to that class. For example we look at the mean() function:

mean
# function (x, ...) 
# UseMethod("mean")
# <bytecode: 0x0000000009f15630>
# <environment: namespace:base>

# It merely passes the string 'mean' to a function called UseMethod.
# UseMethod function then calls the version of mean that is designated
# for the class of the object named in the first argument. If the
# class is widget, UseMethod calls the function mean.widget() with
# the same arguments that were supplied by the user.

# Any generic function can be extended by adding a version that acts
# differently depending on the class. Not all functions are generic;
# we can find out if a function is generic using methods function:

methods(mean)

methods(var)

# To write a function that is automatically invoked to replace a
# generic function fu when applied to an object of class bar, all
# we have to do is name the function "fu.bar" ("function.class")

# Note that this is for S3 classes only; S4 classes have a 
# different and more formal approach.

# EXAMPLE: SEED DISPERSAL, PLANT BIOLOGY

# We will create a new class (e.g. a new type of object) and then 
# write some generic functions (for print and mean) that apply to 
# that class.

# In studying the spread of plants (in particular, weeds) it is
# useful to know how their seeds are dispersed. Seed dispersal is
# measured using seed traps, containers of fixed size that are
# situated at a known distance and bearing from a parent plant
# for a set amount of time. The number of seeds landing in each
# container are counted, and the number attributed to the parent
# plant identified.

# We assume the seed traps are laid out in a straight line
# at various distances and anchored at the parent plant. The
# seeds in each trap are then counted after a certain amount of time.

# So our data is: (1) distance of center of trap from plant;
# (2) the trap area; (3) count of seeds in trap after some period

# We will call our class "trapTransect"; A "transect" is a straight 
# line or narrow section through an object or natural feature or 
# across the earth's surface, along which observations are made...
# There is no such class now. So there are no special methods 
# for trapTransect objects. We inquire:

methods(class = "trapTransect") # no methods were found

# We must decide what attributes these objects will have.
# Each object will contain data from a single transect of traps,
# so we want to store (1) the trap distances from parent plant and
# (2) seed counts. Assume trap sizes are constant.

# So basic structure of trapTransect object will be a list with
# 3 compnents: (1) trap distances; (2) trap seed counts; and
# trap size.

# We write a 'constructor' function to create S3 classes in R. The
# 'constructor' function: (1) creates objects of the appropriate
# structure; (2) sets the class attribute; and (3) returns the
# object. We will then write functions that manipulate objects
# of that class. Here is S3 class constructor function:

trapTransect <- function(distances, seed.counts, trap.area = 0.0001) {
  if (length(distances) != length(seed.counts)) stop("lengths differ")
  if (length(trap.area) != 1) stop("Ambig trap area")
  trapTransect<-list(distances=distances,
                     seed.counts=seed.counts,
                     trap.area=trap.area)
  class(trapTransect)<- "trapTransect"
  return(trapTransect)
}

# In interest of brevity, we've omitted checks for missing values,
# inputs that are not numbers, etc although a real solution would
# require these. Also, our function assumes that the trap area is
# 0.0001, although this can be overridden when function is called.
# Also we do not specify units which would be required.

# Now we create a function, print.trapTransect, that prints out
# relevant information about trapTransect data when invoked. It
# uses str function so we have a compact example rather than a
# perfect one !

print.trapTransect <- function(x,...) {
  str(x)
}

# Note we have chosen same arguments (x and ...) as for the generic
# function print.

# Now we write a specific function for the mean that uses the
# structure of the trapTransect object to compute the mean
# dispersal distance from the plant along the transect. We indicate
# that this version of the mean should be used only for trapTransect
# objects by post-fixing '.trapTransect' to the function name:

mean.trapTransect <- function(x,...) {
  return(weighted.mean(x$distances, w=x$seed.counts))
}

# So now we have informed R about the trapTransect class, in that
# it can recognize specific versions of generic methods suitable
# for that class:

methods(class=trapTransect)

# So now we demonstrate the use of the class methods
# print.trapTransect and mean.trapTransect

s1 <- trapTransect(distances=1:4,seed.counts=c(4,3,2,0))
s1

# What are the variables in S1?:
names(s1)

# What does an ASCII text representation of S1 look like?:
dput(s1)

# What is the mean dispersal distance from the plant along transect?:
mean(s1)

# Mean dispersal distance should be (and it is!):
16/9 # (4+ 6 + 6 )/9

# Keep in mind that S1, an object of class trapTransect, is still
# a list. That is, objects of class trapTransect inherit the
# characteristics of objects of class list:
is.list(s1)
typeof(s1)

# But the (S3) class is trapTransect:
class(s1)

# So we can still manipulate s1 using its list struture. We
# examine the first element:
s1[[1]]

# Creating this infrastructure is a bit of work, but the advantages
# are evident when we need to construct, manipulate, and analyze
# models of complex systems. OOP prototyping is easy and rapid
# and allows for adding complexity when it is necessary. For
# example, we could easily add in new generic functions that apply
# to the S3 trapTransect class.

# We might come back to this specific example later

## OBJECT ORIENTED PROGRAMMING WITH S4 OBJECTS

# So now we will briefly cover the infrastructure provided by
# S4 classes. S4 classes provide a formal object-method framework
# for objecy oriented programming. So we now reinvent the seed
# trap example and explain. We assume the same structure of the
# class and trapTransect objects:

# First we tell about the class itself using setClass function
# which takes the proposed class name and the proposed structure
# of the objects of the class as arguments:

setClass("trapTransect", 
         representation(distances = "numeric", 
                        seed.counts="numeric", 
                        trap.area = "numeric"))

# Writing a constructor is a bit more involved than with S3 classes.
# The constructor function is called new, but it we wishh to do any
# processing of the arguments, including validity checks, then
# we need to add a specific initialize fuction, which will be
# called by new

setMethod("initialize",
          "trapTransect",
          function(.Object,
                   distances=numeric(0), 
                   seed.counts =numeric(0), 
                   trap.area = numeric(0))
          {
            if (length(distances) != length(seed.counts)) 
              stop("Lengths of distances and counts differ.")
            if (length(trap.area) != 1) 
              stop("Ambiguous trap area.")
            .Object@distances=distances
            .Object@seed.counts=seed.counts
            .Object@trap.area=trap.area
            .Object
          })

# new creates an empty object and passes it to initialize, along
# with the arguments that were provided to it. initialize then
# returns the updated object, if the evaluations are successful.

s1<-new("trapTransect",
        distances=1:4,
        seed.counts=c(4, 3, 2, 0), 
        trap.area = 0.0001)

# S4 class objects differ from S3 in a few important ways.
# The elements that comprise the object, as defined in the 
# setClass function, are called "slots". The names of the
# slots can be found by:
slotNames(s1)

# Notice that names are NULL with S4
names(s1)

# What does an ASCII text representation of S1 look like?:
dput(s1)

# Values in the slots are accessed by either the slot function
# or the "@" operator, which replaces the $ operator:
s1@distances

# We now add two methods for the class: (1) show, to print objects
# of the class when just the object name is input, and (2) mean,
# to compute and return the mean seed distance from the object.
# In each case we use the setMethod function, which requires
# (1) the method name, (2) the pattern of expected formal
# arguments (called the signature), and (3) the function itself.

setMethod("mean",
          signature(x="trapTransect"), 
          function(x, ...) weighted.mean(x@distances,
                                         w=x@seed.counts))

# Creating a generic function for 'mean' from package 'base' 
# in the global environment
# [1] "mean"

setMethod("show",
          signature(object="trapTransect"), 
          function(object) str(object))

# [1] "show"

# We monstrate the application of the new methods to the object
s1

# Formal class 'trapTransect' [package ".GlobalEnv"] with 3 slots
# ..@ distances  : int [1:4] 1 2 3 4
# ..@ seed.counts: num [1:4] 4 3 2 0
# ..@ trap.area  : num 1e-04

mean(s1)

# We list the S4 methods for the trapTransect class by
showMethods(classes="trapTransect")

# Function: initialize (package methods)
# .Object="trapTransect"

# Function: mean (package base)
# x="trapTransect"

# Function: show (package methods)
# object="trapTransect"

# We can display the code for a particular S4 method:
getMethod("mean","trapTransect")

# Method Definition:
  
#  function (x, ...) 
#    weighted.mean(x@distances, w = x@seed.counts)

# Signatures:
#   x             
# target  "trapTransect"
# defined "trapTransect"