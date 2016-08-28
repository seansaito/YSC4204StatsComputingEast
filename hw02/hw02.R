# Code for YSC4204 Homework 2
# Authors
# Sean Saito
# John Reid
# Hrishi Olickel
# Han Chong

#imports
require(ggplot2)

# Automatically sets working directory to source file location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Problem 1 - Writing a mean function in C to be used in R
meanR <- function(x) {
  # Placeholder for result
  res = 0.0
  n = length(x)
  
  dyn.load("mean.so")
  m = .C("meanC", x=as.double(x), n=n, res=res)
  
  return(m$res)
}

# Problem 2 - Pythag
# (a)
pythag <- function(a, b) {
  # Function to compute the Pythagorean sum sqrt(a^2 + b^2).
  # a and b can be numeric vectors of any length.
  if (!is.numeric(a) || !is.numeric(b)) # Sanity check.
    stop("ERROR: pythag() needs numeric input.")
  absa <- abs(a)
  absb <- abs(b)
  p <- pmax(absa, absb)
  q <- absa + absb - p
  for (k in seq_len(3)) f
  r <- (q/p) ^ 2
  s <- r / (4 + r)
  p <- p * (1 + 2*s)
  q <- q * s
  g
  p <- ifelse (a==0.0 & b==0.0, 0.0, p)
  return(p)
}

# Explanation
# From the geometrical figure, one can see that the target value, or the Pythagorean sum of a and b,
# is the radius of the circle. Hence the intersection between the x-axis and the circle is our target value.
# What the algorithm does is to update p0 every iteration such that it gets closer to the intercept.

## MORE EXPLANATION NEEDED

# (b)
pythag2 <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b)) # Sanity check.
    stop("ERROR: pythag2() needs numeric input.")
  return(sqrt(a^2 + b^2))
}