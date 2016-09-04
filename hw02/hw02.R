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
  for (k in seq_len(3)) {
    r <- (q/p) ^ 2
    s <- r / (4 + r)
    p <- p * (1 + 2*s)
    q <- q * s
  }
  p <- ifelse (a==0.0 & b==0.0, 0.0, p)
  return(p)
}

# Explanation
# From the geometrical figure, one can see that the target value, or the Pythagorean sum of a and b,
# is the radius of the circle. Hence the intersection between the x-axis and the circle is our target value.
# What the algorithm does is to update p0 every iteration such that it gets closer to the intercept.

## FULL EXPLANATION IS IN THE PDF

# (b)
pythag2 <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b)) # Sanity check.
    stop("ERROR: pythag2() needs numeric input.")
  return(sqrt(a^2 + b^2))
}

# Try these values
x = 3e200
y = 4e200
pythag2(x,y) # returns Inf
pythag(x,y) # returns 5e200

# Now try these values
x = 3e-200
y = 4e-200
> pythag2(x,y) # returns 0
> pythag(x,y) # returns 5e-200

# ANSWER: The reasons pythag2() fails is likely due to an overflow or underflow in the intermediary results

# Problem 3 - Inverse Transform Methdd

# Define the standard laplacian pdf
laplacian_pdf <- function (x){
  return(0.5 * exp(-abs(x)))
}

# Define the laplacian cdf (not used)
laplacian_cdf <- function (x){
  return(ifelse(x < 0, 0.5*exp(x), 1 - 0.5*exp(-x)))
}

# Define the inverse transform of the laplacian cdf
inv_trf <- function (x){
  return(ifelse(x < 0.5, log(2*x), -log(2 - 2*x)))
}

# Generate the random sample
result <- inv_trf(runif(10000))

# Graph the sample
hist(result, probability = TRUE, breaks = 100,
     xlim = c(-5, 5), ylim = c(0,0.5))

# Overlay the line
x <- seq(-5,5,0.1)
y <- laplacian_pdf(x)
lines(x,y,col = "red")



# Problem 4 - Rayleigh Distribution

## Define function to generate Rayleigh random variables ##
rray <- function(n,sigma){
  u <- runif(n)
  output <- sqrt(-2*sigma^2*log(u))
  return(output)
}

## Set seed to ensure consistency before generating numbers and plotting ##
set.seed(0)

########## simga = 0.5 ##########
x1 <- rray(10000,0.5)
hist(x1,prob=TRUE,
     ylim=c(0,1.3), # extend the y-axis so that the density line will not be cut off
     main="Generated rayleigh random variables with sigma=0.5",
     xlab="x") # plot the histogram

## Overlay a density line ##
xlines1 <- seq(0,round(max(x1)),0.01)
ylines1 <- (xlines1/0.5^2)*exp(-xlines1^2/(2*(0.5^2)))
lines(xlines1,ylines1)

########## sigma = 2 ##########
x2 <- rray(10000,2)
hist(x2,prob=TRUE,
     main="Generated rayleigh random variables with sigma=2",
     xlab="x") # plot the histogram

## Overlay a density line ##
xlines2 <- seq(0,round(max(x2)),0.01)
ylines2 <- (xlines2/2^2)*exp(-xlines2^2/(2*(2^2)))
lines(xlines2,ylines2)
