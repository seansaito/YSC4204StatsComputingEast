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