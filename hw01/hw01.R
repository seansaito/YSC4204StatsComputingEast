# Code for YSC4204 Homework 1
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

# Problem 1

# 1 (a)
# Read the text file
florida <- read.table("florida2000.txt", header=TRUE)

# Get the votes for Buchanan and Nader
buchanan <- florida$Buchanan
nader <- florida$Nader

# Plot
plot(nader, buchanan, xlab="Nader", ylab="Buchanan", pch=20, main = "Nader vs. Buchanan")

# 1 (b)
# Linear regression
fit <- lm(buchanan ~ nader)
abline(fit)

# 1 (c)
# Evaluating the model

# First take a look at the residuals
res <- residuals(fit)
plot(jitter(res)~jitter(buchanan), ylab="Residuals", xlab="buchanan", data=florida, pch=20)
abline(0, 0)

# What's the R-squared value?
print(summary(fit)$r.squared)

# The result is 0.428, so maybe it does not look like a reasonable fit

# 1 (d)
# Residual Sum of Squares function
rss = function(row) {
  z <- sum((buchanan - row[1] - row[2] * nader)^2)
}

# Generate of vector of intercepts and slopes
b0 <- seq(1, 120)
b1 <- seq(0.11, 0.18, length=50)

# Compute rss for all (b0, b1)
prod = merge(b0, b1) # Cartesian product of b0 and b1
z <- matrix(apply(prod, 1, rss), nrow=length(b0), ncol=length(b1))

# Plot persp
persp(b0, b1, z, theta=30, phi=20, xlab="Intercept", 
      ylab="Slope", zlab="RSS", ticktype="detailed",
      expand=1.0)

# 1 (e)
# Contour plots
contour(b0, b1, z, xlab="Intercept", ylab="Slope", main="Contour of Intercept, Slope, and RSS")
# Plot the best combination
points(fit$coefficients[1], fit$coefficients[2], pch=20, col="red")


#### Problem 2 - Code first, Comment Later

example <- seq(-10, 10, 0.001)

taylorLoop <- function (n, x) {
  if (n < 1 || length(x) == 0 ){
    print("Either n or x is an invalid parameter")
    break
  }
  output <- numeric(0)  # Setting up the output vector
  for (a in x){   # Outer for loop iterating over x
    
    result <- 0   # Setting up the summation
    for (j in 0:n ){
      result <- result + (a^j / factorial(j))
    }
    output <- c(output, result)    #Concatenate the result to the output vector
  }
  return(output)
}

taylorVect <- function (n, x){
  if (n < 1 || length(x) == 0 ){
    print("Either n or x is an invalid parameter")
    break
  }
  return(unlist(lapply(x, function(a) sum((a ^ (0:n)) / factorial(0:n)))))
}

system.time(taylorLoop(10, example))
system.time(taylorVect(10, example))

snx_vect <- function(n, x)
  (sum(unlist(lapply(c(0:n), function(j) ((factorial((2*n)-j)*factorial(n))*(x^j))/(factorial(2*n)*factorial(j)*factorial(n-j))))))

pade <- function(n, x) {
  return(unlist(lapply(x, function(xval) (snx_vect(n, xval)/snx_vect(n, 0-xval)))))
}

## Graphing

taylor10 <- function(x) taylorVect(10, x)
pade10 <- function(x) pade(10, x)

ggplot(data.frame(x = c(-10, 10)), aes(x)) + 
    stat_function(fun = exp, aes(colour="exp")) +
    stat_function(fun = taylor10, aes(colour="taylor")) +
    stat_function(fun=pade10, aes(colour="pade")) + 
    scale_y_log10()

