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
b0 <- seq(1, 120,3)
b1 <- seq(0.11, 0.18, length=40)

# Compute rss for all (b0, b1)
prod = merge(b0, b1) # Cartesian product of b0 and b1
z <- matrix(apply(prod, 1, rss), nrow=length(b0), ncol=length(b1))

# Prepare a matrix of colors scaled to the RSS value, to use in the plot
alphas <- (z-min(z))/(max(z)-min(z))
colors <- rgb(matrix(1,40,40),matrix(0,40,40),matrix(0,40,40),alphas)

# Plot persp
persp(b0, b1, z, theta=65, phi=10, xlab="Intercept",
      ylab="Slope", zlab="RSS", ticktype="detailed",
      expand=0.5,col=colors)

# 1 (e)
# Contour plots
contour(b0, b1, z, xlab="Intercept", ylab="Slope", main="Contour of Intercept, Slope, and RSS")
# Plot the best combination
points(fit$coefficients[1], fit$coefficients[2], pch=20, col="red")


#### Problem 2 - Code first, Comment Later

# 2 (a)
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

# 2(b)
taylorVect <- function (n, x){
  if (n < 1 || length(x) == 0 ){
    print("Either n or x is an invalid parameter")
    break
  }
  return(unlist(lapply(x, function(a) sum((a ^ (0:n)) / factorial(0:n)))))
}

# 2(c)
example <- seq(-10, 10, 0.001)
system.time(taylorLoop(10, example))
system.time(taylorVect(10, example))

# 2(d)
# Compute S_nx
snx_vect <- function(n, x)
  (sum(unlist(lapply(c(0:n), function(j) ((factorial((2*n)-j)*factorial(n))*(x^j))/(factorial(2*n)*factorial(j)*factorial(n-j))))))

# Apply over x
pade <- function(n, x) {
  return(unlist(lapply(x, function(xval) (snx_vect(n, xval)/snx_vect(n, 0-xval)))))
}

# 2(e)
taylor10 <- function(x) taylorVect(10, x)
taylor100 <- function(x) taylorVect(100, x)
pade10 <- function(x) pade(10, x)

ggplot(data.frame(x = c(-10, 10)), aes(x)) + 
    stat_function(fun = exp, aes(colour="exp")) +
#    stat_function(fun = taylor10, aes(colour="taylor")) +
    stat_function(fun = taylor100, aes(colour="taylor100")) +
    stat_function(fun=pade10, aes(colour="pade")) + 
    scale_y_log10()

# 2 (f)
system.time(taylorLoop(10, example))
system.time(taylorVect(10, example))
system.time(pade(10, example))

# 2(g)
# Pade is the most accurate representation, but taylor is much faster. 
# However, let's first test this assumption (Graph2.png)- 

# Code below to generate graph2 (takes a while)
# xval <- seq(1,100,1)
# padeY <- unlist(lapply(xval, function(x) as.double(system.time(pade(x, c(10:100)))[3])))
# taylorY <- unlist(lapply(xval, function(x) as.double(system.time(taylorVect(x, c(10:100)))[3])))
# ggplot(data.frame(x=xval, tY=taylorY, pY=padeY), aes(x))+geom_line(aes(y=tY, color='taylor')) + 
#  geom_line(aes(y=pY, color='pade')) + ggtitle('Runtime against n - pade and taylor')

# From the curve we can see that taylor is much faster than pade, even for high values of n.
# but what about accuracy? Let's try graphing higher values of n for taylor. (Graph3)

# Code below to generate graph3 (also takes a little while, but not long)
# ggplot(data.frame(x = c(-10, 10)), aes(x)) + stat_function(fun = exp, aes(colour="exp"))+
#  stat_function(fun = function (x) taylorVect(10, x), aes(color="10")) +
#  stat_function(fun = function (x) taylorVect(20, x), aes(color="20")) +
#  stat_function(fun = function (x) taylorVect(30, x), aes(color="30")) +
#  stat_function(fun = function (x) taylorVect(40, x), aes(color="40")) +
#  stat_function(fun = function (x) taylorVect(50, x), aes(color="50")) +
#  stat_function(fun = function (x) taylorVect(60, x), aes(color="60")) +
#  scale_y_log10() + ggtitle('Values of exp and taylor(n=10-60)')

# Taylor is the better choice. By using higher values of n depending on the value of x,
# we can still provide fast results without sacrificing accuracy.
# Taylor is a better choice for industry, but pade is a better solution for smaller datasets.
