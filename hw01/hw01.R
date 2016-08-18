# Code for YSC4204 Homework 1
# Authors
# Sean Saito
# John Reid
# Hrishi Olickel
# Han Chong

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
persp(b0, b1, z, theta=120, phi=5, xlab="Intercept", 
      ylab="Slope", zlab="RSS", ticktype="detailed",
      expand=1.0)

# 1 (e)
# Contour plots
contour(b0, b1, z, xlab="Intercept", ylab="Slope", main="Countour of Intercept, Slope, and RSS")
# Plot the best combination
points(fit$coefficients[1], fit$coefficients[2], pch=20, col="red")
