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

# Problem 1

# (a)
# Option 1
# https://stat.ethz.ch/mailman/listinfo/r-devel
# A "list is intended for questions and discussion about code development in R. 
# Questions likely to prompt discussion unintelligible to non-programmers or topics 
# that are too technical for R-help???s audience should go to R-devel"
# 
# Option 2
# https://stat.ethz.ch/mailman/listinfo/r-package-devel
# This is the mailing list to all package developers of R
# To get " help about package development in R"

# (b)
# Draft of the question
# In src/nmath/sexp.c, the Ahrens-Dieter algorithm is being used to generate random numbers
# from an exponential distribution.
# 
# I am curious as to why this algorithm is favored over the inverse transform method, which is the
# straightforward method of producing random numbers from a particular probability distribution function.
# For example, since the cumulative distribution function of the exponential function is F_x(X) = 1 - e^{-lambda x},
# one would use its inverse, F_x^-1(u) = -(1/lambda)log(1-u), to produce samples. In R, this is as easy as
# -log(runif(n)) / lambda.
# 
# I have tried to hypothesize some benefits of the Ahrens-Dieter algorithm myself. However there does not seem to be 
# any asymptotic time benefits, nor any obvious improvement in precision.

# Problem 2
# Assessing a proposed alternative to the Box-Muller method

library(MVN)

# Helper functions
z1_gen <- function(u, v) {
  return(sqrt(-2 * log(u)) * cos(2 * pi * v))
}

z2_gen <- function(u, v) {
  return(sqrt(-2 * log(v)) * sin(2 * pi * u))
}

# Initialization
n = 2000
u <- runif(n)
v <- runif(n)

# Get z1 and z2
z1 <- z1_gen(u, v)
z2 <- z2_gen(u, v)

# Plot the distributions
plot(z1, z2, main="Z1 vs. Z2", xlab="Z1", ylab="Z2")

pair <- matrix(c(c(z1), c(z2)), ncol=2)
pair_df <- data.frame(pair)

# Assess the pair using MVN
# Useful documentation here: https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
mardiaResult <- mardiaTest(pair_df, qqplot=TRUE)
print(mardiaResult)
# Skew is too big, and thus is not multivariate normal
hzResult <- hzTest(pair_df)
print(hzResult)
# HZ statistic is too high - not multivariate normal
roystonResult <- roystonTest(pair_df)
print(roystonResult)
# Here, the Royston Test thinks the data is multivariate normal

# In the end, however, the majority of tests label the data as not multivariate normal.
# The Q-Q plot, which plots the theoretical and observed quantiles of the different distributions, 
# also shows that the points deviate from the line which indicates multivariate normality.


