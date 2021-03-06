# Automatically sets working directory to source file location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


#### Question 1 ####
# An iteration of Brent's method requires three values a_k, b_k and b_k - 1
# and calculates a new iterate each time. We start with two values and set 
# a_k to 0, b_k to 5. R uses the secant method to calculate the 
# third value, which is 1.8.

# Next, R does bisection of (1.8, 5) to get 3.4, which is the 
# result after one iteration. In the second iteration, R uses secant (1.8, 3.4) to
# get 2.9067. We were initially puzzled by why R didn't use 
# inverse quad here, since the source code in zeroin.c seems to suggest
# that secant is only every used instead of inverse quadratic when 
# two of the values are not distinct i.e. the iteration only has two distinct
# points to work with. We surmise that R has thrown away one of the previous 
# results, and thus cannot do inverse quadratic interpolation. 

# The third iteration uses secant as well. The fourth iteration uses 
# inverse quadratic interpolation. 

f <- function(x){
  return(x^2 - 9)
}

get_x_at_iter <- function(n) {
  res <- uniroot(f, c(0,5), maxiter=n)
  return(res$root)
}
four_iterations <- sapply(1:4, get_x_at_iter)

bisec <- function(f, a, b, iter) {
  # a is negative, b is positive
  for (i in 1:iter) {
    fa <- f(a)
    fb <- f(b)
    half <- (a+b)/2
    if (f(half) < 0) {
      a <- half
    } else if (f(half) > 0){
      b <- half
    } else {
      cat("Converged\n")
      cat("Root ", half, "\n")
    }
    cat("Iteration ", i, "Half-point: ", half, " with value of ", f(half), "\n")
  }
}

bisec_simple <- function(f, a, b) {
  return ((a + b)/2)
}

secant <- function(f, a, b){
  return(a - f(a) * (a - b) / (f(a) - f(b)))
}

secant_loop <- function(f, a, b){
  xa = a
  xb = b
  for (i in 1:10){
    temp = xa
    xa = secant(f, xa, xb)
    xb = temp
    print(xa)
  }
  return(xa)
}

inverse_quad_loop <- function (f, a, b, c, n){
  for (i in 1:n){
    temp1 = a
    temp2 = b
    a = inverse_quad(f, a, b, c)
    b = temp1
    c = temp2
    print(a)
  }
}

inverse_quad <- function(f, a, b, c){
  fa = f(a)
  fb = f(b)
  fc = f(c)
  term1 <- (fa * fb) / ((fc - fb) * (fc - fa)) * c
  term2 <- (fc * fa) / ((fb - fc) * (fb - fa)) * b
  term3 <- (fb * fc) / ((fa - fc) * (fa - fb)) * a
  return (term1 + term2 + term3)
}

a <- 0
b <- 5

cat("\nUniroot returns ", four_iterations)
iter0 = secant(f, a, b)
cat("\nResult after 0th iteration is ", iter0)

iter1 = bisec_simple(f, iter0, b)
cat("\nResult after 1st iteration (bisection) is ", iter1)
iter1_quad = inverse_quad(f, a, iter0, b)
cat("\nResult after 1st iteration for inverse quad is ", iter1_quad)

iter2 = secant(f, iter0, iter1)
cat("\nResult after 2nd iteration (secant) is ", iter2)
iter2_quad = inverse_quad(f, iter0, iter1, b)
cat("\nResult after 2nd iteration for inverse quad is ", iter2_quad)

iter3 = secant(f, iter1, iter2)
cat("\nResult after 3rd iteration (secant) is ", iter3)
iter3_quad = inverse_quad(f, iter0, iter2, iter1)
cat("\nResult after 3rd iteration for inverse quad is ", iter3_quad)

iter4 = secant(f, iter2, iter3)
cat("\nResult after 4th iteration (inverse quad) is ", iter4)
iter4_quad = inverse_quad(f, iter2, iter3, iter1)
cat("\nResult after 4th iteration for inverse quad is ", iter4_quad)

#### Question 2 ####
# (a)
x <- c(-7.99,24.11,3.41,7.01,22.28,0.54,-1.97,15.97,11.53,237.63)

g <- function(gam){
  return(10/gam - sum(2*gam/(x^2+gam^2)))
}

# (b)
bisec <- function(f, a, b, iter) {
  # a is negative, b is positive
  for (i in 1:iter) {
    fa <- f(a)
    fb <- f(b)
    half <- (a+b)/2
    if (f(half) < 0) {
      a <- half
    } else if (f(half) > 0){
      b <- half
    } 
    if (abs(f(half)) < 1e-6) {
      cat("Converged on iteration", i, "\n")
      cat("Root is ", half, " with value ", f(half), "\n")
      break
    }
   # cat("Iteration ", i, "Half-point: ", half, " with value of ", f(half), "\n")
  }
}
bisec(g, 100, 1, 55)
# Converges after 19 iterations for accuracy 10^-6

# (c)
res <- uniroot(g, c(1, 100), tol=1e-6, check.conv=TRUE)
print(res$iter)
# Converges at 12

# (d)
gprime <- function(gam) {
  return(-10 / (gam^2) - sum((2 * x^2 - 2 * gam^2) / (x^2 + gam^2)^2))
}

get_new_x <- function(x, f, fprime) {
  y <- f(x)
  m <- fprime(x)
  c <- y - (m * x)
  
  # Solve for 0 = mx + c
  intercept <- -c / m
  return(intercept)
}

newton_raphson <- function(f, fprime, gam) {
  i <- 1
  while (abs(f(gam)) > 1e-6) {
    gam <- get_new_x(gam, f, fprime)
    cat("Iteration ", i, " with root ", gam, " and f.root ", f(gam), "\n")
    i <- i + 1
  }
  cat("Converged at iteration ", i, " with root ", gam, " and f.root ", f(gam), "\n")
}

newton_raphson(g, gprime, 1)
# Converged at iteration 9

#### Problem 4 ####

# (a)
face <- read.csv("face_recog.csv",as.is=TRUE)
lr <- glm(face$match~face$eyediff,family=binomial,data=face)
cat(lr$coefficients) #) MLEs

# (b)
# Newton-Raphson using explicit matrix calculations
nobs <- nrow(face)
beta <- c(1.0, 0.0)

X <- cbind(rep(1, nobs), face$eyediff)
for (i in 1:4) {
  pi <- 1.0 / (1.0 + exp(-as.vector(X %*% beta)))
  W <- diag(pi * (1 - pi))
  fac1 <- solve(t(X) %*% W %*% X)
  fac2 <- t(X) %*% (face$match - pi)
  beta <- beta + fac1 %*% fac2
  print(beta)  
}

# (c)
# Editted the fitting function via trace(glm.fit, edit=TRUE)

# Lines 78 - 81. Printing the intermediate coefficients during the loop
# for (iter in 1L:control$maxit) {
#   print(coef)
#   good <- weights > 0

# Lines 226 - 229. Printing the final coefficients
# aic.model <- aic(y, n, mu, weights, dev) + 2 * rank
# print(coef)
# list(coefficients = coef, residuals = residuals, fitted.values = mu, 
#      effects = if (!EMPTY) fit$effects, R = if (!EMPTY) Rmat, 

# This prints out the intermediate coefficients
glm(face$match~face$eyediff,family=binomial,data=face)

# These are the results we get when we print intermediate coefficients
# [1]   1.82592 -13.55090
# [1]   1.757199 -13.389226
# [1]   1.758701 -13.400035
# (Intercept) face$eyediff 
# [1] 1.758701   -13.400040 

# It appears that glm is not using pure Newton Raphson, since the 
# intermediate results are not the same. However, they converge in
# the same number of iterations, with seemingly the same degree of 
# precision. In the help menu for glm.fit, under method, it states
# that the default method used is 'iteratively reweighted least squares'.
