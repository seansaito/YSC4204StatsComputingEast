#### Question 1 ####
# Brent's method requires three values a_k, b_k and b_k - 1
#, and calculates a new iterate each time. For the start, we set 
# a_k to 0, b_k to 5. We use the secant method to calculate the 
# third value, which is 1.8.

# Next, we do bisection of (1.8, 5) to get 3.4, which is the value 
# result after one iteration. After that, we do secant (1.8, 3.4) to
# get 2.9067

# For the last two iterations, we use inverse quadratic interpolation. 

# Next, we compare the results of the 

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
cat("\nResult after 1st iteration is ", iter1)

iter2 = secant(f, iter0, iter1)
cat("\nResult after 2nd iteration is ", iter2)

iter3 = secant(f, iter1, iter2)
cat("\nResult after 2nd iteration is ", iter3)

iter4 = secant(f, iter2, iter3)
cat("\nResult after 2nd iteration is ", iter4)

#### Question 2 ####
# (a)
x <- c(-7.99,24.11,3.41,7.01,22.28,0.54,-1.97,15.97,11.53,237.63)

g <- function(gam){
  return(10/gam - sum(2*gam/(x^2+gam^2)))
}

# (b)
bisec <- function(f, a, b) {
  # a is negative, b is positive
  i <- 1
  while (TRUE) {
    fa <- f(a)
    fb <- f(b)
    half <- (a+b)/2
    if (f(half) < 0) {
      a <- half
    } else if (f(half) > 0){
      b <- half
    } else if (f(half) < 1e-6) {
      cat("Converged on iteration ", i, " with root ", half, "\n")
      break
    } else {
    }
    # cat("Iteration ", i, "Half-point: ", half, " with value of ", f(half), "\n")
  }
}

# (c)
res <- uniroot(g, c(1, 100), check.conv=TRUE)
print(res$iter)
