# Brent's method requires three values a_k, b_k and b_k - 1
#, and calculates a new iterate each time. For the start, we set 
# a_k to 0, b_k to 5 and b_k - 1 to 0. 

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

inverse_quad <- function(f, a, b, c){
  fa = f(a)
  fb = f(b)
  fc = f(c)
  term1 <- (fa * fb) / ((fc - fb) * (fc - fa)) * c
  term2 <- (fc * fa) / ((fb - fc) * (fb - fa)) * b
  term3 <- (fb * fc) / ((fa - fc) * (fa - fb)) * a
  return (term1 + term2 + term3)
}


