# Brent's method requires three values a_k, b_k and b_k - 1
#, and calculates a new iterate each time. For the start, we set 
# a_k to 0, b_k to 5 and b_k - 1 to 0. 

# Next, we compare the results of the 

f <- function(x){
  return(x^2 - 9)
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


