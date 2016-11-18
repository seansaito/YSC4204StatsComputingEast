# Question 1 - Golden Section Search

# This is a helper function to get a middle that is 
# lower than two endpoints. It evaluates candidate points at
# the golden ratio on either side, and picks the one with the 
# lower function value. 

middle_minimum <- function(f, ax, cx, tol = 1e-4) {
  R <- 0.61803399
  # Find a bx point that is lower than the two endpoints
  bx <- ax + abs(cx - ax)/2 # Halfway point in interval
  fa <- f(ax)
  fb <- f(bx)
  fc <- f(cx)
  
  while (fb >= fc || fb >= fc) # fb greater than two endpoints
  {
    if (abs(fb - fa) > tol) {
      cat("Minimum found at lower endpoint")
      return(ax)
    } else if (abs(fb - fc) > tol) {
      cat("Minimum found at upper endpoint")
      return(cx)
    }
    
    # check candidate points to either side of bx 
    bx1 <- bx - abs(bx - ax) * R
    bx2 <- bx + abs(bx - cx) * R
    if (f(bx1) < f(bx2)) {
      bx <- bx1
    } else {
      bx <- bx2
    }
    fb <- f(bx)
  }
  return(bx)
}

# Mostly a simple translation from Numerical Recipes. 
gss_min <- function(f, ax, cx, tol=1e-4) {
  R <- 0.61803399
  C <- 1-R
  count <- 0
  
  # Find a bx point that is lower than the two endpoints
  bx <- middle_minimum(f, ax, cx)
  fa <- f(ax)
  fb <- f(bx)
  fc <- f(cx)
  while ((fb >= fc || fb >= fc) &&  # fb greater than two endpoints
         (abs(bx - ax) > tol || abs(bx - cx) > tol)) # bx not too close to endpoints
    {
    # check candidate points to either side of bx 
    bx1 <- bx - abs(bx - ax)/2
    bx2 <- bx + abs(bx - cx)/2
    if (f(bx1) < f(bx2)) {
      bx <- bx1
    } else {
      bx <- bx2
    }
    fb <- f(bx)
  }
  
  # Initialize the four points we will be keeping track of
  x0 <- ax
  x1 <- 0
  x2 <- 0
  x3 <- cx
  
  # Determine the two new points
  if (abs(cx-bx) > abs(bx-ax)) {
    x1 <- bx
    x2 <- bx + C*(cx - bx)
  } else {
    x2 <- bx
    x1 <- bx - C*(bx-ax)
  }
  
  # Initial function evaluations
  f1 <- f(x1)
  f2 <- f(x2)
  
  while (abs(x3 - x0) > tol*(abs(x1) + abs(x2))) {
    if (f2 < f1) {
      x0 <- x1
      x1 <- x2
      x2 <- R*x1 + C*x3
      f1 <- f2
      f2 <- f(x2)
    } else {
      x3 <- x2
      x2 <- x1
      x1 <- R*x2 + C*x0
      f2 <- f1
      f1 <- f(x1)     
    }
    count <- count + 1
  }
  
  cat("\n Search took ", count, " iterations")
  if (f1 < f2) {
    cat("\nxmin = ", x1, ", f(xmin) = ", f1)
    return(x1)
  } else {
    cat("\nxmin = ", x2, ", f(xmin) = ", f2)
    return(x2)
  }
}

# Test the golden section search. f has a minimum at x = 3
f <- function(x) -(x - 3)^2
res <- gss_min(f, 0, 5)
cat("\nExpected 0, got ", res, "\n")

# Problem 3
# (a)
f <- function(x) {
  return(x^4)
}

integrated_f <- function(x) {
  return(0.2 * (x ^ 5))
}

simpson <- function(fun, a, b, n=100) {
  h <- (b-a)/n
  x <- seq(a, b, by=h)
  if (n == 2) {
    s <- fun(x[1]) + 4*fun(x[2]) +fun(x[3])
  } else {
    s <- fun(x[1]) + fun(x[n+1]) + 2*sum(fun(x[seq(2,n,by=2)])) + 4 *sum(fun(x[seq(3,n-1, by=2)]))
  }
  s <- s*h/3
  return(s)
}
a <- -1
b <- 1
n = 20

res <- simpson(f, a, b, n)
cat("Simpson Integral result: ", res, "\n")
cat("Difference between exact integral is ", 0.4 - res, "\n")

# (b)
abscissae <- c(-0.1488743389816312, 0.1488743389816312, -0.4333953941292472, 0.4333953941292472,
               -0.6794095682990244, 0.6794095682990244, -0.8650633666889845, 0.8650633666889845,
               -0.9739065285171717, 0.9739065285171717)
weights <- c(0.2955242247147529, 0.2955242247147529, 0.2692667193099963, 0.2692667193099963,
             0.2190863625159820, 0.2190863625159820, 0.1494513491505806, 0.1494513491505806,
             0.0666713443086881, 0.0666713443086881)

gauss_legendre <- function(f) {
  return(sum(f(abscissae) * weights))
}

res <- gauss_legendre(f)
cat("Gaussian-Legendre quadrature result: ", res, "\n")
# Therefore the Gauss-Legendre quadrature approximates the intergral much more accurately
# than Simpson at the given points for evaluating the integrand. If we increase the number of
# points that the Simpson quadrature evaluates (say from 20 to 20000), then the approximation
# will improve.
