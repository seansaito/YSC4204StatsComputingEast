# Question 1 - Golden Section Search

# This is a helper function to get a middle that is 
# lower than two endpoints. It could probably use some 
# optimization, since it does simple bisection, and may
# not work for more complex functions that have sudden, sharp 
# minima. 

middle_minimum <- function(f, ax, cx) {
  # Find a bx point that is lower than the two endpoints
  bx <- ax + abs(cx - ax)/2 # Halfway point in interval
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
    cat("\n xmin = ", x1, " , f(xmin) = ", f1)
    return(x1)
  } else {
    cat("\n xmin = ", x2, " , f(xmin) = ", f2)
    return(x2)
  }
}

# Test the golden section search. f has a minimum at x = 3
f <- function(x) (x - 3)^2
res <- gss_min(f, 0, 5)
cat("Expected 3, got ", res)
