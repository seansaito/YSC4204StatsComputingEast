
# Problem 1

# (a) We have chosen to represent the state as a single integer
# ranging from 0 (---) to 7 (+++), basically binary numbers

# This function updates a single state
update_state <- function(state, beta){
  
  # Condition 1 - if we are not in a ground state, then return to 
  # either ground state with equal probability
  if (state != 0 && state != 7){
    state <- sample(c(0, 7), 1, prob = c(0.5,0.5))
  }
  # Condition 2 - if we are in a ground state, then move according 
  # to the following probabilities
  else {
    p1 <- 0.5 * (1 - 3*exp(-4*beta))
    p2 <- 0.5 *exp(-4*beta)
    state <- sample((0:7), 1, prob = c(p1, p2, p2, p2, p2, p2, p2, p1))
  }
  return(state)
}

# This function runs m samples for time t, starting in state +++
ising3site_run <- function(m, t, beta) {
  states <- rep(7, m)
  for (i in seq_len(t)){
    states <- sapply(states, update_state, beta)
  }
  return(states)
}

# (b) 
# The exact boltzmann probabilities are given by (1/Z) * exp(-E_u/beta)

# Calculating Z
p_ground <- exp(3 / beta)
p_higher <- exp(-1 / beta)
Z <- 2 * p_ground + 6 * p_higher

exact <- 1/Z * c(p_ground, p_higher, p_higher, p_higher, p_higher, p_higher, p_higher, p_ground)

m = 10000
beta = 1/3

# Checking for time t = 1, 2, 5, 10, 100
t_1 <- ising3site_run(m, 1, beta)
res_1 <- (as.vector(table(t_1))) / m

t_2 <- ising3site_run(m, 2, beta)
res_2 <- (as.vector(table(t_2))) / m

t_5 <- ising3site_run(m, 5, beta)
res_5 <- (as.vector(table(t_5))) / m

t_10 <- ising3site_run(m, 10, beta)
res_10 <- (as.vector(table(t_10))) / m

t_100 <- ising3site_run(m, 100, beta)
res_100 <- (as.vector(table(t_100))) / m

plot(exact, type = "l", xaxt = "n", xlab ="States", ylab = "Prob",
     main = "3-site Ising Model: Exact vs Monte Carlo probabilities")
lines(res_1, col = "red")
lines(res_2, col = "green")
lines(res_5, col = "blue")
lines(res_10, col = "yellow")
lines(res_100, col = "pink")
legend(x = 2, y = 0.45, legend = c("exact", "t1", "t2", "t5", "t10", "t100"), 
       lwd = 2,col = c("black", "red", "green", "blue", "yellow", "pink"))
axis(side = 1, at = 1:8,
     labels = c("---", "--+", "-+-", "-++", "+--", "+-+", "++-", "+++"))
# Problem 2

# laplace <- function(x, b) {
#   return((0.5 * (1 / b)) * exp(-abs(x)/b))
# }
# 
# m <- 10000
# sigma <- 4
# x <- numeric(m)
# x[1] <- rchisq(1, df=1)
# k <- 0
# u <- runif(m)
# 
# for (i in 2:m) {
#   xt <- x[i - 1]
#   y <- rchisq(1, df = xt)
#   num <- laplace(y, b=sigma) * dchisq(xt, df = y)
#   den <- laplace(xt, b=sigma) * dchisq(y, df = xt)
#   if (u[i] <= num / den) x[i] <- y else {
#     x[i] <- xt
#     k <- k + 1
#   }
# }
# 
# index <- 1000:5500
# y1 <- x[index]
# plot(index, y1, type="l", main="", ylab="x")

# Metropolis Random Walk

laplace <- function(x) { return (0.5*exp(-abs(x)));}
N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25

rw.Metropolis <- function(sigma, x0, N) { 
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (laplace(y) / laplace(x[i-1]))) 
      x[i]<-y 
    else{
      x[i] <- x[i-1]
      k <- k + 1 
    }
  }
  return(list(x=x, k=k)) 
}

n <- 4 #degrees of freedom for target Student t dist. 
N <- 10000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N) 
rw2 <- rw.Metropolis(sigma[2], x0, N) 
rw3 <- rw.Metropolis(sigma[3], x0, N) 
rw4 <- rw.Metropolis(sigma[4], x0, N)
hist(rw3$x, prob=TRUE)
y <- seq(-10, 10, .1)
lines(y, laplace(y), col="blue")

# Question 3

# First, we need to import the code from our last lesson

# setwd("...")

source("hw04/ising_lecture10c.R")

# a) Correlation time vs kT

kTlist <- seq(0.6, 5, 0.2)

# This function takes a really long time to run. 
getACtime <- function(beta) {
  cat ("Beta = ", beta, "\n")
  trans.list <- isingTransient(beta, lx, ly, ngb)
  s <- trans.list$latt.inf
  
  if (beta < 1.8 || beta > 3) {
    sweep = 200
  } else {
    sweep = 1000
  }
  steady.list <- isingSteady(beta, s, ngb, sweep)
  
  mabs <- abs(steady.list$mTimeSeries)
  ac <- acf(mabs, lag.max = 200, plot = FALSE)$acf
  return (match(TRUE, ac < exp(-1)))
}

AClist <- sapply(kTlist, getACtime)

# Part b - Magnetization per spin

exact <- function(beta) {
  if (beta > 2.27) {
    return(0)
  } else {
    return( (1 - sinh(2/beta) ^(1/4)) ^ (1/8) )
  }
}

