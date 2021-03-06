
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
# The exact boltzmann probabilities are given by (1/Z) * exp(-E_u*beta)

# Calculating Z
m = 10000
beta = 1/3

p_ground <- exp(3 * beta)
p_higher <- exp(-1 * beta)
Z <- 2 * p_ground + 6 * p_higher

# Boltzmann Probabilities
exact <- 1/Z * c(p_ground, p_higher, p_higher, p_higher, p_higher, p_higher, p_higher, p_ground)

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

## Plot comparisons of probabilities ##
plot(res_1, type = "l",col="red",lwd=2, xaxt = "n",
     xlab ="States", ylab = "Prob",
     main = "3-site Ising Model: Boltzmann vs MCMC probabilities",
     ylim=c(0,0.35))
lines(res_2, col = "green",lwd=2)
lines(res_5, col = "blue",lwd=2)
lines(res_10, col = "grey",lwd=2)
lines(res_100, col = "pink2",lwd=2)
lines(exact, col="black",lwd=2,lty=2)
legend(x = 2, y = 0.35,
       legend = c("Boltzmann", "t=1", "t=2", "t=5", "t=10", "t=100"), 
       lwd = 2,col = c("black", "red", "green", "blue", "grey", "pink2"),
       lty=c(2,1,1,1,1,1))
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

N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N) 
rw2 <- rw.Metropolis(sigma[2], x0, N) 
rw3 <- rw.Metropolis(sigma[3], x0, N) 
rw4 <- rw.Metropolis(sigma[4], x0, N)

# Check histogram
hist(rw2$x, prob=TRUE,breaks=50,
     main="Distribution of points generated with sigma=2,\n compared against Laplace distribution",
     xlab="X")
y <- seq(-10, 10, .1)
lines(y, laplace(y), col="blue")

# Acceptance rates
print("Acceptance rates")
print(c(rw1$k / 2000, rw2$k / 2000, rw3$k / 2000, rw4$k / 2000))

# index <- 1000:5500
# y1 <- rw1$x[index]
y <- 1:2000
par(mfrow=c(2,2))
plot(rw1$x, type="l", main="", xlab="Sigma = 0.05", ylab="x")
lines(y, rep(-2.5, 2000))
lines(y, rep(2.5, 2000))
plot(rw2$x, type="l", main="", xlab="Sigma = 0.5", ylab="x")
lines(y, rep(2.5, 2000))
lines(y, rep(-2.5, 2000))
plot(rw3$x, type="l", main="", xlab="Sigma = 2", ylab="x")
lines(y, rep(2.5, 2000))
lines(y, rep(-2.5, 2000))
plot(rw4$x, type="l", main="", xlab="Sigma = 16", ylab="x")
lines(y, rep(2.5, 2000))
lines(y, rep(-2.5, 2000))
par(mfrow=c(1,1))

# Tables 
qlaplace <- function(x) { 
  return(ifelse(x==0.5, 0, ifelse(x<0.5, (log(2*(0.5-x))), (-log(2*(x-0.5))))))}
a <- c(.05, seq(.1, .9, .1), .95)
Q <- qlaplace(a)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
mc <- rw[501:N, ]
Qrw <- apply(mc, 2, function(x) quantile(x, a)) 
print(round(cbind(Q, Qrw), 3)) #not shown 
xtable::xtable(round(cbind(Q, Qrw), 3)) #generate latex table

# Comments
# As also observed in the Rizzo book, the chains are very sensitive to the variance of
# the proposal distribution. Our desirable acceptance ratio is in the range [0.15, 0.5]
# The third and fourth walk generate acceptance rates that come within this range.

# Question 3

# First, we need to import the code from our last lesson

# setwd("...")

source("~/hw04/ising_lecture10c.R")

# a) Correlation time vs kT

kTlist <- 1 / seq(0.6, 5, 0.2)

# I split this into three to check intermediate results
kTlist.a <- kTlist[1:6]
kTlist.b <- kTlist[7:12]
kTlist.c <- kTlist[13:23]

# Essentially the same function from class
getACtime <- function(beta, steady_sweep) {
  cat ("Beta = ", beta, "\n")
  trans.list <- isingTransient(beta, lx, ly, ngb)
  s <- trans.list$latt.inf
  steady.list <- isingSteady(beta, s, ngb, steady_sweep)
  
  mabs <- abs(steady.list$mTimeSeries)
  ac <- acf(mabs, lag.max = 2000, plot = FALSE)$acf
  print(match(TRUE, ac < exp(-1)))
  return (match(TRUE, ac < exp(-1)))
}

# Compute
AC.a <- sapply(kTlist.a, getACtime, 1000)
AC.b <- sapply(kTlist.b, getACtime, 1000)
AC.c <- sapply(kTlist.c, getACtime, 1000)

AC.final <- c(AC.a, AC.b, AC.c)

# Plot
x <- seq(0.6, 5.0, 0.2)
plot(x, AC.final, main = "Correlation time vs kT", 
     xlab = "kT", ylab = "Correlation time")
lines(x, AC.final)

# Part b - Magnetization per spin

onsagerExact <- function(kT) {
  return (ifelse(kT >= 2.27, 0, (1 - sinh(2/kT) ^(-4)) ^ (1/8) ))
}

getMag <- function(beta, steady_sweep) {
  cat ("Beta = ", beta, "\n")
  trans.list <- isingTransient(beta, lx, ly, ngb)
  s <- trans.list$latt.inf
  steady.list <- isingSteady(beta, s, ngb, steady_sweep)
  
  mabs <- abs(steady.list$mTimeSeries)
  print(mean(mabs))
  return (mean(mabs))
}

# Compute
mag.final <- sapply(kTlist, getMag, 1000)

# Plot
plot(seq(0.6, 5.0, 0.2), mag.final, main ="Magnetization vs kT", 
     xlab = "kT", ylab = "|mag|")

points <- seq(0.6, 5.0, 0.01)
lines(points, onsagerExact(points))
