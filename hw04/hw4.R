# Problem 1

# (a) 

# This function updates a single state
update_inner <- function(state, beta){
  if (state != 0 && state != 7){
    state <- sample(c(0, 7), 1, prob = c(0.5,0.5))
  } else {
    p1 <- 0.5 * (1 - 3*exp(-4*beta))
    p2 <- 0.5 *exp(-4*beta)
    
    state <- sample((0:7), 1, prob = c(p1, p2, p2, p2, p2, p2, p2, p1))
  }
  return(state)
}

# This function applies the update to a vector of states
update <- function(state, beta){
  return(sapply(state, update_inner, beta))
}

beta = 1/3
p1 <- 0.5 * (1 - 3*exp(-4*beta))
p2 <- 0.5 *exp(-4*beta)

probs <- c(p1, p2, p2, p2, p2, p2, p2, p1)

# This function runs 
run <- function(m, t, beta) {
  states <- rep(7, m)
  for (i in seq_len(t)){
    states <- update(states, beta)
  }
  return(states)
}

result <- run(1000, 1000, 1/3)
hist(result, prob = TRUE)

# (b) 
# The exact boltzmann probabilities are given by (1/Z) * exp(-E_u/beta)

# Let us first calculate Z
Z <- 2 * exp(3/beta) + 6 * exp(-1/beta)

p_ground <- exp(3 / beta)
p_higher <- exp(-1 / beta)

exact <- 1/Z * c(p_ground, p_higher, p_higher, p_higher, p_higher, p_higher, p_higher, p_ground)

#plot(exact)



