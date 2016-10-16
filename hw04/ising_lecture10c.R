showlatt <- function(latt) {
  
  # Function to plot a square lattice with the cell at (i, j) coloured
  # black if latt[i, j] == 1.
  
  if (!is.matrix(latt))                                 # Sanity check.
    stop("latt in showlatt() must be a matrix")
  lx <- nrow(latt)                         # Obtain lattice dimensions.
  ly <- ncol(latt)
  op <- par(pty = "s",                          # Square plotting area,
            mar = rep(0, 4))                    # no margins.
  on.exit(par(op))          # Reset graphics parameters before exiting.
  plot(c(1, lx+1), c(1, ly+1), type = "n",   # Empty plot to initialize
       axes = FALSE, xlab = "", ylab = "",   # the coordinate system.
       asp = 1)
  for (i in seq_len(lx))
    for (j in seq_len(ly))
      if (latt[i, j] == 1)
        rect(i, j, i+1, j+1, col = "black")       # Colour the lattice.
  if (max(lx, ly) < 100) {                   # Draw cell boundaries 
    for (i in seq_len(lx + 1))               # if the lattice is small.
      lines(c(i, i), c(1, ly + 1), col = "grey")
    for (i in seq_len(ly + 1))
      lines(c(1, lx + 1), c(i, i), col = "grey")
  } else {                                        # Otherwise only draw
    rect(1, 1, lx+1, ly+1, border = "grey")       # the outer shape.
  }
}

#######################################################################
# Functions to return the neighbours of the k-th site on a square
# lattice with periodic boundary conditions.

leftNeighbor <- function(k, lx, ly)
  k - 1 + lx * (k %% lx == 1)
rightNeighbor <- function(k, lx, ly)
  k + 1 - lx * (k %% lx == 0)
bottomNeighbor <- function(k, lx, ly)
  k - lx + lx * ly * (k <= lx)
topNeighbor <- function(k, lx, ly)
  k + lx - lx * ly * (k > lx*(ly-1))

#######################################################################
metropSweep <- function(s, ngb, beta, e.minus4beta, e.minus8beta) {
  
  # Function to carry out one sweep of the single-spin-flip Metropolis
  # algorithm for the Ising model on a lattice s. The adjacency list is
  # passed as argument ngb. It returns the lattice after one sweep.
  
  lx <- nrow(s)                            # Obtain lattice dimensions.
  ly <- ncol(s)
  for (j in seq_len(lx * ly)) {
    flip <- sample(lx*ly, 1)                   # Index of spin to flip.
    de <-                        # Change in energy if spin is flipped.
      2 * sum(s[ngb[[flip]]]) * s[flip]
    if (de <= 0 || (de == 4 && e.minus4beta > runif(1))    # Metropolis
        || (de == 8 && e.minus8beta > runif(1)))           # algorithm.
      s[flip] <- -s[flip]              # If we get here, flip the spin.
  }
  return(s)
}

#######################################################################
isingTransient <- function(beta, lx, ly, ngb, max.sweep = 10000) {
  
  # Function to obtain two equilibrated lattices.
  # Input: beta - inverse temperature,
  #        lx, ly: dimensions of the lattice.
  #        max.sweep - if no equilibration until this time, exit with
  #                    a value "not equilibrated".
  # Return value: a list containing a string (equilibrated?), the final
  #        spins and the time series of the magnetization (one data 
  #        point per sweep).
  
  e.minus4beta <- exp(-4*beta)  # The only two Boltzmann ratios to show
  e.minus8beta <- exp(-8*beta)  # up in the Metropolis algorithm.
  
  ## Initialize two lattices: one starting from a random (T=Inf)     ##
  ## (T=Inf), another from a perfectly aligned (T=0) configuration.  ##
  if (beta < 2.2) {           # If we are clearly below the phase 
    prob = c(0.6, 0.4)        # transition, we accelerate equilibration
  } else {                    # by breaking the symmetry. Otherwise
    prob = c(0.5, 0.5)        # 50-50 up- and down-spins are OK.
  }
  s.inf <-                                              # Random spins.
    matrix(sample(c(-1, 1), lx*ly, replace = TRUE, 
                  prob = prob), 
           nrow = lx)
  s.0 <- matrix(1, nrow = lx, ncol = ly)
  m.inf <- m.0 <-                      # Initialize vector to store the
    numeric(max.sweep)                 # magnetization time series.
  
  ###################### Now the MCMC simulation. #####################
  for (i in seq_len(max.sweep)) {
    s.inf <-                          # Carry out Metropolis algorithm.
      metropSweep(s.inf, ngb, beta,e.minus4beta, e.minus8beta)       
    s.0 <- metropSweep(s.0, ngb, beta, e.minus4beta, e.minus8beta)           
    m.inf[i] <- sum(s.inf) / (lx * ly)      # Record the magnetization.
    m.0[i] <- sum(s.0) / (lx*ly)
    if (i %% 100 == 0) {
      string1 <- sprintf("In the transient. Sweep number %d.", i)
      string2 <- sprintf("|m.inf| = %.3f, |m.0| = %.3f.\n", 
                         abs(m.inf[i]), abs(m.0[i]))
      cat(string1, string2)
    }
    if (abs(m.inf[i]) >= abs(m.0[i])) {      # Have the systems reached
      m.inf <- m.inf[seq_len(i)]             # equilibrium?
      m.0 <- m.0[seq_len(i)]
      # showlatt(s.inf)                                  # Visual output.
      # showlatt(s.0)
      return(list(eq = "equilibrated", latt.inf = s.inf, m.inf = m.inf,
                  latt.0 = s.0, m.0 = m.0))
    }
  }

  #### If we get here, then the Markov chain has not equilibrated. ####
  # showlatt(s.inf)                                      # Visual output.
  # showlatt(s.0)
  return(list(eq = "not equilibrated", latt.inf = s.inf, m.inf = m.inf,
              latt.0 = s.0, m.0 = m.0))
}

#######################################################################
isingSteady <- function(beta, s, ngb, sweep) {
  
  # Function to perform measurements of the magnetization in the steady
  # state.  
  # Input: beta - inverse temperature, 
  #        s - initial spins as an lx*ly matrix,
  #        sweep - mean number of times a spin gets updated.
  # Return value: a list containing the final spins and a time series
  #        of the magnetization (one data point per sweep).
  
  e.minus4beta <- exp(-4*beta)  # The only two Boltzmann ratios to show
  e.minus8beta <- exp(-8*beta)  # up in the Metropolis algorithm.
  lx <- nrow(s)                            # Obtain lattice dimensions.
  ly <- ncol(s)
  mTimeSeries <-                       # Initialize vector to store the
    numeric(sweep)                     # magnetization time series.
  
  ###################### Now the MCMC simulation. #####################
  for (i in seq_len(sweep)) {
    if (i %% 100 == 0)
       cat("In the steady state. Sweep number", i, "\n")
    s <-                              # Carry out Metropolis algorithm.
      metropSweep(s, ngb, beta, e.minus4beta, e.minus8beta)     
    mTimeSeries[i] <- sum(s) / (lx * ly)    # Record the magnetization.
  }
  # showlatt(s)                                          # Visual output.
  return(list(latt = s, mTimeSeries = mTimeSeries))
}

##################### Setup the lattice geometry. #####################
lx <- 100                         # Lattice dimensions are lx times ly.
ly <- 100
ngb <- list(lx*ly)                      # Setup the list of neighbours.
for (k in seq_len(lx*ly))
  ngb[[k]] <- c(leftNeighbor(k, lx, ly), rightNeighbor(k, lx, ly),
                bottomNeighbor(k, lx, ly), topNeighbor(k, lx, ly))

################### Here comes the actual simulation. #################




