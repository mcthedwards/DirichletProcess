# Stick-breaking representation of Dirichlet process
DP_SB = function(L, M = 1, G0 = runif, ...) {
  
  # L: Truncation parameter
  # M: Shape parameter for breaks
  # G0: Base distribution (Default: Uniform)
  # ...: Additional arguments in G0. 
  #      e.g., min and max for runif, mean and sd for rnorm, etc
  
  # Sample V
  V = rbeta(L, 1, M)
  
  # Construct stick-breaking probabilities (p from V)
  p = rep(NA, L)
  p[1] = V[1]
  for (i in 2:L) {
    p[i] = prod(1 - V[1:(i - 1)]) * V[i]
  }
  p = c(1 - sum(p), p)  # Attach p0 as p[1]
  
  # Sample U from base measure G0
  U = G0(L + 1, ...)
    
  # Output
  return(list(p = p,
              V = V,
              U = U))
  
}

# Examples
L = 20
M = 1  
DP_SB(L, M, runif, min = 0, max = 1)      # Base measure: U[0, 1]
DP_SB(L, M, rnorm, mean = 0, sd = 10)     # Base measure: N(0, 100)
DP_SB(L, M, rgamma, shape = 1, rate = 1)  # Base measure: Gamma(1, 1)

# Play around with L, M, G and plot
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0)) 
# H = 1
for (i in 1:4) {
  G = DP_SB(10000, 1, rnorm)   
  plot(NULL, xlim = range(G$U), ylim = range(G$p),
       axes = TRUE, xlab = "U", ylab = "p")
  segments(G$U, 0, G$U, G$p)
}
# H = 10
for (i in 1:4) {
  G = DP_SB(10000, 10, rnorm)   
  plot(NULL, xlim = range(G$U), ylim = range(G$p),
       axes = TRUE, xlab = "U", ylab = "p")
  segments(G$U, 0, G$U, G$p)
}
# H = 100
for (i in 1:4) {
  G = DP_SB(10000, 100, rnorm)   
  plot(NULL, xlim = range(G$U), ylim = range(G$p),
       axes = TRUE, xlab = "U", ylab = "p")
  segments(G$U, 0, G$U, G$p)
}
# H = 1000
for (i in 1:4) {
  G = DP_SB(10000, 1000, rnorm)   
  plot(NULL, xlim = range(G$U), ylim = range(G$p),
       axes = TRUE, xlab = "U", ylab = "p")
  segments(G$U, 0, G$U, G$p)
}
par(mfrow = c(1, 1)) 



