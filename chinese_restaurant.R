# Function for sampling from Chinese Restaurant Process
DP_CRP = function(N, alpha) {
  
  C = rep(NA, N)  # Customer table allocation
  K = 1           # Initial number of tables
  C[1] = 1        # First customer sits at table 1
  nk = 1          # Update number of people at table 1
  
  for (n in 2:N) {
    
    # Find number of tables/clusters
    K = max(which(nk != 0))
    
    # Weights
    w = rep(NA, K + 1)
    w = nk[1:K] / (alpha + n - 1)
    w[K + 1] = alpha / (alpha + n - 1)
    
    # Sample with weights
    k = sample(1:(K + 1), 1, prob = w)  # Table k sampled
    C[n] = k # ith customer sits at table k
    
    # Update number customers at each table/cluster
    nk = as.numeric(table(C[1:n]))
    
  }
  
  # Output vector of table numbers for each of the N customers
  return(C)
  
}

N = 1000        # Number of customers
alpha = 10      # Concentration parameter

# Sample from Chinese Restaurant Process
C = DP_CRP(N, alpha)
table(C)
hist(C, nclass = length(C), xlab = "Table Number")

# Expected number clusters O(alpha*log(N))
alpha * log(N)  # On this order of magnitude



