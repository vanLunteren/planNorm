sim_sample_pow <- function (sd = seq(4, 42, 1.5), delta = 0, alpha = 0.05, beta = 0.2, 
                            Delta = 4, prop = c(0.5, 0.7), simu = 10000, test = 1, 
                            adj = F, regel = F, nbound = 300, nmin = 50){
  
  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected." )
  }
  
  lapply(sd, sig <- function (sd){
    if (test == 1){
      N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
    } else {
      N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
    }
    
    mapply(function(prop){
      n1 <- max(ceiling(prop * N0), nmin)
      if (is.numeric(nbound)){
        n1 <- min(n1, nbound)
      }
      
      calc <- replicate(simu, sim_calc(n1 = n1, delta = delta, sd = sd, alpha = alpha,
                          beta = beta, Delta = Delta, N0 = N0, nbound = nbound, test = test))
      
      return( c(mean(calc[1, ]), mean(calc[2, ])) )
    }, prop)
  })
}

sim_calc <- function (n1 = 65, delta = 0, sd = 4, alpha = 0.05, beta = 0.2, Delta = 4, N0 = 100, 
                      nbound = 300, test = 1){
  X1_h0 <- rnorm(n = ceiling(n1 / 2), mean = delta, sd = sd)
  Y1_h0 <- rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd)
  
  X1_h1 <- rnorm(n = ceiling(n1 / 2), mean = Delta, sd = sd)
  Y1_h1 <- rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd)
  
  if (adj == F){
    S_h0 <- sqrt(var(c(X1_h0, Y1_h0)))
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)))
  } else {
    S_h0 <- sqrt(var(c(X1_h0, Y1_h0)) - n1 / (2 * (n1 - 1)) * delta)
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)) - n1 / (2 * (n1 - 1)) * Delta) 
  }
  
  if (test == 1){
    N_h0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * S_h0 / (Delta - delta)^2)
    N_h1 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * S_h1 / (Delta - delta)^2)
  } else {
    N_h0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * S_h0 / Delta^2)
    N_h1 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * S_h1 / Delta^2)
  }
  
  if (regel == "WB"){
    N_h0 <- max(N0, N_h0)
    N_h1 <- max(N0, N_h1)
  }
  
  if (is.numeric(nbound)){
    N_h0 <- min(N_h0, nbound)
    N_h1 <- min(N_h1, nbound)
  }
  
  n2_h1 <- max(n1, N_h1) - n1
  
  X2_h1 <- rnorm(n = ceiling(n2_h1 / 2), mean = Delta, sd = sd)
  Y2_h1 <- rnorm(n = ceiling(n2_h1 / 2), mean = 0, sd = sd)
  
  X_h1 <- c(X1_h1, X2_h1)
  Y_h1 <- c(Y1_h1, Y2_h1)
  
  S_gepoolt_h1 <- sqrt(((length(X_h1) - 1) * var(X_h1) + (length(Y_h1) - 1) * var(Y_h1) )/ 
                         (length(X_h1) + length(Y_h1) - 2))
  if (length(X_h1) == 1 & length(Y_h1) == 1){
    S_gepoolt_h1 <- sqrt(var(c(X_h1, Y_h1)))
  }
  
  T_h1 <- sqrt((length(X_h1) * length(Y_h1)) / (length(X_h1) + length(Y_h1))) * 
    (mean(X_h1) - mean(Y_h1) - delta) / S_gepoolt_h1
  return( c(max(n1, N_h0), test_h0(test, T_h1, length(X_h1), length(Y_h1), alpha, delta) ))
}
