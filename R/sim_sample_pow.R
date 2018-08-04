sim_sample_pow <- function (sd_ber, delta = 0, Delta = 4, sd, test = 1,  alpha = 0.05, beta = 0.2, 
                             prop = c(0.5, 0.7), adj = F, regel = F, nbound = 300, simu = 10000){
  
  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected.")
  }
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  lapply(sd_ber, sig <- function (sd_ber){
    mapply(function (prop){
      n1 <- ceiling(prop * N0)
      if (is.numeric(nbound)){
        n1 <- min(n1, nbound)
      }
      
      calc <- replicate(simu, sim_calc(n1 = n1, N0 = N0, sd_ber = sd_ber, delta = delta, 
                        Delta = Delta, test = test, alpha = alpha, beta = beta, adj = adj, 
                        regel = regel, nbound = nbound))
      
      return (c(mean(calc[1, ]), mean(calc[2, ])))
    }, prop)
  })
}

sim_calc <- function (n1 = 65, N0 = 100, sd_ber, delta = 0, Delta = 4, test = 1, alpha = 0.05, 
                      beta = 0.2, adj = F, regel = F, nbound = 300){
  
  X1_h0 <- rnorm(n = ceiling(n1 / 2), mean = delta, sd = sd_ber)
  Y1_h0 <- rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd_ber)
  
  X1_h1 <- rnorm(n = ceiling(n1 / 2), mean = Delta, sd = sd_ber)
  Y1_h1 <- rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd_ber)
  
  if (adj == F){
    S_h0 <- sqrt(var(c(X1_h0, Y1_h0)))
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)))
  } else {
    S_h0 <- sqrt(var(c(X1_h0, Y1_h0)) - n1 / (2 * (n1 - 1)) * delta)
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)) - n1 / (2 * (n1 - 1)) * Delta) 
  }
  
  if (test == 1){
    N_h0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * S_h0^2 / (Delta - delta)^2)
    N_h1 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * S_h1^2 / (Delta - delta)^2)
  } else {
    N_h0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * S_h0^2 / Delta^2)
    N_h1 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * S_h1^2 / Delta^2)
  }
  
  if (regel == "WB"){
    N_h0 <- max(N0, N_h0)
    N_h1 <- max(N0, N_h1)
  }
  
  if (is.numeric(nbound)){
    N_h0 <- min(N_h0, nbound)
    N_h1 <- min(N_h1, nbound)
  }
  
  n2_h1 <- max(0, N_h1 - n1) 
  
  X2_h1 <- rnorm(n = ceiling(n2_h1 / 2), mean = Delta, sd = sd_ber)
  Y2_h1 <- rnorm(n = ceiling(n2_h1 / 2), mean = 0, sd = sd_ber)
  
  X_h1 <- c(X1_h1, X2_h1)
  Y_h1 <- c(Y1_h1, Y2_h1)
  
  S_gepoolt_h1 <- sqrt(((length(X_h1) - 1) * var(X_h1) + (length(Y_h1) - 1) * var(Y_h1) )/ 
                         (length(X_h1) + length(Y_h1) - 2))
  if (length(X_h1) == 1 & length(Y_h1) == 1){
    S_gepoolt_h1 <- sqrt(var(c(X_h1, Y_h1)))
  }
  
  T_h1 <- sqrt((length(X_h1) * length(Y_h1)) / (length(X_h1) + length(Y_h1))) * 
    (mean(X_h1) - mean(Y_h1) - delta) / S_gepoolt_h1
  
  return (c(max(n1, N_h0), test_h0(test, T_h1, length(X_h1), length(Y_h1), alpha, delta)))
}
