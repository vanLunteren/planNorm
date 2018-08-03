fix_sample_pow <- function (sd = seq(4, 42, 1.5), delta = 0, alpha = 0.05, beta = 0.2, Delta = 4,
                            simu = 10000, test = 1, nbound = 300, nmin = 65){
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
    N0 <- max(N0, nmin)
    if (is.numeric(nbound)){
      N0 <- min(N0, nbound)
    }
    
    abg <- replicate(simu, fix_calc(N0, delta, sd, alpha, beta, Delta, test))
    
    return (c(N0, mean(abg)))
  })
}

sim_calc <- function(N0 = 65, delta = 0, sd = 4, alpha = 0.05, beta = 0.2, Delta = 4, test = 1){
  X <- rnorm(n = N0 / 2, mean = Delta, sd = sd)
  Y <- rnorm(n = N0 / 2, mean = 0, sd = sd)
  
  S_gepoolt <- sqrt(((length(X) - 1) * var(X) + (length(Y) - 1) * var(Y) )/ (length(X) + length(Y) - 2))
  if (length(X) == 1 & length(Y) == 1){
    S_gepoolt <- sqrt(var(c(X, Y)))
  }
  
  T <- sqrt((length(X) * length(Y)) / (length(X) + length(Y))) * (mean(X) - mean(Y) - delta) / S_gepoolt
  return (test_h0(test, T, length(X), length(Y), alpha, delta))
}