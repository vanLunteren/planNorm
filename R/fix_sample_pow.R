fix_sample_pow <- function (sd_ber, delta = 0, Delta = 4, sd, test = 1, alpha = 0.05, beta = 0.2, 
                            simu = 10000){
  
  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected.")
  }
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  mapply(sig <- function (sd_ber){
    if (test == 1){
      N <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd_ber^2 / (Delta - delta)^2)
    } else {
      N <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd_ber^2 / Delta^2)
    }
  
    abg <- replicate(simu, fix_calc(N0, sd_ber, delta, Delta, test, alpha, beta))
    
    return (c(N, mean(abg)))
  }, sd_ber)
}

fix_calc <- function(N0 = 65, sd_ber, delta = 0, Delta = 4, test = 1, alpha = 0.05, beta = 0.2){
  X <- rnorm(n = N0 / 2, mean = Delta, sd = sd_ber)
  Y <- rnorm(n = N0 / 2, mean = 0, sd = sd_ber)
  
  S_gepoolt <- sqrt(((length(X) - 1) * var(X) + (length(Y) - 1) * var(Y) )/ (length(X) + length(Y) - 2))
  if (length(X) == 1 & length(Y) == 1){
    S_gepoolt <- sqrt(var(c(X, Y)))
  }
  
  T <- sqrt((length(X) * length(Y)) / (length(X) + length(Y))) * (mean(X) - mean(Y) - delta) / S_gepoolt
  return (test_h0(test, T, length(X), length(Y), alpha, delta))
}
