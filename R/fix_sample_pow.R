#' @title 
#' @description 
#' @usage 
#' fix_sample_pow(sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, simu = 10000)
#' @param sd_ber
#' 
#' @param delta
#' Number. Expectation difference of two samples.
#' If you select a Test for superiority/ difference then select 'delta = 0'.
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically 
#' be applied.
#' If not specified, delta is set to 0.
#' 
#' @param Delta
#' Number. Relevant difference of expected values in the alternative hypothesis.
#' 
#' @param sd
#' 
#' @param test
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test) (test = 1)
#' or two-sided (Test for difference) (test = 2).
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y >0
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta
#' Tweo-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0
#' Attention: Choice of delta. (see delta)
#' If not specified, the one-Sided Test (Superiority/ Non-Inferiority Test) is used.
#' 
#' @param alpha
#' Number. Desired alpha-level of the test.
#' If not specified, alpha is set to 0.05.
#' 
#' @param beta
#' Number. Acceptable beta error of the test.
#' If not specified, beta is set to 0.2.
#' 
#' @param simu
#' Number. How many simulations should be performed?
#' If not specified, simu is set to 10000.
#' 
#' @details 
#' @return 
#' 
#' @author
#' Csilla van Lunteren 
#' @export
#' 
fix_sample_pow <- function (sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, 
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

fix_calc <- function(N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2){
  X <- rnorm(n = N0 / 2, mean = Delta, sd = sd_ber)
  Y <- rnorm(n = N0 / 2, mean = 0, sd = sd_ber)
  
  S_gepoolt <- sqrt(((length(X) - 1) * var(X) + (length(Y) - 1) * var(Y) )/ (length(X) + length(Y) - 2))
  if (length(X) == 1 & length(Y) == 1){
    S_gepoolt <- sqrt(var(c(X, Y)))
  }
  
  T <- sqrt((length(X) * length(Y)) / (length(X) + length(Y))) * (mean(X) - mean(Y) - delta) / S_gepoolt
  return (test_h0(test, T, length(X), length(Y), alpha, delta))
}
