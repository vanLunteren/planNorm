#' @title 
#' @description 
#' @usage 
#' sim_sample_pow(sd_ber, delta = 0, Delta, sd, test = 1,  alpha = 0.05, beta = 0.2, 
#'                prop = c(0.5, 0.7), adj = F, regel = F, nbound = 500, simu = 10000)
#' @param sd_ber
#' Sequence of numbers. Interval of the actual standard deviation in the data. 
#' With regard to this interval, the sample size and the power are computed.
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
#' Number. Assumed standard deviation of the data. 
#' Used to calculate the originally planned number of cases.
#' 
#' @param test 
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test)
#' or two-sided (Test for difference).
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
#' @param prop
#' Number or vector of numbers. Timing of the internal pilot study depending on the originally planned sample size.
#' If a vector is passed, all timings within a plot are displayed.
#' 
#' @param adj
#' Logical. Should the one-sample variance, calculated in the internal pilot study, be adjusted?
#' 
#' @param regel
#' Logical. Should the sample size adjustment rule be applied by Wittes and Brittain?
#' 
#' @param nbound
#' Number. Upper limit of the sample size.
#' Attention: Only if you choose nbound can a suitable standard deviation range for the plots 
#' be calculated automatically. 
#' If no nbound are defined then a standard deviation range must be chosen (see sd_ber).
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
sim_sample_pow <- function (sd_ber, delta = 0, Delta, sd, test = 1,  alpha = 0.05, beta = 0.2, 
                             prop = c(0.5, 0.7), adj = F, regel = F, nbound = 500, simu = 10000){
  
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
      
      calc <- replicate(simu, sim_calc(n1 = n1, N0 = N0, sd_ber = sd_ber, delta = delta, Delta = Delta,
                                       test = test, alpha = alpha, beta = beta, adj = adj, regel = regel,
                                       nbound = nbound))
      
      return (c(quantile(calc[1, ],c(0, 0.25, 0.5, 0.75, 1)), mean(calc[2, ])))
    }, prop)
  })
}

sim_calc <- function (n1, N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2, adj = F,
                      regel = F, nbound = 500){
  
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
