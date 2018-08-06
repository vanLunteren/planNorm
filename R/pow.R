#' @title 
#' @description 
#' @usage 
#' pow(delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = seq(0.1, 1, 0.05), 
#'     adj = F, regel = F, nbound = 500, simu = 10000)
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
#' @param prop
#' 
#' @param adj
#' 
#' @param regel
#' 
#' @param nbound
#' Number. Upper limit of the sample size.
#' Attention: only if you choose nbound and nmin can a suitable standard deviation range for the plots be calculated automatically. If no nbound and / or nmin are defined then a standard deviation range must be chosen.
#' simu = 10000, test = 1, nbound = 500, nmin = 10)
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
pow <- function (delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = seq(0.1, 1, 0.05), 
                     adj = F, regel = F, nbound = 500, simu = 10000){
  

  
  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected." )
  }
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  lapply(Delta, Del <- function (Delta){
    mapply(function(prop){
      n1 <- ceiling(prop * N0)
      if (is.numeric(nbound)){
        n1 <- min(n1, nbound)
      }
      
      calc <- replicate(simu, sim_p_calc(n1 = n1, delta = delta, Delta = Delta, sd = sd, test = test,
                                         alpha = alpha, beta = beta, N0 = N0, nbound = nbound))
      
      return (mean(calc))
    }, prop)
  })
}


sim_p_calc <- function(n1, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, N0, nbound = 500){
  
  X1_h1 <- rnorm(n = n1 / 2, mean = Delta, sd = sd)
  Y1_h1 <- rnorm(n = n1 / 2, mean = 0, sd = sd)
  
  if (adj == F){
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)))
  } else {
    S_h1 <- sqrt(var(c(X1_h1, Y1_h1)) - n1 / (2 * (n1 - 1)) * Delta)
  }
  
  if (test == 1){
    N_h1 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * S_h1^2 / (Delta - delta)^2)
  } else {
    N_h1 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * S_h1^2 / Delta^2)
  }
  
  if (regel == "WB"){
    N_h1 <- max(N0, N_h1)
  }
  if (is.numeric(nbound)){
    N_h1 <- min(N_h1, nbound)
  }
  
  n2_h1 <- max(0, N_h1 - n1)
  
  X2_h1 <- rnorm(n = n2_h1 / 2, mean = Delta, sd = sd)
  Y2_h1 <- rnorm(n = n2_h1 / 2, mean = 0, sd = sd)
  
  X_h1 <- c(X1_h1, X2_h1)
  Y_h1 <- c(Y1_h1, Y2_h1)
  
  S_gepoolt_h1 <- sqrt(((length(X_h1) - 1) * var(X_h1) + (length(Y_h1) - 1) * var(Y_h1) ) / 
                         (length(X_h1) + length(Y_h1) - 2))
  
  if (length(X_h1) == 1 & length(Y_h1) == 1){
    S_gepoolt_h1 <- sqrt(var(c(X_h1, Y_h1)))
  }
  
  T <- sqrt((length(X_h1) * length(Y_h1)) / (length(X_h1) + length(Y_h1))) * 
        (mean(X_h1) - mean(Y_h1) - delta) / S_gepoolt_h1
  return (test_h0(test, T, length(X_h1), length(Y_h1), alpha, delta))
}
