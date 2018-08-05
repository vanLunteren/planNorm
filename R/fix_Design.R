#' @title 
#' Generates plots for the sample size and power in the fixed sample design.
#' 
#' @description
#' Generates plots sample size/ power as a function of the standard deviation in the fixed sample design.
#' Attention: The sample size of both samples are the same size.
#' 
#' @usage 
#' fix_D(delta = 0, Delta, alpha = 0.05, beta = 0.2, simu = 10000, test = 1, nbound = 500, nmin = 10)
#' 
#' @param delta
#' Number. Difference of the expectations of the two samples.
#' If you select a Test for superiority/ difference then select 'delta = 0'.
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically be applied.
#' If not specified, delta is set do 0.
#' 
#' @param Delta
#' Number. Relevant difference of expected values in the alternative hypothesis.
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
#' @param test
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test) (test = 1)
#' or two-sided (Test for difference) (test = 2).
#' Attention: Choice of delta. (see delta)
#' If not specified, the one-Sided Test (Superiority/ Non-Inferiority Test) is used.
#' 
#' @param nbound
#' Number. Upper limit of the sample size.
#' Attention: only if you choose nbound and nmin can a suitable standard deviation range for the plots be calculated automatically. If no nbound and / or nmin are defined then a standard deviation range must be chosen.
#' simu = 10000, test = 1, nbound = 500, nmin = 10)
#' 
#' @details 
#' 
#' 
#' @return 
#'  
#' 
#' @author 
#' Csilla van Lunteren
#' 
#' @export
#' 
fix_D <- function (sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, simu = 10000){
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  text <- paste("Settings: \n", "\n delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                ", test = one-sided,", "\n alpha = ", alpha, ", beta = ", beta, ",", 
                "\n N0 = ", N0)
  
  layout(matrix(c(1, 1, 2, 2, 2, 1, 1, 3, 3, 3), 5, 2))

  sd_min <- (1 / 2 * sqrt(N0 * min(prop)) * 1 / (qnorm(1 - alpha) + qnorm(0.95)) * (Delta - delta)) 
  sd_max <- (1 / 2 * sqrt(nbound) * 1 / (qnorm(1 - alpha) + qnorm(0.18)) * (Delta - delta)) 
  sd_ber <- seq(sd_min, sd_max, (sd_max - sd_min) / 25)

  N_pow <- fix_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test, 
                          alpha = alpha, beta = beta, simu = simu)
  
  plot(1, 2, axes = F, type = "n", xlab = "", ylab = "")
  mtext(text, side = 1, cex = 0.8)
  
  ##Fallzahl
  plot(sd_ber, N_pow[1, ], type = "l", main = "Sample Size \n standard deviation", 
       xlab = "standard deviation", ylab = "Sample Size")
  
  
  ##Power
  plot(sd_ber, N_pow[2, ], c(sd_min, sd_max), ylim = c(0, 1), type = "l", 
       main = "Power \n standard deviation", xlab = "standard deviation", ylab = "Power")
  abline(h = 0.8, lty = 2, col = "grey")
  abline(h = 0.2, lty = 2, col = "grey")
}
