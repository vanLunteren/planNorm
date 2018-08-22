#' @title
#' Simulation of the sample size and power
#'
#' @description
#' This is an auxiliary function.
#' It calculates the sample size and power of the design with an internal pilot study for different
#' standard deviations and for multiple timings of the internal pilot studies.
#' The originally planned sample size is calculated on the basis of an assumed standard deviation.
#' A distinction is made between one-sided and two-sided tests.
#'
#' @usage
#' sim_sample_pow(sd_ber, delta = 0, Delta, sd, test = 1,  alpha = 0.05, beta = 0.2,
#'                prop = c(0.5, 0.7), adj = F, regel = F, nbound = 500, simu = 10000)
#' @param sd_ber
#' Sequence of numbers. Interval of the actual standard deviation in the data. \cr
#' With regard to this interval, the sample size and the power are computed.
#'
#' @param delta
#' Number. Expectation difference of two samples.\cr
#' If you select a Test for superiority/ difference then select 'delta = 0'.\cr
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.\cr
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically\cr
#' be applied.\cr
#' If not specified, delta is set to 0.
#'
#' @param Delta
#' Number. Relevant difference of expected values in the alternative hypothesis.
#'
#' @param sd
#' Number. Assumed standard deviation of the data. \cr
#' Used to calculate the originally planned number of cases.
#'
#' @param test
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test)\cr
#' or two-sided (Test for difference)\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y > 0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Tweo-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0\cr
#' Attention: Choice of delta. (see \code{delta})\cr
#' If not specified, the one-Sided Test (Superiority/ Non-Inferiority Test) is used.
#'
#' @param alpha
#' Number. Desired alpha-level of the test.\cr
#' If not specified, alpha is set to 0.05.
#'
#' @param beta
#' Number. Acceptable beta error of the test.\cr
#' If not specified, beta is set to 0.2.
#'
#' @param prop
#' Number or vector of numbers. Timing of the internal pilot study depending on the originally planned sample size.\cr
#' If a vector is passed, all timings within a plot are displayed.
#'
#' @param adj
#' Logical. Should the one-sample variance, calculated in the internal pilot study, be adjusted?
#'
#' @param regel
#' Logical. Should the sample size adjustment rule be applied by Wittes and Brittain?
#'
#' @param nbound
#' Number. Upper limit of the sample size.\cr
#' Attention: Only if you choose nbound can a suitable standard deviation range for the plots \cr
#' be calculated automatically. \cr
#' If no nbound are defined then a standard deviation range must be chosen (see sd_ber).
#'
#' @param simu
#' Number. How many simulations should be performed?\cr
#' If not specified, simu is set to 10000.
#'
#' @return
#' This function only creates the sample size and power values for multiple standard deviations.\cr
#' The output is used in the function \code{sample_pow} to visualize the sample size and power depending \cr
#' on the timing of the internal pilot studies.
#'
#' @author
#' Csilla van Lunteren
#'
#' @seealso
#' \code{\link{sim_calc}}
#'
#' @import stats
#'
#' @export
#'
sim_sample_pow <- function (sd_ber, delta = 0, Delta, sd, test = 1,  alpha = 0.05, beta = 0.2,
                             prop = c(0.5, 0.7), adj = F, regel = F, nbound = 500, simu = 10000){

  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected.")
  }

  if (test == 1){
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * sd^2 / Delta^2)
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

      return (c(stats::quantile(calc[1, ],c(0, 0.25, 0.5, 0.75, 1)), mean(calc[2, ])))
    }, prop)
  })
}

#' @title
#' Sample size and decision regarding test statistics
#'
#' @description
#' This is an auxiliary function of 'sim_sample_pow'.
#'
#' @usage
#' sim_calc(n1, N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2,
#'          adj = F, regel = F, nbound = 500)
#'
#' @param n1
#' Number. Size of the internal pilot study.
#'
#' @param N0
#' Number. Size of the originally planned sample size.
#'
#' @param sd_ber
#' Number. Actual standard deviation in the data.
#'
#' @param delta
#' Number. Expectation difference of two samples.\cr
#' If you select a Test for superiority/ difference then select 'delta = 0'.\cr
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.\cr
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically\cr
#' be applied.\cr
#' If not specified, delta is set to 0.
#'
#' @param Delta
#' Number. Relevant difference of expected values in the alternative hypothesis.
#'
#' @param test
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test)\cr
#' or two-sided (Test for difference).\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y > 0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Tweo-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0\cr
#' Attention: Choice of delta. (see delta)\cr
#' If not specified, the one-Sided Test (Superiority/ Non-Inferiority Test) is used.
#'
#' @param alpha
#' Number. Desired alpha-level of the test.\cr
#' If not specified, alpha is set to 0.05.
#'
#' @param beta
#' Number. Acceptable beta error of the test.\cr
#' If not specified, beta is set to 0.2.
#'
#' @param adj
#' Logical. Should the one-sample variance, calculated in the internal pilot study, be adjusted?
#'
#' @param regel
#' Logical. Should the sample size adjustment rule be applied by Wittes and Brittain?
#'
#' @param nbound
#' Number. Upper limit of the sample size.\cr
#' Attention: Only if you choose nbound can a suitable standard deviation range for the plots\cr
#' be calculated automatically.\cr
#' If no nbound are defined then a standard deviation range must be chosen (see sd_ber).
#'
#' @return
#' This function returns a value if the simulation study rejects (1) or can not reject (0) the null hypothesis.
#'
#' @author
#' Csilla van Lunteren
#'
#' @seealso
#' \code{\link{test_h0}}
#'
#' @import stats
#'
#' @export
#'


sim_calc <- function (n1, N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2, adj = F,
                      regel = F, nbound = 500){

  X1_h0 <- stats::rnorm(n = ceiling(n1 / 2), mean = delta, sd = sd_ber)
  Y1_h0 <- stats::rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd_ber)

  X1_h1 <- stats::rnorm(n = ceiling(n1 / 2), mean = Delta, sd = sd_ber)
  Y1_h1 <- stats::rnorm(n = ceiling(n1 / 2), mean = 0, sd = sd_ber)

  if (adj == F){
    S_h0 <- sqrt(stats::var(c(X1_h0, Y1_h0)))
    S_h1 <- sqrt(stats::var(c(X1_h1, Y1_h1)))
  } else {
    S_h0 <- sqrt(stats::var(c(X1_h0, Y1_h0)) - n1 / (2 * (n1 - 1)) * delta)
    S_h1 <- sqrt(stats::var(c(X1_h1, Y1_h1)) - n1 / (2 * (n1 - 1)) * Delta)
  }

  if (test == 1){
    N_h0 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * S_h0^2 / (Delta - delta)^2)
    N_h1 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * S_h1^2 / (Delta - delta)^2)
  } else {
    N_h0 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * S_h0^2 / Delta^2)
    N_h1 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * S_h1^2 / Delta^2)
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

  X2_h1 <- stats::rnorm(n = ceiling(n2_h1 / 2), mean = Delta, sd = sd_ber)
  Y2_h1 <- stats::rnorm(n = ceiling(n2_h1 / 2), mean = 0, sd = sd_ber)

  X_h1 <- c(X1_h1, X2_h1)
  Y_h1 <- c(Y1_h1, Y2_h1)

  S_gepoolt_h1 <- sqrt(((length(X_h1) - 1) * stats::var(X_h1) + (length(Y_h1) - 1) * stats::var(Y_h1) )/
                         (length(X_h1) + length(Y_h1) - 2))
  if (length(X_h1) == 1 & length(Y_h1) == 1){
    S_gepoolt_h1 <- sqrt(stats::var(c(X_h1, Y_h1)))
  }

  T_h1 <- sqrt((length(X_h1) * length(Y_h1)) / (length(X_h1) + length(Y_h1))) *
            (mean(X_h1) - mean(Y_h1) - delta) / S_gepoolt_h1

  return (c(max(n1, N_h0), test_h0(test = test, T = T_h1, n1 = length(X_h1), n2 = length(Y_h1), alpha = alpha,
                                   delta = delta)))
}
