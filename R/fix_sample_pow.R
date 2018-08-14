#' @title
#' Simulation of the sample size and the power
#'
#' @description
#' This is an auxiliary function.
#' It calculates the sample size and the simulated power of a fixed-sample design.
#' The originally planned sample size is calculated on the basis of an assumed standard deviation.
#' A distinction is made between one-sided and two-sided tests.
#'
#' @usage
#' fix_sample_pow(sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2,
#'                simu = 10000)
#'
#' @param sd_ber
#' Vector of numbers. Interval of an area for the actual standard deviation in the data.
#'
#' @param delta
#' Number. Expectation difference of two samples.\cr
#' If you select a Test for superiority/ difference then select 'delta = 0'.\cr
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.\cr
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically \cr
#' be applied.\cr
#' If not specified, delta is set to 0.\cr
#'
#' @param Delta
#' Number. Relevant difference of expected values in the alternative hypothesis.
#'
#' @param sd
#' Number. Assumed standard deviation of the data. \cr
#' Used to calculate the originally planned sample size.
#'
#' @param test
#' Number. What type of hypothesis test should be performed, one-sided (Superiority/ Non-Inferiority test)\cr
#' or two-sided (Test for difference).\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y >0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Two-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0\cr
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
#' @param simu
#' Number. How many simulations should be performed?\cr
#' If not specified, simu is set to 10000.
#'
#' @return
#' This function only creates the sample size and Power values for multiple actual standard deviation values.\cr
#' The output is used in the function sample_pow to visualize the sample size and the power.
#'
#' @author
#' Csilla van Lunteren
#'
#'  @seealso
#' \code{\link{fix_calc}}
#'
#' @import stats
#' @export
#'
fix_sample_pow <- function (sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2,
                            simu = 10000){

  set.seed(12021994)
  if (delta != 0 & test == 2){
    stop("When choosing a two-sided hypothesis test (test for differences), delta = 0 must be selected.")
  }

  if (test == 1){
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }

  mapply(sig <- function (sd_ber){
    if (test == 1){
      N <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * sd_ber^2 / (Delta - delta)^2)
    } else {
      N <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * sd_ber^2 / Delta^2)
    }

    abg <- replicate(simu, fix_calc(N0, sd_ber, delta, Delta, test, alpha, beta))

    return (c(N, mean(abg)))
  }, sd_ber)
}



#' @title
#' Decision regarding test statistics (Design with fix sample size)
#'
#' @description
#' This is an auxiliary function of 'fix_sample_pow'.
#'
#' @usage
#' fix_calc(N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2)
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
#' Number. What type of hypothesis test should be performed, one-sided (Superiority/ Non-Inferiority test)\cr
#' or two-sided (Test for difference).\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y > 0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Two-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0\cr
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
#' @return
#' This function returns a value if the simulation study rejects (1) or can not reject (0) the null hypothesis.
#'
#' @author
#' Csilla van Lunteren
#'
#'  @seealso
#' \code{\link{test_h0}}
#'
#' @import stats
#' @export
#'
fix_calc <- function(N0, sd_ber, delta = 0, Delta, test = 1, alpha = 0.05, beta = 0.2){
  X <- stats::rnorm(n = N0 / 2, mean = Delta, sd = sd_ber)
  Y <- stats::rnorm(n = N0 / 2, mean = 0, sd = sd_ber)

  S_gepoolt <- sqrt(((length(X) - 1) * stats::var(X) + (length(Y) - 1) * stats::var(Y) )/ (length(X) + length(Y) - 2))
  if (length(X) == 1 & length(Y) == 1){
    S_gepoolt <- sqrt(stats::var(c(X, Y)))
  }

  T <- sqrt((length(X) * length(Y)) / (length(X) + length(Y))) * (mean(X) - mean(Y) - delta) / S_gepoolt
  return (test_h0(test, T, length(X), length(Y), alpha, delta))
}
