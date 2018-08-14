#' @title
#' Test a test statistic to see if the null hypothesis can be rejected.
#'
#' @description
#' This is an important secondary function.
#' It checks whether the corresponding null hypothesis can be rejected by a calculated test statistic.
#' Three cases of hypotheses can be considered: test for superiority, test for non-inferiority, test for difference.
#'
#' @usage
#' test_h0(test = 1, T, n1, n2, alpha = 0.05, delta = 0)
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
#' @param T
#' Number. Value of the calculated test statistic.
#'
#' @param n1
#' Number. sample size of the first sample.
#'
#' @param n2
#' Number. sample size of the second sample.
#'
#' @param alpha
#' Number. Desired alpha-level of the test.\cr
#' If not specified, alpha is set to 0.05.
#'
#' @param delta
#' Number. Expectation difference of two samples.\cr
#' If you select a Test for superiority/ difference then select 'delta = 0'.\cr
#' Only if you select a Test for non-inferiority you can select 'delta != 0'.\cr
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will automatically \cr
#' be applied.\cr
#' If not specified, delta is set to 0.
#'
#' @details
#' The aim of this function is to simplify the application of test decisions in hypothesis tests.
#'
#' @return
#' The value 0 or 1 will be returned. \cr
#' 0 means the null hypothesis can not be rejected to the level alpha.\cr
#' 1 means the null hypothesis is rejected to the level alpha.
#'
#' @author
#' Csilla van Lunteren
#'
#' @import stats
#'
#' @export

test_h0 <- function (test = 1, T, n1, n2, alpha = 0.05, delta = 0){

  if (test == 1){
    if (n1 + n2 <= 2){
      if (1 - alpha > 0.5){
        quantil <- Inf
      } else if (1 - alpha > 0.5){
        quantil <- -Inf
      } else {
        quantil <- 0
      }
    } else {
      quantil <- stats::qt(1 - alpha, n1 + n2 - 2)
    }
  } else {
    if (n1 + n2 <= 2){
      if (1 - alpha / 2 > 0.5){
        quantil <- Inf
      } else if (1 - alpha / 2 > 0.5){
        quantil <- -Inf
      } else {
        quantil <- 0
      }
    } else {
      quantil <- stats::qt(1 - alpha / 2, n1 + n2 - 2)
    }
  }

  decision <- c()

  if (test == 1 & delta == 0){
    if (T > quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  } else if (test == 1 & delta != 0){
    if (T < -quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  } else if (test == 2){
    if (abs(T) > quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  }
  return (decision)
}
