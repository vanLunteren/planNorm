#' @title 
#' Creates a plot for the power based on the timing of the internal pilot study.
#' 
#' @description 
#' That is one of the main functions. 
#' It draws a plot of power based on the timing of the internal pilot study. 
#' If desired, it may represent several actual expected value differences for comparison.
#' 
#' @usage 
#' pow_prop(delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = c(0.1, 1),
#'          adj = F, regel = F, nbound = 500, simu = 10000)
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
#' Number/ vector of numbers. Relevant difference of expected values in the alternative hypothesis.
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
#' Attention: Choice of delta. (see \code{delta})
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
#' Vector of two Numbers. Timing of the internal pilot study depending on the originally planned sample size.
#' Two numbers must be passed, the smallest and largest timing of the internal pilot study. 
#' The plot is displayed for the interval of these two values.
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
#' If no nbound are defined then a standard deviation range must be chosen (see \code{sd_ber}).
#' 
#' @param simu
#' Number. How many simulations should be performed?
#' If not specified, simu is set to 10000.
#' 
#' @return
#' It returns a plot. 
#' 
#' @author
#' Csilla van Lunteren
#' 
#' @seealso
#' \code{\link{pow}}\cr
#' \link{ggplot2}
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @import ggplot2 
#' @export
#'
pow_prop <- function (delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = c(0.1, 1),
                     adj = F, regel = F, nbound = 500, simu = 10000){
  
  prop_area <- seq(min(prop), max(prop), (max(prop) - min(prop)) / 20 )
  pow_prop <- pow(delta = delta, Delta = Delta, sd = sd, test = test, alpha = alpha, beta = beta,
                      prop = prop_area, adj = adj, regel = regel, nbound = nbound, simu = simu)
 
  if (test == 1){
    test_n <-"one-sided"
  } else if (test == 2){
    test_n <-"two-sided"
  }
  ##evtl N0 dazu
  text <- paste("Settings:", "\n ","delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                  "\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta, 
                  ", nbound = ", nbound )

  powplot <- ggplot2::ggplot()
  
  for (j in 1:length(Delta)){
    pow <- pow_prop[[j]]
    pow_dat <- data.frame(pow = pow, prop = prop_area)
    powplot <- powplot +
      ggplot2::geom_line(data = pow_dat, ggplot2::aes( x = prop, y = pow), col = j, size = 1)
  }
  
  powplot <- powplot + 
    ggplot2::coord_cartesian(xlim = c(prop_area[1], prop_area[length(prop_area)]), 
                    ylim = c(min(pow_prop[[1]]), max(pow_prop[[length(Delta)]])))

  p1 = rep(-1, 2 * length(Delta))
  p2 = rep(-1, 2 * length(Delta))
  
  point_leg <- data.frame(p1 = p1, p2 = p2, prop = 1:length(Delta))
    
  powplot <- powplot +
    ggplot2::geom_line(data = point_leg, ggplot2::aes(x = p1, y = p2), col = factor(prop)) +
    ggplot2::labs(color = "") +
    ggplot2::scale_color_manual(labels = c(paste("Delta =", Delta)), 
                       values = c(1:length(Delta))) +
    ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
  
  powplot +
    ggplot2::geom_hline(yintercept = 0.8, linetype = 2, col = "gray") +
    ggplot2::scale_y_continuous(name = "Power") +
    ggplot2::scale_x_continuous(name = "Timing (n1/N)") +
    ggplot2::ggtitle("Power") +
    ggplot2::theme(axis.line.x = ggplot2::element_line(size = 0.5, colour = "black"),
          axis.line.y = ggplot2::element_line(size = 0.5, colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank())
}
