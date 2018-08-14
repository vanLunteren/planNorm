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
#'          adj = F, regel = F, nbound = 500, simu = 10000, create = "plot")
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
#' Number/ vector of numbers. Relevant difference of expected values in the alternative hypothesis.\cr
#' A maximum of five different values are possible if you choose \code{create = "tab"}.
#'
#' @param sd
#' Number. Assumed standard deviation of the data.\cr
#' Used to calculate the originally planned number of cases.
#'
#' @param test
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ Non-Inferiority test)\cr
#' or two-sided (Test for difference).\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y > 0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Tweo-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: mu_x -  mu_y != 0\cr
#' Attention: Choice of delta. (see \code{delta})\cr
#' If not specified, the one-Sided Test (Superiority/ Non-Inferiority Test) is used.\cr
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
#' Two numbers/ Vector of numbers. Timing of the internal pilot study depending on the originally planned\cr
#' sample size.\cr
#' Two numbers must be passed, the smallest and largest timing of the internal pilot study.\cr
#' The plot is displayed for the interval of these values.\cr
#' If two values are transferred, a sequence is automatically defined.\cr
#' If several values are transferred, these are interreted as a sequence.
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
#' If no nbound are defined then a standard deviation range must be chosen (see \code{sd_ber}).\cr
#'
#' @param simu
#' Number. How many simulations should be performed?\cr
#' If not specified, simu is set to 10000.
#'
#' @param create
#' "plot" if a plot is to be returned.\cr
#' "tab" if a table is to be returned.
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
#' @importFrom flextable regulartable
#' @importFrom flextable set_header_labels
#' @importFrom flextable merge_at
#' @importFrom flextable align
#' @importFrom flextable style
#' @importFrom flextable border
#' @importFrom flextable bold
#' @importFrom flextable width
#' @importFrom flextable vline
#' @importFrom officer fp_text
#' @importFrom officer fp_cell
#' @importFrom officer fp_border
#'
#' @export
#'
pow_prop <- function (delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = c(0.1, 1),
                     adj = F, regel = F, nbound = 500, simu = 10000, create = "plot"){

  if (length(Delta) > 5 & create == "tab"){
    stop("Maximum five values for Delta are allowed!")
  }

  if (length(prop) == 2){
    prop_area <- round(seq(min(prop), max(prop), (max(prop) - min(prop)) / 20 ), 2)
  }

  pow_prop_ <- pow(delta = delta, Delta = Delta, sd = sd, test = test, alpha = alpha, beta = beta,
                      prop = prop_area, adj = adj, regel = regel, nbound = nbound, simu = simu)

  if (test == 1){
    test_n <-"one-sided"
  } else if (test == 2){
    test_n <-"two-sided"
  }

  if (create == "plot"){
    text <- paste("Settings:", "\n ","delta = ", delta, ", Delta = ", Delta, ", SD = ", sd,
                  "\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta,
                  ", nbound = ", nbound )

    powplot <- ggplot2::ggplot()

    for (j in 1:length(Delta)){
      pow <- pow_prop_[[j]]
      pow_dat <- data.frame(pow = pow, prop = prop_area)
      powplot <- powplot +
        ggplot2::geom_line(data = pow_dat, ggplot2::aes( x = prop, y = pow), col = j, size = 1)
    }

    powplot <- powplot +
      ggplot2::coord_cartesian(xlim = c(prop_area[1], prop_area[length(prop_area)]),
                               ylim = c(min(pow_prop_[[1]]), max(pow_prop_[[length(Delta)]])))

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

  } else if (create == "tab"){
    pow_d <- c()
    for (i in 1:length(Delta)){
      pow_d <- cbind(pow_d, pow_prop_[[i]])
    }

    dat <- data.frame(prop_area = c(prop_area, rep(" ", max(0, 6 - length(prop_area)))),
                      pow = rbind(pow_d, rep(" ", max(0, 6 - length(prop_area))),
                                  rep(" ", max(0, 6 - length(prop_area)))),
                      Settings = c(paste("delta = ", delta), paste("SD = ", sd), paste("test = ", test_n),
                                   paste("alpha = ", alpha), paste("beta = ", beta),
                                   paste("nbound = ", nbound), rep(" ", max(0, length(prop_area) - 6))))

    dat <- flextable::regulartable(data = dat)

    if (length(Delta) == 1){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", pow = "Power", Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", pow = "Delta =", Settings = " ", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", pow = Delta, Settings = " ", top = FALSE)
    } else if (length(Delta) == 2){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", pow.1 = "Power", pow.2 = "Power",
                                          Settings = "Settings", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = "Delta =", pow.2 = "Delta =",
                                   Settings = " ")
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = Delta[1], pow.2 = Delta[2], Settings = "",
                                   top = FALSE)
    } else if (length(Delta) == 3){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", pow.1 = "Power", pow.2 = "Power",
                                          pow.3 = "Power", Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = "Delta =", pow.2 = "Delta =",
                                   pow.3 = "Delta =", Settings = " ", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = Delta[1], pow.2 = Delta[2],
                                   pow.3 = Delta[3], Settings = " ", top = FALSE)
    } else if (length(Delta) == 4){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", pow.1 = "Power", pow.2 = "Power",
                                          pow.3 = "Power", pow.4 = "Power", Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = "Delta =", pow.2 = "Delta =",
                                   pow.3 = "Delta =", pow.4 = "Delta =", Settings = " ", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = Delta[1], pow.2 = Delta[2],
                                   pow.3 = Delta[3], pow.4 = Delta[4], Settings = " ", top = FALSE)
    } else if (length(Delta) == 5){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", pow.1 = "Power", pow.2 = "Power",
                                          pow.3 = "Power", pow.4 = "Power", pow.5 = "Power",
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = "Delta =", pow.2 = "Delta =",
                                   pow.3 = "Delta =", pow.4 = "Delta =", pow.5 = "Delta =", Settings = " ",
                                   top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", pow.1 = Delta[1], pow.2 = Delta[2],
                                   pow.3 = Delta[3], pow.4 = Delta[4], pow.5 = Delta[5], Settings = " ",
                                   top = FALSE)
    }

    dat <- flextable::merge_at(dat, i = 1, j = 2:(2 + length(Delta) - 1), part = "head")
    dat <- flextable::merge_at(dat, i = 2, j = 2:(2 + length(Delta) - 1), part = "head")

    dat <- flextable::autofit(dat)



    dat <- flextable::align(dat, j = 1, align = "left", part = "header")
    dat <- flextable::align(dat, j = 1, align = "left", part = "all")
    dat <- flextable::align(dat, j = length(Delta) + 2, align = "right", part = "header")
    dat <- flextable::align(dat, j = length(Delta) + 2, align = "right", part = "all")
    dat <- flextable::align(dat, j = 2:(length(Delta) + 1), align = "center", part = "all")

    dat <- flextable::style(dat, pr_t = officer::fp_text(bold = TRUE), part = "header")
    dat <- flextable::fontsize(dat, size = 11, part = "all")

    def_cell <- officer::fp_cell(border = officer::fp_border(color = "transparent"))
    dat <- flextable::style(dat , pr_c = def_cell, part = "all")

    dat <- flextable::vline(x = dat, j = c(1, 1 + length(Delta)), border = officer::fp_border(width = 1), part = "all")
    dat <- flextable::hline(x = dat, i = 1, border = officer::fp_border(width = 1), part = "head")
    dat <- flextable::hline(x = dat, i = 3, border = officer::fp_border(width = 2), part = "head")
  }

}
