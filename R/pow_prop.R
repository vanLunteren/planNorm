#' @title
#' Creates a plot or a table for the type I error rate and power versus timing of the internalpilot study
#'
#' @description
#' It creates a plot or a table for the sample size, type I error rate, and power versus standard deviation.
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
#' Attention: If you chose 'test = 1' and 'delta != 0', the test for non-inferiority will \cr
#' automatically be applied.\cr
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
#' Number. What type of hypothesis test should be performed. One-sided (Superiority/ \cr
#' Non-Inferiority test) or two-sided (Test for difference).\cr
#' One-sided (test = 1): Superiortity H0: mu_x - mu_y <= 0 vs. H1: mu_x - mu_y > 0\cr
#'                       Non-Inferiority H0: mu_x - mu_y >= delta vs. H1: mu_x - mu_y < delta\cr
#' Two-sided (test = 2): Difference H0: |mu_x - mu_y| = 0 vs. H1: |mu_x -  mu_y| != 0\cr
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
#' Two numbers/ Vector of numbers. Timing of the internal pilot study depending on the originally\cr
#' planned sample size.\cr
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

  if (test == 1){
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * sd^2 /
                    (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }

  if (length(prop) == 2){
    prop_area <- round(seq(min(prop), max(prop), (max(prop) - min(prop)) / 10 ), 2)
  } else if (length(prop) >= 2){
    prop_area <- prop
  }

  Delta <- sort(Delta)
  pow_prop_ <- pow(delta = delta, Delta = Delta, sd = sd, test = test, alpha = alpha, beta = beta,
                      prop = prop_area, adj = adj, regel = regel, nbound = nbound, simu = simu)

  if (test == 1){
    test_n <-"one-sided"
  } else if (test == 2){
    test_n <-"two-sided"
  }

  if (create == "plot"){
    text <- paste("Settings:", "\n ","delta = ", delta, ", SD = ", sd, ", test = ", test_n,
                  "\n alpha = ", alpha, ", beta = ", beta, ", nbound = ", nbound )

    alpplot <- ggplot2::ggplot()
    powplot <- ggplot2::ggplot()

    for (j in 1:length(Delta)){
      alp_ <- pow_prop_[[j]][1,]
      pow_ <- pow_prop_[[j]][2,]

      alp_dat <- data.frame(alp_ = alp_, prop = prop_area)
      pow_dat <- data.frame(pow_ = pow_, prop = prop_area)

      alpplot <- alpplot +
        ggplot2::geom_line(data = alp_dat, ggplot2::aes(x = prop, y = alp_), col = j, size = 1)
      powplot <- powplot +
        ggplot2::geom_line(data = pow_dat, ggplot2::aes(x = prop, y = pow_), col = j, size = 1)
    }

    alpplot <- alpplot +
      ggplot2::coord_cartesian(xlim = c(prop_area[1], prop_area[length(prop_area)]),
                               ylim = c(0.4 * alpha, 1.6 * alpha))
#min(pow_prop_[[1]][1,]), max(pow_prop_[[length(Delta)]][1,]))
    powplot <- powplot +
      ggplot2::coord_cartesian(xlim = c(prop_area[1], prop_area[length(prop_area)]),
                               ylim = c(min(pow_prop_[[1]][2,]),
                                        max(pow_prop_[[length(Delta)]][2,])))

    p1 = rep(-1, 2 * length(Delta))
    p2 = rep(-1, 2 * length(Delta))

    point_leg <- data.frame(p1 = p1, p2 = p2, prop = 1:length(Delta))

    alpplot <- alpplot +
      ggplot2::geom_line(data = point_leg, ggplot2::aes(x = p1, y = p2, col = factor(prop))) +
      ggplot2::labs(color = "") +
      ggplot2::scale_color_manual(
        labels = c(paste("Delta =", sort(Delta, decreasing = TRUE),
                         "(N0 =", sort(N0, decreasing = FALSE), ")")),
        values = c(length(Delta):1)) +
      ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
    powplot <- powplot +
      ggplot2::geom_line(data = point_leg, ggplot2::aes(x = p1, y = p2, col = factor(prop))) +
      ggplot2::labs(color = "") +
      ggplot2::scale_color_manual(
        labels = c(paste("Delta =", sort(Delta, decreasing = TRUE),
                         "(N0 =", sort(N0, decreasing = FALSE), ")")),
        values = c(length(Delta):1)) +
      ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))

    alpplot <- alpplot +
      ggplot2::geom_hline(yintercept = alpha, linetype = 2, col = "gray") +
      ggplot2::scale_y_continuous(name = "Type I error rate") +
      ggplot2::scale_x_continuous(name = "Timing (n1/N)") +
      ggplot2::ggtitle("Type I error rate") +
      ggplot2::theme(axis.line.x = ggplot2::element_line(size = 0.5, color = "black"),
                     axis.line.y = ggplot2::element_line(size = 0.5, color = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank())
    powplot <- powplot +
      ggplot2::geom_hline(yintercept = 0.8, linetype = 2, col = "gray") +
      ggplot2::scale_y_continuous(name = "Power") +
      ggplot2::scale_x_continuous(name = "Timing (n1/N)") +
      ggplot2::ggtitle("Power") +
      ggplot2::theme(axis.line.x = ggplot2::element_line(size = 0.5, color = "black"),
                     axis.line.y = ggplot2::element_line(size = 0.5, color = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank())

    gridExtra::grid.arrange(
      alpplot,
      powplot,
      nrow = 2,
      ncol = 1,
      top = "Type I error rate and Power",
      bottom = grid::textGrob(
        text, gp = grid::gpar(fontface = 1, fontsize = 9), x = 0, hjust = -0.1
      )
    )

  } else if (create == "tab"){
    alp_d <- c()
    pow_d <- c()
    for (i in 1:length(Delta)){
      alp_d <- cbind(alp_d, pow_prop_[[i]][1, ])
      pow_d <- cbind(pow_d, pow_prop_[[i]][2, ])
    }
    alp_d <- round(alp_d, 3)
    pow_d <- round(pow_d, 2)

    dat <- data.frame(prop_area = c(prop_area, rep(" ", max(0, 6 - length(prop_area)))),
                      alp = rbind(alp_d, rep(" ", max(0, 6 - length(prop_area))),
                                  rep(" ", max(0, 6 - length(prop_area)))),
                      pow = rbind(pow_d, rep(" ", max(0, 6 - length(prop_area))),
                                  rep(" ", max(0, 6 - length(prop_area)))),
                      Settings = c(paste("delta = ", delta), paste("SD = ", sd),
                                   paste("test = ", test_n), paste("alpha = ", alpha),
                                   paste("beta = ", beta), paste("nbound = ", nbound),
                                   rep(" ", max(0, length(prop_area) - 6))))

    dat <- flextable::regulartable(data = dat)

    typeone <- "Type I error rate"
    pp <- "Power"
    DN <- "Delta (N0)"
    DN0 <- paste(Delta[1:length(Delta)], "(", N0[1:length(Delta)], ")")


    if (length(Delta) == 1){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", alp = typeone, pow = pp,
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", alp = DN,  pow = DN, Settings = " ",
                                   top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", alp = DN0[1], pow = DN0[1],
                                   Settings = " ", top = FALSE)
    } else if (length(Delta) == 2){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", alp.1 = typeone,
                                          alp.2 = typeone, pow.1 = pp, pow.2 = pp,
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN, alp.2 = DN, pow.1 = DN,
                                   pow.2 = DN, Settings = " ",  top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN0[1], alp.2 = DN0[2],
                                   pow.1 = DN0[1], pow.2 = DN0[2], Settings = "", top = FALSE)
    } else if (length(Delta) == 3){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", alp.1 = typeone,
                                          alp.2 = typeone, pow.1 = pp, pow.2 = pp, pow.3 = pp,
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN, alp.2 = DN,
                                   alp.3 = DN, pow.1 = DN, pow.2 = DN, pow.3 = DN, Settings = " ",
                                   top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN0[1], alp.2 = DN0[2],
                                   alp.3 = DN0[3], pow.1 = DN0[1], pow.2 = DN0[2], pow.3 = DN0[3],
                                   Settings = " ", top = FALSE)
    } else if (length(Delta) == 4){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", alp.1 = typeone,
                                          alp.2 = typeone, alp.3 = typeone, alp.4 = typeone,
                                          pow.1 = pp, pow.2 = pp, pow.3 = pp, pow.4 = pp,
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN, alp.2 = DN, alp.3 = DN,
                                   alp.4 = DN, pow.1 = DN, pow.2 = DN, pow.3 = DN, pow.4 = DN,
                                   Settings = " ", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN0[1], alp.2 = DN0[2],
                                   alp.3 = DN0[3], alp.4 = DN0[4], pow.1 = DN0[1], pow.2 = DN0[2],
                                   pow.3 = DN0[3], pow.4 = DN0[4], Settings = " ", top = FALSE)
    } else if (length(Delta) == 5){
      dat <- flextable::set_header_labels(dat, prop_area = "Timing", alp.1 = typeone,
                                          alp.2 = typeone, alp.3 = typeone, alp.4 = typeone,
                                          alp.5 = typeone, pow.1 = pp, pow.2 = pp, pow.3 = pp,
                                          pow.4 = pp, pow.5 = pp, Settings = "Settings")
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN, alp.2 = DN, alp.3 = DN,
                                   alp.4 = DN, alp.5 = DN, pow.1 = DN, pow.2 = DN, pow.3 = DN,
                                   pow.4 = DN, pow.5 = DN, Settings = " ", top = FALSE)
      dat <- flextable::add_header(dat, prop_area = " ", alp.1 = DN0[1], alp.2 = DN0[2],
                                   alp.3 = DN0[3], alp.4 = DN0[4], alp.5 = DN0[5], pow.1 = DN0[1],
                                   pow.2 = DN0[2], pow.3 = DN0[3], pow.4 = DN0[4], pow.5 = DN0[5],
                                   Settings = " ", top = FALSE)
    }

    long <- length(Delta) - 1
    start1 <- 3 + long

    dat <- flextable::merge_at(dat, i = 1, j = 2:(2 + long), part = "head")
    dat <- flextable::merge_at(dat, i = 1, j = start1:(start1 + long), part = "head")

    dat <- flextable::merge_at(dat, i = 2, j = 2:(2 + long), part = "head")
    dat <- flextable::merge_at(dat, i = 2, j = start1:(start1 + long), part = "head")

    dat <- flextable::autofit(dat)

    dat <- flextable::align(dat, j = 1, align = "left", part = "header")
    dat <- flextable::align(dat, j = 1, align = "left", part = "all")
    dat <- flextable::align(dat, j = 2 * length(Delta) + 2, align = "right", part = "header")
    dat <- flextable::align(dat, j = 2 * length(Delta) + 2, align = "right", part = "all")
    dat <- flextable::align(dat, j = 2:(2 * length(Delta) + 1), align = "center", part = "all")

    dat <- flextable::style(dat, pr_t = officer::fp_text(bold = TRUE), part = "header")
    dat <- flextable::fontsize(dat, size = 11, part = "all")

    def_cell <- officer::fp_cell(border = officer::fp_border(color = "transparent"))
    dat <- flextable::style(dat , pr_c = def_cell, part = "all")

    dat <- flextable::vline(x = dat, j = c(1, 1 + length(Delta), 1 + 2 * length(Delta)),
                            border = officer::fp_border(width = 1), part = "all")
    dat <- flextable::hline(x = dat, i = 1, border = officer::fp_border(width = 1), part = "head")
    dat <- flextable::hline(x = dat, i = 3, border = officer::fp_border(width = 2), part = "head")
    dat
  }

}
