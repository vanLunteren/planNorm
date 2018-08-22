#' @title
#' Creates a plot for the sample size and the power based on the standard deviation
#'
#' @description
#' That is one of the main functions.
#' It draws a plot of the sample size and the power based on the standard deviation.
#' The sample size and power can be calculated for the design with a fixed sample size and / or
#' with an internal pilot study.
#' If desired, it may represent several timings for the internal pilot studies for comparison.
#'
#' @usage
#' sample_pow(sd_ber = T, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2,
#'            prop = c(0.5, 0.7), adj = F,  regel = F, nbound = 500,
#'            fix_sim = c("fix", "sim"), simu = 10000, create = "plot")
#'
#' @param sd_ber
#' Sequence of numbers. Interval of the actual standard deviation in the data.\cr
#' With regard to this interval, the sample size and the power are displayed.\cr
#' If no interval with more than two numbers but an upper limit is defined (see nbound),\cr
#' a reasonable range is calculated automatically.
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
#' Number. Assumed standard deviation of the data.\cr
#' Used to calculate the originally planned sample size.
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
#' @param prop
#' Number/ vector of numbers.\cr
#' Timing of the internal pilot study depending on the originally planned sample size.\cr
#' If a vector is passed, all timings are displayed.\cr
#' A maximum of five different values are possible if you choose \code{create = "tab"}.\cr
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
#' be calculated automatically. If no nbound are defined then a standard deviation range must be chosen\cr
#' (see sd_ber).
#'
#' @param fix_sim
#' Which design should be applied and presented?\cr
#' "fixed": Design with fixed sample size,\cr
#' "sim": Design with internal pilot study,\cr
#' c("fix", "sim"): both design
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
#' \code{\link{fix_sample_pow}}\cr
#' \code{\link{sim_sample_pow}}\cr
#' \link{ggplot2}\cr
#' \link{gridExtra}
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob
#' @import stats
#' @importFrom flextable regulartable
#' @importFrom flextable set_header_labels
#' @importFrom flextable add_header
#' @importFrom flextable merge_at
#' @importFrom flextable autofit
#' @importFrom flextable  align
#' @importFrom flextable style
#' @importFrom flextable fontsize
#' @importFrom flextable vline
#' @importFrom flextable hline
#' @importFrom officer fp_text
#' @importFrom officer fp_cell
#' @importFrom officer fp_border
#'
#' @export
#'
sample_pow <- function (sd_ber = T, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2,
                        prop = c(0.5, 0.7), adj = F,  regel = F, nbound = 500, fix_sim = c("fix", "sim"),
                        simu = 10000, create = "plot"){
  if (length(prop) > 5 & create == "tab"){
    stop("Maximum five values for prop are allowed!")
  }

  if (test == 1){
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }

  if (test == 1){
    test_n <- "one-sided"
  } else if (test == 2){
    test_n <- "two-sided"
  }

  if (is.numeric(sd_ber) & length(sd_ber) == 2){
    sd_ber <- seq(min(sd_ber), max(sd_ber), (max(sd_ber) - min(sd_ber)) / 25)
  }  else if (is.numeric(sd_ber) & length(sd_ber) >= 2){
    sd_ber <- sort(sd_ber)
  }  else {
    sd_min <- (1 / 2 * sqrt(N0 * min(prop)) * 1 / (stats::qnorm(1 - alpha) + stats::qnorm(0.95)) *
                 (Delta - delta))
    sd_max <- (1 / 2 * sqrt(nbound) * 1 / (stats::qnorm(1 - alpha) + stats::qnorm(0.18)) * (Delta - delta))
    sd_ber <- round(seq(sd_min, sd_max, (sd_max - sd_min) / 25), 2)
  }

  if ("fix" %in% fix_sim) {
    N_pow_f <- fix_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test,
                              alpha = alpha, beta = beta, simu = simu)
  }

  if ("sim" %in% fix_sim){
    N_pow_s <- sim_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test,
                              alpha = alpha, beta = beta, prop = prop, adj = adj, regel = regel,
                              nbound = nbound, simu = simu)
  }

  if (create == "plot"){
    if ("sim" %in% fix_sim){
      text <- paste("Settings:", "\n ","delta = ", delta, ", Delta = ", Delta, ", SD = ", sd,
                    ", N0 = ", N0, "\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta,
                    ", nbound = ", nbound )
    } else if (!("sim" %in% fix_sim)){
      text <- paste("Settings:", "\n", "delta = ", delta, ", Delta = ", Delta, ", SD = ", sd,
                    ", N0 = ", N0, ",\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta)
    }

    pplot <- ggplot2::ggplot()

    if ("fix" %in% fix_sim){
      N_fix <- data.frame(sd = sd_ber, N = N_pow_f[1, ])
      if (!("sim" %in% fix_sim)){
        pplot <- pplot +
          ggplot2::geom_line(data = N_fix, ggplot2::aes(x = sd, y = N), col = "black")
      } else {
        pplot <- pplot +
          ggplot2::geom_line(data = N_fix, ggplot2::aes(x = sd, y = N), col = "darkgrey")
      }
    }

    if ("sim" %in% fix_sim){
      for (j in 1:length(prop)){
        sd_ <- c()
        N <- c()
        for (i in 1:length(sd_ber)){
          sd_ <- c(sd_, rep(sd_ber[i], 5))
          N <- c(N, N_pow_s[[i]][1:5, j])
        }
        N_sim <- data.frame(N = N, sd = sd_)
        pplot <- pplot +
          ggplot2::geom_boxplot(data = N_sim, ggplot2::aes(x = sd, y = N, group = sd), outlier.shape = NA,
                                col = j, fill = j, alpha = 0.2)
      }
      pplot <- pplot +
        ggplot2::coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]),
                                 ylim = c(0, nbound + nbound / 12))
    } else if ("fix" %in% fix_sim & !("sim" %in% fix_sim)){
      pplot <- pplot +
        ggplot2::coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]))
    }

    if ("sim" %in% fix_sim){
      x = rep(-1, 2 * (length(prop) + 1))
      y = rep(-1, 2 * (length(prop) + 1))
      point_leg <- data.frame(x = x, y = y, prop = rep(1:(length(prop) + 1), 2))
      pplot <- pplot +
        ggplot2::geom_line(data = point_leg, ggplot2::aes(x = x, y = y, color = factor(prop))) +
        ggplot2::labs(color = "") +
        ggplot2::scale_color_manual(labels = c("fix", paste("prop =",prop)),
                                    values = c("darkgray", 1:length(prop))) +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
    } else {
      point_leg <- data.frame(x = c(-1, -1), y = c(-1, -1), prop = c(1, 1))

      pplot <- pplot +
        ggplot2::geom_line(data = point_leg, ggplot2::aes(x = x, y = y, color = factor(prop))) +
        ggplot2::labs(color = "") +
        ggplot2::scale_color_manual(labels = "fix", values = "black") +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
    }

    pplot <- pplot +
      ggplot2::geom_hline(yintercept = N0, linetype = 2, col = "gray") +
      ggplot2::scale_y_continuous(name = "Sample Size") +
      ggplot2::scale_x_continuous(name = "Standard Deviation",
                                  breaks = seq(round(min(sd_ber)), round(max(sd_ber)),
                                               round((max(sd_ber) - min(sd_ber)) / 10))) +
      ggplot2::ggtitle("Sample Size") +
      ggplot2::theme(axis.line.x = ggplot2::element_line(size = 0.5, colour = "black"),
                     axis.line.y = ggplot2::element_line(size = 0.5, colour = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank())

    powplot <- ggplot2::ggplot()

    if ("fix" %in% fix_sim){
      pow_fix <- data.frame(sd = sd_ber, N = N_pow_f[2, ])

      if (!("sim" %in% fix_sim)){
        powplot <- powplot +
          ggplot2::geom_line(data = pow_fix, ggplot2::aes(x = sd, y = N), col = "black")
      } else {
        powplot <- powplot +
          ggplot2::geom_line(data = pow_fix, ggplot2::aes(x = sd, y = N), col = "darkgrey")
      }
    }

    if ("sim" %in% fix_sim){
      for (j in 1:length(prop)){
        N <- c()
        for (i in 1:length(sd_ber)){
          N <- c(N, N_pow_s[[i]][6, j])
        }
        pow_sim <- data.frame(N = N, sd = sd_ber)
        powplot <- powplot +
          ggplot2::geom_line(data = pow_sim, ggplot2::aes( x = sd, y = N ), col = j, size = 1)
      }
    }

    powplot <- powplot +
      ggplot2::coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]), ylim = c(0, 1))

    if ("sim" %in% fix_sim){
      x = rep(-1, 2 * (length(prop) + 1))
      y = rep(-2, 2 * (length(prop) + 1))
      point_leg <- data.frame(x, y, prop = 1:(length(prop) + 1))

      powplot <- powplot +
        ggplot2::geom_line(data = point_leg, ggplot2::aes(x = x, y = y, col = factor(prop))) +
        ggplot2::labs(color = "") +
        ggplot2::scale_color_manual(labels = c("fix", paste("prop =", prop)),
                                    values = c("darkgray", 1:length(prop))) +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
    } else {
      point_leg <- data.frame(x = c(-1, -1), y = c(-1, -1), prop = 1)

      powplot <- powplot +
        ggplot2::geom_line(data = point_leg, ggplot2::aes(x = x, y = y, col = factor(prop))) +
        ggplot2::labs(color = "") +
        ggplot2::scale_color_manual(labels = "fix", values = "black") +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"))
    }

    powplot <- powplot +
      ggplot2::geom_hline(yintercept = 0.8, linetype = 2, col = "gray") +
      ggplot2::geom_hline(yintercept = 0.2, linetype = 2, col = "gray") +
      ggplot2::scale_y_continuous(name = "Power", breaks = seq(0, 1, 0.2)) +
      ggplot2::scale_x_continuous(name = "Standard Deviation",
                                  breaks = seq(round(min(sd_ber)), round(max(sd_ber)),
                                               round((max(sd_ber) - min(sd_ber)) / 10))) +
      ggplot2::ggtitle("Power") +
      ggplot2::theme(axis.line.x = ggplot2::element_line(size = 0.5, colour = "black"),
                     axis.line.y = ggplot2::element_line(size = 0.5, colour = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank())

    gridExtra::grid.arrange(
      pplot,
      powplot,
      nrow = 1,
      ncol = 2,
      top = "Sample Size and Power",
      bottom = grid::textGrob(text, gp = grid::gpar(fontface = 1, fontsize = 9), x = 0, hjust = -0.1
      )
    )
  } else if (create == "tab"){

    if ("fix" %in% fix_sim & "sim" %in% fix_sim){
      simN <- c()
      simpow <- c()
      for (j in 1:length(prop)){
        N_ <- c()
        pow_ <- c()
        for (i in 1:length(sd_ber)){
          N_ <- c(N_, N_pow_s[[i]][3, j])
          pow_ <- c(pow_, N_pow_s[[i]][6, j])
        }
        simN <- cbind(simN, N_)
        simpow <- cbind(simpow, pow_)
      }

      dat <- data.frame(sd_ber = c(sd_ber, rep(" ", max(0, 8 - length(sd_ber)))),
                        N_f = c(N_pow_f[1, ], rep(" ", max(0, 8 - length(sd_ber)))),
                        N_s = rbind(simN, rep(" ", max(0, 8 - length(sd_ber))),
                                    rep(" ", max(0, 8 - length(sd_ber)))),
                        p_f = c(N_pow_f[2, ], rep(" ", max(0, 8 - length(sd_ber)))),
                        p_s = rbind(simpow, rep(" ", max(0, 8 - length(sd_ber))),
                                    rep(" ", max(0, 8 - length(sd_ber)))),
                        Settings = c(paste("delta = ", delta), paste("Delta = ", Delta), paste("SD = ", sd),
                                     paste("N0 = ", N0), paste("test = ", test_n), paste("alpha = ", alpha),
                                     paste("beta = ", beta), paste("nbound = ", nbound),
                                     rep(" ", max(0, length(sd_ber) - 8))))

    } else if ("fix" %in% fix_sim & !("sim" %in% fix_sim)){
      dat <- data.frame(sd_ber = c(sd_ber, rep(" ", max(0, 7 - length(sd_ber)))),
                        N_f = c(N_pow_f[1, ], rep(" ", max(0, 7 - length(sd_ber)))),
                        p_f = c(N_pow_f[2, ], rep(" ", max(0, 7 - length(sd_ber)))),
                        Settings = c(paste("delta = ", delta), paste("Delta = ", Delta), paste("SD = ", sd),
                                     paste("N0 = ", N0), paste("test = ", test_n), paste("alpha = ", alpha),
                                     paste("beta = ", beta), rep(" ", max(0, length(sd_ber) - 7))))

    } else if (!("fix" %in% fix_sim) & "sim" %in% fix_sim){
      simN <- c()
      simpow <- c()
      for (j in 1:length(prop)){
        N_ <- c()
        pow_ <- c()
        for (i in 1:length(sd_ber)){
          N_ <- c(N_, N_pow_s[[i]][3, j])
          pow_ <- c(pow_, N_pow_s[[i]][6, j])
        }
        simN <- cbind(simN, N_)
        simpow <- cbind(simpow, pow_)
      }

      dat <- data.frame(sd_ber = c(sd_ber, rep(" ", max(0, 8 - length(sd_ber)))),
                        N_s = rbind(simN, rep(" ", max(0, 8 - length(sd_ber))),
                                    rep(" ", max(0, 8 - length(sd_ber)))),
                        p_s = rbind(simpow, rep(" ", max(0, 8 - length(sd_ber))),
                                    rep(" ", max(0, 8 - length(sd_ber)))),
                        Settings = c(paste("delta = ", delta), paste("Delta = ", Delta),
                                     paste("SD = ", sd), paste("N0 = ", N0), paste("test = ", test_n),
                                     paste("alpha = ", alpha), paste("beta = ", beta),
                                     paste("nbound = ", nbound), rep(" ", max(0, length(sd_ber) - 8))))
    }

    dat <- flextable::regulartable(data = dat)

    if ("fix" %in% fix_sim & "sim" %in% fix_sim){
      if (length(prop) == 1){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", N_ = "Sample size",
                                            p_f = "Power", pow_ = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", N_ = "prop =", p_f = "fix",
                                     pow_ = "prop =", Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", N_ = prop[1], p_f = " ",
                                     pow_ = prop[1], Settings = " ", top = FALSE )
      } else if (length(prop) == 2){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", p_f = "Power", p_s.pow_ = "Power",
                                            p_s.pow_.1 = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     p_f = "fix", p_s.pow_ = "prop =", p_s.pow_.1 = "prop =", Settings = " ",
                                     top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     p_f = " ", p_s.pow_ = prop[1], p_s.pow_.1 = prop[2], Settings = " ",
                                     top = FALSE )
      } else if (length(prop) == 3){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            p_f = "Power", p_s.pow_ = "Power", p_s.pow_.1 = "Power",
                                            p_s.pow_.2 = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", p_f = "fix", p_s.pow_ = "prop =",
                                     p_s.pow_.1 = "prop =", p_s.pow_.2 = "prop =", Settings = " ",
                                     top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], p_f = " ", p_s.pow_ = prop[1],
                                     p_s.pow_.1 = prop[2], p_s.pow_.2 = prop[3], Settings = " ",
                                     top = FALSE )
      } else if (length(prop) == 4){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            N_s.N_.3 = "Sample size", p_f = "Power", p_s.pow_ = "Power",
                                            p_s.pow_.1 = "Power", p_s.pow_.2 = "Power", p_s.pow_.3 = "Power",
                                            Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", N_s.N_.3 = "prop =", p_f = "fix",
                                     p_s.pow_ = "prop =", p_s.pow_.1 = "prop =", p_s.pow_.2 = "prop =",
                                     p_s.pow_.3 = "prop =", Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], N_s.N_.3 = prop[4], p_f = " ", p_s.pow_ = prop[1],
                                     p_s.pow_.1 = prop[2], p_s.pow_.2 = prop[3], p_s.pow_.3 = prop[4],
                                     Settings = " ", top = FALSE )
      } else if (length(prop) == 5){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            N_s.N_.3 = "Sample size", N_s.N_.4 = "Sample size",
                                            p_f = "Power", p_s.pow_ = "Power", p_s.pow_.1 = "Power",
                                            p_s.pow_.2 = "Power", p_s.pow_.3 = "Power",
                                            p_s.pow_.4 = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", N_s.N_.3 = "prop =", N_s.N_.4 = "prop =",
                                     p_f = "fix", p_s.pow_ = "prop =", p_s.pow_.1 = "prop =",
                                     p_s.pow_.2 = "prop =", p_s.pow_.3 = "prop =", p_s.pow_.4 = "prop =",
                                     Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], N_s.N_.3 = prop[4], N_s.N_.4 = prop[5], p_f = " ",
                                     p_s.pow_ = prop[1], p_s.pow_.1 = prop[2], p_s.pow_.2 = prop[3],
                                     p_s.pow_.3 = prop[4], p_s.pow_.4 = prop[5], Settings = " ",
                                     top = FALSE )
      }

    } else if ("fix" %in% fix_sim & !("sim" %in% fix_sim)){
      dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_f = "Sample size", p_f = "Power",
                                          Settings = "Settings")
      dat <- flextable::add_header(dat, sd_ber = " ", N_f = "fix", p_f = "fix", Settings = " ", top = FALSE )
      dat <- flextable::add_header(dat, sd_ber = " ", N_f = " ", p_f = " ", Settings = " ", top = FALSE )

    } else if (!("fix" %in% fix_sim) & "sim" %in% fix_sim){
      if (length(prop) == 1){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_ = "Sample size", pow_ = "Power",
                                            Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_ = "prop =", pow_ = "prop =", Settings = " ",
                                     top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_ = prop[1], pow_ = prop[1], Settings = " ",
                                     top = FALSE )
      } else if (length(prop) == 2){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", p_s.pow_ = "Power",
                                            p_s.pow_.1 = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     p_s.pow_ = "prop =", p_s.pow_.1 = "prop =", Settings = " ",
                                     top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     p_s.pow_ = prop[1], p_s.pow_.1 = prop[2], Settings = " ",
                                     top = FALSE )
      } else if (length(prop) == 3){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            p_s.pow_ = "Power", p_s.pow_.1 = "Power", p_s.pow_.2 = "Power",
                                            Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", p_s.pow_ = "prop =", p_s.pow_.1 = "prop =",
                                     p_s.pow_.2 = "prop =", Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], p_s.pow_ = prop[1], p_s.pow_.1 = prop[2],
                                     p_s.pow_.2 = prop[3], Settings = " ", top = FALSE )
      } else if (length(prop) == 4){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            N_s.N_.3 = "Sample size", p_s.pow_ = "Power",
                                            p_s.pow_.1 = "Power", p_s.pow_.2 = "Power", p_s.pow_.3 = "Power",
                                            Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", N_s.N_.3 = "prop =", p_s.pow_ = "prop =",
                                     p_s.pow_.1 = "prop =", p_s.pow_.2 = "prop =", p_s.pow_.3 = "prop =",
                                     Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], N_s.N_.3 = prop[4], p_s.pow_ = prop[1],
                                     p_s.pow_.1 = prop[2], p_s.pow_.2 = prop[3], p_s.pow_.3 = prop[4],
                                     Settings = " ", top = FALSE )
      } else if (length(prop) == 5){
        dat <- flextable::set_header_labels(dat, sd_ber = "SD", N_s.N_ = "Sample size",
                                            N_s.N_.1 = "Sample size", N_s.N_.2 = "Sample size",
                                            N_s.N_.3 = "Sample size", N_s.N_.4 = "Sample size",
                                            p_s.pow_ = "Power", p_s.pow_.1 = "Power", p_s.pow_.2 = "Power",
                                            p_s.pow_.3 = "Power", p_s.pow_.4 = "Power", Settings = "Settings")
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = "prop =", N_s.N_.1 = "prop =",
                                     N_s.N_.2 = "prop =", N_s.N_.3 = "prop =", N_s.N_.4 = "prop =",
                                     p_s.pow_ = "prop =", p_s.pow_.1 = "prop =", p_s.pow_.2 = "prop =",
                                     p_s.pow_.3 = "prop =", p_s.pow_.4 = "prop =", Settings = " ", top = FALSE )
        dat <- flextable::add_header(dat, sd_ber = " ", N_s.N_ = prop[1], N_s.N_.1 = prop[2],
                                     N_s.N_.2 = prop[3], N_s.N_.3 = prop[4], N_s.N_.4 = prop[5],
                                     p_s.pow_ = prop[1], p_s.pow_.1 = prop[2], p_s.pow_.2 = prop[3],
                                     p_s.pow_.3 = prop[4], p_s.pow_.4 = prop[5], Settings = " ", top = FALSE )
      }
    }

    if ("sim" %in% fix_sim){
      long <- length(prop)
      start2 <- 2 + length(prop)
      if ("fix" %in% fix_sim){
        long <- long + 1
        start2 <- start2 + 1
      }
      dat <- flextable::merge_at(dat, i = 1, j = 2:(2 + long - 1), part = "head")
      dat <- flextable::merge_at(dat, i = 1, j = start2:(start2 + long - 1), part = "head")

      where1 <- 2
      where2 <- 2 + length(prop)
      if ("fix" %in% fix_sim){
        where1 <- where1 + 1
        where2 <- where2 + 2
      }
      dat <- flextable::merge_at(dat, i = 2, j = where1:(where1 + length(prop) - 1), part = "head")
      dat <- flextable::merge_at(dat, i = 2, j = where2:(where2 + length(prop) - 1), part = "head")
    }

    dat <- flextable::autofit(dat)

    end <- 2
    if ("fix" %in% fix_sim){
      end <- end + 2
    }
    if ("sim" %in% fix_sim){
      end <- end + 2 * (length(prop))
    }

    dat <- flextable::align(dat, j = 1, align = "left", part = "header")
    dat <- flextable::align(dat, j = 1, align = "left", part = "all")
    dat <- flextable::align(dat, j = end, align = "right", part = "header")
    dat <- flextable::align(dat, j = end, align = "right", part = "all")
    dat <- flextable::align(dat, j = 2:(end - 1), align = "center", part = "all")

    dat <- flextable::style(dat, pr_t = officer::fp_text(bold = TRUE), part = "header")
    dat <- flextable::fontsize(dat, size = 11, part = "all")

    def_cell <- officer::fp_cell(border = officer::fp_border(color = "transparent"))
    dat <- flextable::style(dat , pr_c = def_cell, part = "all")

    line <- 1
    if ("fix" %in% fix_sim & "sim" %in% fix_sim){
      line <- c(line, 2 + length(prop), 3 + 2 * length(prop))
    } else if (!("fix" %in% fix_sim) & "sim" %in% fix_sim){
      line <- c(line, 1 + length(prop), 2 + 2 * length(prop))
    } else if ("fix" %in% fix_sim & !("sim" %in% fix_sim)){
      line <- c(line, 2, 3)
    }
    dat <- flextable::vline(x = dat, j = line, border = officer::fp_border(width = 1), part = "all")
    dat <- flextable::hline(x = dat, i = 1, border = officer::fp_border(width = 1), part = "head")
    dat <- flextable::hline(x = dat, i = 3, border = officer::fp_border(width = 2), part = "head")

    dat
  }


}
