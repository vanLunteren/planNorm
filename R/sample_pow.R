#' @title 
#' @description 
#' @usage 
#' sample_pow(sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = c(0.5, 0.7), 
#'            adj = F,  regel = F, nbound = 500, fix_sim = c("fix", "sim"), simu = 10000)
#'            
#' @param sd_ber
#' Sequence of numbers. Interval of the actual standard deviation in the data. 
#' With regard to this interval, the sample size and the power are displayed. 
#' If no interval with more than two numbers but an upper limit is defined (see nbound), 
#' a reasonable range is calculated automatically.
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
#' Used to calculate the originally planned sample size.
#' 
#' @param test 
#' Number. What type of hypothesis test should be performed, one-sided (Superiority/ Non-Inferiority test)
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
#' Number or vector of numbers. 
#' Timing of the internal pilot study depending on the originally planned sample size.
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
#' be calculated automatically. If no nbound are defined then a standard deviation range must be chosen (see sd_ber).
#' 
#' @param fix_sim
#' Which design should be applied and presented? 
#' "fixed": Design with fixed sample size, 
#' "sim": Design with internal pilot study,
#' c("fix", "sim"): both design
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
sample_pow <- function (sd_ber, delta = 0, Delta, sd, test = 1, alpha = 0.05, beta = 0.2, prop = c(0.5, 0.7), 
                         adj = F,  regel = F, nbound = 500, fix_sim = c("fix", "sim"), simu = 10000){

  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  if (test == 1){
    test_n <- "one-sided"
  } else if (test == 2){
    test_n <- "two-sided"
  }
  
  if ("sim" %in% fix_sim){
    text <- paste("Settings:", "\n ","delta = ", delta, ", Delta = ", Delta, ", SD = ", sd,
                  ", N0 = ", N0, "\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta, 
                  ", nbound = ", nbound )
  } else if (!("sim" %in% fix_sim)){
    text <- paste("Settings:", "\n", "delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                  ", N0 = ", N0, ",\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta)
  }
  
  if (length(sd_ber) <= 2){
    sd_min <- (1 / 2 * sqrt(N0 * min(prop)) * 1 / (qnorm(1 - alpha) + qnorm(0.95)) * (Delta - delta)) 
    sd_max <- (1 / 2 * sqrt(nbound) * 1 / (qnorm(1 - alpha) + qnorm(0.18)) * (Delta - delta)) 
    sd_ber <- seq(sd_min, sd_max, (sd_max - sd_min) / 25)
  } else if (length(sd_ber) == 2){
    sd_ber <- seq(min(sd_ber), max(sd_ber), (max(sd_ber) - min(sd_ber)) / 25)
  }
  
  if ("fix" %in% fix_sim){
    N_pow_f <- fix_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test, 
                              alpha = alpha, beta = beta, simu = simu)
  }
  
  if ("sim" %in% fix_sim){
    N_pow_s <- sim_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test,
                              alpha = alpha, beta = beta, prop = prop, adj = adj, regel = regel, 
                              nbound = nbound, simu = simu)
  }
  
  library("ggplot2")
  library("gridExtra")
  library("grid")
  
  pplot <- ggplot()
  
  if ("fix" %in% fix_sim){
    N_fix <- data.frame(sd = sd_ber, N = N_pow_f[1, ])
    if (!("sim" %in% fix_sim)){
      pplot <- pplot + 
        geom_line(data = N_fix, aes(x = sd, y = N), col = "black")
    } else {
      pplot <- pplot + 
        geom_line(data = N_fix, aes(x = sd, y = N), col = "darkgrey")
    }
  }
  
  if ("sim" %in% fix_sim){
    for (j in 1:length(prop)){
      x <- c()
      y <- c()
      for (i in 1:length(sd_ber)){
        x <- c(x, N_pow_s[[i]][1:5, j])
        y <- c(y, rep(sd_ber[i], 5))
      }
      N_sim <- data.frame(N = x, sd = y)
      pplot <- pplot +  
        geom_boxplot(data = N_sim, aes(x = sd, y = N, group = sd), outlier.shape = NA, col = j, 
                     fill = j, alpha = 0.2)
    }
    pplot <- pplot + 
      coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]), ylim = c(0, nbound + nbound / 12))
  } else if ("fix" %in% fix_sim & !("sim" %in% fix_sim)){
    pplot <- pplot + 
      coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]))
  }
  
  if ("sim" %in% fix_sim){
    point_leg <- data.frame(x = rep(-1, 2 * (length(prop) + 1)), y = rep(-1, 2 * (length(prop) + 1)),
                            prop = rep(1:(length(prop) + 1), 2))
    pplot <- pplot +
      geom_line(data = point_leg, aes(x = x, y = y, color = factor(prop))) +
      labs(color = "") +
      scale_color_manual(labels = c("fix", paste("prop =",prop)), 
                         values = c("darkgray", 1:length(prop))) +
      theme(legend.key = element_rect(fill = "white"))
  } else {
    point_leg <- data.frame(x = c(-1, -1), y = c(-1, -1), prop = c(1, 1))
    
    pplot <- pplot +
      geom_line(data = point_leg, aes(x = x, y = y, color = factor(prop))) +
      labs(color = "") +
      scale_color_manual(labels = "fix", values = "black") +
      theme(legend.key = element_rect(fill = "white"))
  }

  pplot <- pplot +
    geom_hline(yintercept = N0, linetype = 2, col = "gray") +
    scale_y_continuous(name = "Sample Size") +
    scale_x_continuous(name = "Standard Deviation", 
                       breaks = seq(round(min(sd_ber)), round(max(sd_ber)), round((max(sd_ber) - min(sd_ber)) / 10))) +
    ggtitle("Sample Size") +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  powplot <- ggplot()
  
  if ("fix" %in% fix_sim){
    pow_fix <- data.frame(sd = sd_ber, N = N_pow_f[2, ])
    
    if (!("sim" %in% fix_sim)){
      powplot <- powplot + 
        geom_line(data = pow_fix, aes(x = sd, y = N), col = "black")
    } else {
      powplot <- powplot + 
        geom_line(data = pow_fix, aes(x = sd, y = N), col = "darkgrey")
    }
  }
  
  if ("sim" %in% fix_sim){
    for (j in 1:length(prop)){
      x <- c()
      for (i in 1:length(sd_ber)){
        x <- c(x, N_pow_s[[i]][6, j])
      }
      pow_sim <- data.frame(N = x, sd = sd_ber)
      powplot <- powplot + 
        geom_line(data = pow_sim, aes( x = sd, y = N ), col = j, size = 1)
    }
  }
  
  powplot <- powplot + 
    coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]), ylim = c(0, 1))
  
  if ("sim" %in% fix_sim){
    point_leg <- data.frame(x = rep(-1, 2 * (length(prop) + 1)), y = rep(-1, 2 * (length(prop) + 1)),
                            prop = 1:(length(prop) + 1))
    
    powplot <- powplot +
      geom_line(data = point_leg, aes(x = x, y = y, color = factor(prop))) +
      labs(color = "") +
      scale_color_manual(labels = c("fix", paste("prop =", prop)), 
                         values = c("darkgray", 1:length(prop))) +
      theme(legend.key = element_rect(fill = "white"))
  } else {
    point_leg <- data.frame(x = c(-1, -1), y = c(-1, -1), prop = 1)
    
    powplot <- powplot +
      geom_line(data = point_leg, aes(x = x, y = y, color = factor(prop))) +
      labs(color = "") +
      scale_color_manual(labels = "fix", values = "black") +
      theme(legend.key = element_rect(fill = "white"))
  }
  
  powplot <- powplot +
    geom_hline(yintercept = 0.8, linetype = 2, col = "gray") +
    geom_hline(yintercept = 0.2, linetype = 2, col = "gray") +
    scale_y_continuous(name = "Power", breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(name = "Standard Deviation", 
                       breaks = seq(round(min(sd_ber)), round(max(sd_ber)), 
                                    round((max(sd_ber) - min(sd_ber)) / 10))) +
    ggtitle("Power") +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  grid.arrange(
    pplot,
    powplot,
    nrow = 1,
    ncol = 2,
    top = "Sample Size and Power",
    bottom = textGrob(text, gp = gpar(fontface = 1, fontsize = 9), x = 0, hjust = -0.1
    )
  )
  
}
