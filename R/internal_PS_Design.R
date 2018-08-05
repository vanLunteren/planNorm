sample_pow <- function (delta = 0, Delta = 4, alpha = 0.05, beta = 0.2, simu = 10000, adj = F,
                 regel = F, prop = c(0.5, 0.7), test = 1, nbound = 300, fix_sim = c("fix", "sim")){
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  if (test == 1){
    test_n <-"one-sided"
  } else if (test == 2){
    test_n <-"two-sided"
  }
  
  if ("sim" %in% fix_sim){
    text <- paste("Settings:", "\n ","delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                  ", N0 = ", N0, "\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta, 
                  ", nbound = ", nbound )
  } else if (!("sim" %in% fix_sim)){
    text <- paste("Settings:", "\n", "delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                  ", N0 = ", N0, ",\ntest = ", test_n, ", alpha = ", alpha, ", beta = ", beta)
  }
  
  if (!(is.numeric(sd_ber))){
    sd_min <- (1 / 2 * sqrt(N0 * min(prop)) * 1 / (qnorm(1 - alpha) + qnorm(0.95)) * (Delta - delta)) 
    sd_max <- (1 / 2 * sqrt(nbound) * 1 / (qnorm(1 - alpha) + qnorm(0.18)) * (Delta - delta)) 
    sd_ber <- seq(sd_min, sd_max, (sd_max - sd_min) / 25)
  } else if(length(sd_ber) == 2){
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
      coord_cartesian(xlim = c(sd_ber[1], sd_ber[length(sd_ber)]), ylim = c(0, nbound + nbound/12))
  } else if("fix" %in% fix_sim & !("sim" %in% fix_sim)){
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
                       breaks = seq(round(min(sd_ber)), round(max(sd_ber)), round((max(sd_ber) 
                                                                          - min(sd_ber)) / 10))) +
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
