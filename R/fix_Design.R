fix_D <- function (delta = 0, Delta = 4, alpha = 0.05, beta = 0.2, simu = 10000, test = 1, nbound = 300, nmin = 65){
  
  text <- paste("Settings: \n", "\n delta = ", delta, ", Delta = ", Delta, ", alpha = ",
              alpha, ", beta = ", beta, ",", "\n nbound = ", nbound, ", nmin = ", nmin)
  
  
  layout(matrix(c(1, 1, 2, 2, 2, 1, 1, 3, 3, 3), 5, 2))

  sd_min <- (1 / 2 * sqrt(nmin) * 1 / (qnorm(1 - alpha) + qnorm(1 - beta)) * (Delta - delta)) -1
  sd_max <- (1 / 2 * sqrt(nbound) * 1 / (qnorm(1 - alpha) + qnorm(1 - beta)) * (Delta - delta)) / 0.35
  sd_ber <-seq(sd_min, sd_max, (sd_max - sd_min) / 25)
  
  N_pow <- fix_sample_pow(sd = sd_ber, delta = delta, alpha = alpha, beta = beta, Delta = Delta, 
                       simu = simu, test = test, nbound = nbound, nmin = nmin)
  
  plot(1, 1, axes = F, type = "n", xlab = "", ylab = "")
  legend(0.755, 0.6, c("prop = 0.2", "prop = 0.5", "prop = 0.7"), pch = c(1, 1, 1),  
         xpd = T, horiz = T, bty = "n", col = 1:3, text.col = 1:3)
  mtext(text, side = 1, cex = 0.8)
  
  ##Fallzahl
  plot(1, 1, c(sd_min, sd_max), ylim = c(0, nbound), type = "n", main = "Sample Size \n standard deviation", 
       xlab = "standard deviation", ylab = "Sample Size")
  for (i in 1:length(sd_ber)){
    points(sd_ber[i], N_pow[[i]][1])
  }
  
  ##Power
  plot(1, 1, c(sd_min, sd_max), ylim = c(0, 1), type = "n", main = "Power \n standard deviation", 
       xlab = "standard deviation", ylab = "Power")
  for (i in 1:length(sd_ber)){
    points(sd_ber[i], N_pow[[i]][2])}
  abline(h = 0.8, lty = 2)
  abline(h = 0.2, lty = 2)
}
