int_D <- function (delta = 0, Delta = 4, alpha = 0.05, beta = 0.2, simu = 10000, adj = F,
                 regel = F, prop = c(0.5, 0.7), test = 1, nbund = 300, nmin = 65){
  
  if (test == 1){
    N0 <- ceiling(4 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 * sd^2 / (Delta - delta)^2)
  } else {
    N0 <- ceiling(4 * (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * sd^2 / Delta^2)
  }
  
  text <- paste("Settings: \n", "\n delta = ", delta, ", Delta = ", Delta, ", SD = ", sd, 
                ", test = one-sided,", "\n alpha = ", alpha, ", beta = ", beta, ",", 
                "\n nbound = ", nbound, ", N0 = ", N0)
  
  layout(matrix(c(1, 1, 2, 2, 2, 1, 1, 3, 3, 3), 5, 2))
  
  sd_min <- (1 / 2 * sqrt(N0 * min(prop)) * 1 / (qnorm(1 - alpha) + qnorm(0.95)) * (Delta - delta)) 
  sd_max <- (1 / 2 * sqrt(nbound) * 1 / (qnorm(1 - alpha) + qnorm(0.18)) * (Delta - delta)) 
  sd_ber <- seq(sd_min, sd_max, (sd_max - sd_min) / 25)

  N_pow_s <- sim_sample_pow(sd_ber = sd_ber, delta = delta, Delta = Delta, sd = sd, test = test, alpha = alpha, beta = beta,
                     prop = prop, adj = adj, regel = regel, nbound = nbound, simu = simu)
  
  plot(1, 2, axes = F, type = "n", xlab = "", ylab = "")
  legend(0.755, 0.6, c("prop = 0.2", "prop = 0.5", "prop = 0.7"), pch = c(1, 1, 1),  
         xpd = T, horiz = T, bty = "n", col = 1:3, text.col = 1:3)
  mtext(text, side = 1, cex = 0.8)
  
  ##Fallzahl
  plot(1, 1, c(min(sd_ber), max(sd_ber)), ylim = c(0, nbound), type = "n", main = "Sample Size \n standard deviation", 
       xlab = "standard deviation", ylab = "Sample Size")
  for (j in 1:3){
    for (i in 1:length(sd_ber)){
      points(sd_ber[i], N_pow_s[[i]][1, j], col = j, pch = 19, cex = 0.9)}}
  
  ##Power
  plot(1, 1, c(sd_min, sd_max), ylim = c(0, 1), type = "n", main = "Power \n standard deviation", 
       xlab = "standard deviation", ylab = "Power")
  for (j in 1:3){
    for (i in 1:length(sd_ber)){
      points(sd_ber[i], N_pow_s[[i]][2, j], col = j, pch = 19, cex = 0.9)}}
  abline(h = 0.8, lty = 2, col = "grey")
  abline(h=0.2, lty = 2, col = "grey")
  
}
