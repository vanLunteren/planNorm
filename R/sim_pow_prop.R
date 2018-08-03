sim_pow_prop <- function (sd = 8, delta = 0, alpha = 0.05, beta = 0.2, Delta = c(3, 4, 5), 
                     prop = c(0.1, 1.2), simu = 10000, test = 1, adj = F, regel = F, 
                     nbound = 300, nmin = 65){
  par(mfrow = c(1,1))
  
  prop_area <- seq(min(prop), max(prop), (max(prop) - min(prop)) / 20 )
  pow_prop <- sim_pow(sd = sd, delta = delta, alpha = alpha, beta = beta, Delta = Delta,
                      prop = prop_area, simu = simu, test = test, adj = adj, 
                      regel = regel, nbound = nbound, nmin = nmin)
  
  ##Power
  plot(1, 1, c(min(prop), max(prop)), ylim = c(0.6, 0.95), type="n", main = "Power",
       xlab = "Timing (n1/N0)", ylab = "Power")
  
  for (j in 1:length(Delta)){
    points(prop_area, pow_prop[[j]], col = j)
  }
  abline(h = 0.8, lty = 2)
  legend("bottomright", paste("Delta = ", Delta), pch = rep(1, 4) , bty = "n",
         col = 1:4, text.col = 1:4, cex = 0.6, pt.cex = 1)
}