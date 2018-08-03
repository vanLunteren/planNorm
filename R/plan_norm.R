
delta <- 0
Delta <- 4
alpha <- 0.025
beta <- 0.2
simu <- 1000
test <- 1
nbound <- 300
nmin<- 65


fix_D(delta, Delta, alpha, beta, simu, test, nbound, nmin)

adj <- F
regel <- "WB"
prop <- c(0.2,0.5,0.7)

int_D(delta, Delta, alpha, beta, simu, adj, regel, prop, test, 500, nmin)



sd <- 8
prop <- seq(0.1, 1.5, 0.1)

sim_pow_prop(sd, delta, alpha, beta, Delta, prop, simu, test, adj, regel, nbound, nmin)




