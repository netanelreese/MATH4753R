#' ntickets
#'
#' @param N number of seats on a flight
#' @param gamma probibility that a flight will be overbooked
#' @param p probability for a "show"
#'
#'
#'
#' @returns list - a list containing nc the optimum number of tickets sold in a continuious manner, nd a distributed
#' @export
#'
ntickets = function(N, gamma,p){
  domain <- N:(N + round(0.1 * N))
  nd <- 1 - gamma - pbinom(q = N, size = domain, prob = p) # discrete distribution
  nc <- 1 - gamma - pnorm(N + 0.5,  domain * p, sqrt(domain * p * (1 - p))) # continuous distribution

  # Find the optimal minimum of obj
  d <- N
  while(N != qbinom(1-gamma, d, p)){
    d = d + 1
  }

  c <- which.min(abs(nc))



  plot(domain, nd, type = 'b', main=paste("Objective Vs n to find optimal tickets sold\n", "(", d, ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective", pch = 16)
  abline(h = 0, v = d, lwd = 2, col = "coral")

  plot(domain, nc, type = 'l', main=paste("Objective Vs n to find optimal tickets sold\n", "(", domain[c], ")", "gamma = ", gamma, "N = ", N, "continuous"), ylab = "Objective")
  abline(h = 0, v = domain[c], col = "lavender", lwd = 2)

  list <- list(nd=d,nc=c, N=N, p=p, gamma=gamma)
  print(list)
  #return(list)
}
#0 errors ✔ | 0 warnings ✔ | 3 notes ✖

#R CMD check succeeded
