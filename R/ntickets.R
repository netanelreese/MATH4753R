#' nTickets
#'
#' @param mu
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
ntickets = function(N, gamma,p){
  x = seq(0, N, by = 1)
  x2 <- seq(0, N, by = 0.1)

  nd <- 1 - gamma - pbinom(N/2,x,p)
  nc <- 1 - gamma - pnorm(N/2, x2 * p, sqrt(x2 * p * (1 - p)), lower.tail = TRUE)


  dmin = which.min(abs(nd))
  cmin <- min(nc)

  results <- list(nd = nd[dmin],
                  nc = nc[cmin],
                  N = N, p = p, gamma = gamma)


  binom <- function(n) {
    nd <- 1 - gamma - pbinom(N/2,round(n),p)
  }
  norm <- function(n) {
    nc <- 1 - gamma - pnorm(N/2, n * p, sqrt(n * p * (1 - p)))
  }


  plot(binom(x), xlim = c(N/2,N/2+20),ylim=c(0,1),xlab="n",ylab = "Objective
       ",type='b',pch=21, main=paste("Objective v. n to find Optimal Tickets Sold\n(",dmin, "), gamma: ",gamma,", N: ", N, ", Discrete"))
  points(dmin,nd[dmin], pch = 21, bg = "Red", cex = 2)


  curve(norm,xlim = c(N/2,N/2+20),ylim=c(0,1),xlab="n",ylab = "Objective
       ", main=paste("Objective v. n to find Optimal Tickets Sold\n(",cmin, "), gamma: ",gamma,", N: ", N, ", Continuous"))  # add a continuous plot of f to the existing plot
  points(cmin / p, nc[cmin], pch = 21, bg = "Blue", cex = 2)
  print(results)
}
