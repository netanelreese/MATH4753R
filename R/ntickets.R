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
  binom <- function(n) {
    nd <- 1 - gamma - pbinom(N/2,round(n),p)
  }
  norm <- function(n) {
    nc <- 1 - gamma - pnorm(N/2, n * p, sqrt(n * p * (1 - p)))
  }

  x = seq.int(0, N, 1L)

  nd <- 1 - gamma - pbinom(N/2,round(x),p)
  nc <- 1 - gamma - pnorm(N/2, x * p, sqrt(x * p * (1 - p)))

  # Find the optimal minimum of obj
  nc <- optimize(norm, interval = c(204, N/2+20))

  dmin = which.min(abs(nd))
  cmin = nc$minimum


  #layout(matrix(2:1, nr=2,nc=1))


  plot(binom(x), xlim = c(N/2,N/2+20),ylim=c(0,1),xlab="n",ylab = "Objective
       ",type='b',pch=21, main=paste("Objective v. n to find Optimal Tickets Sold\n(",dmin, "), gamma: ",gamma,", N: ", N, ", Discrete"))
  points(dmin,nd[dmin], pch = 21, bg = "Red", cex = 2)


  curve(norm(x),xlim = c(N/2,N/2+20),ylim=c(0,1),xlab="n",ylab = "Objective
       ", main=paste("Objective v. n to find Optimal Tickets Sold\n(",cmin, "), gamma: ",gamma,", N: ", N, ", Continuous"))  # add a continuous plot of f to the existing plot
  points(nc$minimum,nc$objective, pch = 21, bg = "Red", cex = 2)
  print(list(nd=dmin,nc=cmin, N=N, p=p, gamma=gamma))
}
