#' Title
#'
#' @param mu -  blah blah
#' @param sigma - blooh blooh
#'
#' @return - uhhh
#'
#' @examples - myncurve(1, 2, 4, "blue")
myncurve = function(mu, sigma,color,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(a-100,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(a-100,xcurve,a),c(0,ycurve,0),col=color)
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(a-100,mean=mu,sd=sigma)
  prob=round(prob,4)
  print(prob)
}
