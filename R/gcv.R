#' gcv
#'
#' @usage gcv(x,y,eps,which,ytype,complex.x,complex.y)
#' @param x input predictor matrix from training set
#' @param y response variables
#' @param eps candidate
#' @param which choose between ex and ey
#' @param ytype type of response variables
#' @param complex.x tuning parameter for the Gaussian kernel in X
#' @param complex.y tuning parameter for the Gaussian kernel in Y
#'
#' @return gcv criterion
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' n = 50; p = 5; sigma = 1;
#' x = matrix(rnorm(n*p),n,p) ; err = rnorm(n)
#' y = (x[,1]+1)^2 + sigma*err; ex=0.01 ; ey=0.01; candidate=0.01
#' epsx <- gcv(x,y,candidate,"ex", "categorical",1,1)
#' epsy <- gcv(x,y,candidate,"ey", "categorical",1,1)
gcv <- function(x,y,eps,which,ytype,complex.x,complex.y){
  p=dim(x)[2]; n=dim(x)[1]
  Kx=gram.gauss(x,x,complex.x)
  if(ytype=="scalar") Ky=gram.gauss(y,y,complex.y)
  if(ytype=="categorical") Ky=gram.dis(y)
  if(which=="ey"){G1=Kx;G2=Ky}
  if(which=="ex"){G1=Ky;G2=Kx}
  G2inv=matpower(G2+eps*onorm(G2)*diag(n),-1)
  nu=sum((G1-G2%*%G2inv%*%G1)^2)
  de=(1-tr(G2inv%*%G2)/n)^2
  return(nu/de)
}
