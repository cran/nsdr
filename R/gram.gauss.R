#' gram.gauss
#'
#' @usage gram.gauss(x,x.new,complexity)
#' @param x previous observations
#' @param x.new new observations
#' @param complexity tuning parameter in Gaussian kernel
#'
#' @return Gram matrix for the Gaussian kernel. This also can be used to project predictor on the testing set.
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' old <- matrix(c(1,2,3,4),2,2)
#' new <- matrix(c(5,6,7,8),2,2)
#' result <- gram.gauss(old,new,1)
gram.gauss=function(x,x.new,complexity){
  x=as.matrix(x);x.new=as.matrix(x.new)
  n=dim(x)[1];m=dim(x.new)[1]
  k2=x%*%t(x);k1=t(matrix(diag(k2),n,n));k3=t(k1);k=k1-2*k2+k3
  sigma=sum(sqrt(k))/(2*choose(n,2));gamma=complexity/(2*sigma^2)
  k.new.1=matrix(diag(x%*%t(x)),n,m)
  k.new.2=x%*%t(x.new)
  k.new.3=matrix(diag(x.new%*%t(x.new)),m,n)
  return(exp(-gamma*(k.new.1-2*k.new.2+t(k.new.3))))
}
