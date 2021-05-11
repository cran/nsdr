#' gramx
#'
#' @usage gramx(x,complexity)
#' @param x data
#' @param complexity tuning parameter in Gaussian kernel
#'
#' @return gram matrix Q x KX x Q
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' vec <- matrix(rnorm(4),2,2)
#' res <- gramx(vec,1)
gramx=function(x,complexity){
  n=dim(x)[1]
  k2=x%*%t(x);k1=t(matrix(diag(k2),n,n));k3=t(k1);k=k1-2*k2+k3
  sigma=(sum(sqrt(k))-tr(sqrt(k)))/(2*choose(n,2));gamma=1/(sigma^2)
  return(exp(-complexity*gamma*(k1-2*k2+k3)))}
