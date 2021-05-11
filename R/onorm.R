#' onorm
#'
#' @usage onorm(a)
#' @param a input matrix
#'
#' @return result of the operator norm
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' a <- matrix(c(1,2,3,4),2,2)
#' result <- onorm(a)
onorm=function(a){
  return(eigen(round((a+t(a))/2,8))$values[1])
  }
