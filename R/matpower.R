#' matpower
#'
#' @usage matpower(a,alpha)
#' @param a matrix
#' @param alpha power
#'
#' @return power of a matrix
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(4,1,3),2,2)
#' invmat <- matpower(mat,-1)
matpower = function(a,alpha){
  a = (a + t(a))/2
  tmp = eigen(a)
  return(tmp$vectors%*%diag((tmp$values)^alpha)%*%
           t(tmp$vectors))}
