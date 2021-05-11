#' ridgepower
#'
#' @usage ridgepower(a,e,c)
#' @param a square matrix
#' @param e tuning parameter
#' @param c power
#'
#' @return matrix with the power
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' x <- matrix(c(1:4),2,2)
#' result <- ridgepower(x, 0.001, -1)
ridgepower=function(a,e,c){
  return(matpower(a+e*onorm(a)*diag(dim(a)[1]),c))
}
