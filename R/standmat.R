#' standmat
#'
#' @usage standmat(x)
#' @param x matrix
#'
#' @return standardized matrix
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(4),2,2)
#' standmat(mat)
standmat=function(x) {
  return(t((t(x)-apply(x,2,mean))/apply(x,2,sd)))
}

utils::globalVariables(c("stats", "sd"))
