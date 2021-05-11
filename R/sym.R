#' sym
#'
#' @usage sym(a)
#' @param a any matrix
#'
#' @return symmetrize matrix when matrix is theoretically symmetric but not in numerically
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' ex <- matrix(c(1.1,2.1,1.2,2.2),2,2)
#' result <- sym(ex)
sym=function(a) {
  return(round((a+t(a))/2,9))
}
