#' trace
#'
#' @usage tr(a)
#' @param a any matrix
#'
#' @return trace value of the matrix
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' mat <- matrix(2,2,2)
#' tr(mat)
tr=function(a){ return(sum(diag(a)))}
