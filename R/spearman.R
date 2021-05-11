#' standmat
#'
#' @usage spearman(x1,x2)
#' @param x1 first argument
#' @param x2 second argument
#' @return standardized matrix
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#' @importFrom stats cor
#' @examples
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' spearman(x1,x2)
spearman = function(x1,x2){
  return(abs(cor(rank(x1),rank(x2))))}
