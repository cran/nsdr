#' mppower
#'
#' @usage mppower(matrix,power,ignore)
#' @param matrix input matrix
#' @param power power
#' @param ignore ignoring criterion
#'
#' @return Moore penrose inverse
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.
#' @export
#'
#' @examples
#' a <- matrix(rnorm(4,0,1),2,2)
#' mppower(a,-1,0.2)
mppower=function(matrix,power,ignore){
  eig = eigen(matrix)
  eval = eig$values
  evec = eig$vectors
  m = length(eval[abs(eval)>ignore])
  tmp = evec[,1:m]%*%diag(eval[1:m]^power)%*%t(evec[,1:m])
  return(tmp)
}
