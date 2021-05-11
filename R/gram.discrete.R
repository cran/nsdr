#' gram.dis
#'
#' @usage gram.dis(y)
#' @param y discrete vector
#'
#' @return gram matrix for discrete vector
#' @references Li, B. (2018). Sufficient dimension reduction: Methods and applications with R. CRC Press.

#' @export
#'
#' @examples
#' toy <- c(1,2,3,1)
#' result=gram.dis(toy)
gram.dis=function(y){
  n=length(y);yy=matrix(y,n,n);diff=yy-t(yy);vecker=rep(0,n^2)
  vecker[c(diff)==0]=1;vecker[c(diff)!=0]=0
  return(matrix(vecker,n,n))}
