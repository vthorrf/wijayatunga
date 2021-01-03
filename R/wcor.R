## Wijayatunga coefficient
wcor <- function(x, y=NULL, mv=NULL) {
  if ((is.matrix(x) & (ncol(x) > 1)) | (is.data.frame(x) & (ncol(x) > 1))) {
    return(wcorm(x))
  } else if(is.vector(x) & is.vector(y)) {
    return(W(x,y,mv))
  } else {stop("Something is not correct. Please check function's documentation.")}
}
