partial.w <- function(x) {
  
  # Invert the dependence matrix
  if (ncol(x) == nrow(x)) {
    R <- x
  } else {
    R <- wcorm(x)
  }
  ind <- unique(dim(R))
  R_inv <- solve(R)
  
  # Inverses of the diagonal elements of the inverse of R
  ZM <- matrix(0, nrow=ind, ncol = ind)
  diag(ZM) <- diag(R_inv)
  D <- solve(ZM)
  
  # Anti-image dependence matrix
  AICOV <- D %*% R_inv %*% D
  diag(ZM) <- diag(AICOV)
  D  <- solve(sqrt(ZM))
  
  # Partial dependence matrix
  AICOR <- D %*% AICOV %*% D
  pcor <- AICOR
  pcor[upper.tri(pcor)] <- -pcor[upper.tri(pcor)]
  pcor[lower.tri(pcor)] <- -pcor[lower.tri(pcor)]
  dimnames(pcor) <- list(colnames(R), colnames(R))
  
  return(pcor)
}  
