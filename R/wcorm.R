# Wijayatunga coefficient matrix
wcorm <- function(data) {
  listV <- lapply(1:(ncol(data)-1), function(g) c(1:ncol(data))[-(1:g)])
  WCM <- lapply(seq_len(length(listV)), function(r) {
    lapply(listV[[r]], function(c) {
      W(data[,r], data[,c], mv=max(data))
    })
  })
  correlation_vector <- unlist(WCM)
  wcorm <- matrix(0, nrow=ncol(data), ncol=ncol(data))
  wcorm[ col(wcorm) < row(wcorm) ] <- correlation_vector
  wcorm <- wcorm + t(wcorm)
  diag(wcorm) <- 1
  colnames(wcorm) <- rownames(wcorm) <- colnames(data)
  wcorm
}
