## Wijayatunga coefficient permutation test
wcor.test <- function(x, y=NULL, mv=NULL, iterations=100, seed=1234) {
  ### Set number of iterations and random seed for replicability
  R = iterations
  set.seed(seed)

  ### Check if data is ok
  if ((is.matrix(x) & (ncol(x) > 1)) | (is.data.frame(x) & (ncol(x) > 1))) {
    data <- x
  } else if(is.vector(x) & is.vector(y)) {
    data <- cbind(x, y)
  } else {stop("Something is not correct. Please check function's documentation.")}

  ### Progress message
  cat(paste("Simulate ",R," independent datasets and return p-values.", sep="")); cat("\n")
  pb <- txtProgressBar(min = 0, max = R, style = 3)

  ### Generate independent observations
  loops <- lapply(1:R, function(g) {
    temp0 <- sapply(1:ncol(data), function(g) {
      sample(sort(unique(data[,g])), nrow(data),
             prop.table(table(data[,g])), replace=T)
    })
    return(temp0)
  })

  ### Get W values for each iteration
  n_dist <- sapply(1:R, function(g) {
    temp <- wcor(loops[[g]])[lower.tri(cor(loops[[g]]))]
    setTxtProgressBar(pb, g)
    return(temp)
  })
  close(pb)

  ### Calculate and return p-values in a symmetric matrix
  WCM <- wcor(data)[lower.tri(cor(data))]
  tempO <- sapply(1:nrow(n_dist), function(g) {
    1 - (length(which(n_dist[g,] < WCM[g]))/R)
  })
  correlation_vector <- tempO
  wcorm <- matrix(0, nrow=ncol(data), ncol=ncol(data))
  wcorm[ col(wcorm) < row(wcorm) ] <- correlation_vector
  wcorm <- wcorm + t(wcorm)
  diag(wcorm) <- 1
  colnames(wcorm) <- rownames(wcorm) <- colnames(data)
  return(wcorm)
}
