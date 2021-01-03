## Wijayatunga coefficient
W <- function(x, y, mv=NULL) {
  if (is.null(mv)) {mv = max(max(x),max(y))}
  # All possible X
  values_x <- sort(unique(x))
  x_perms  <- tryCatch(combinat::permn(values_x), error=function(e) NULL)

  if (length(x_perms) == 0) {
    ### If vectors are too large
    # Fixed X maximal dependence
    PX <- H(x, y, md=F, mv=mv)
    # Fixed Y maximal dependence
    PY <- H(y, x, md=F, mv=mv)
    # Wijayatunga Coefficient
    return( H(x, y, mv=mv) / sqrt( (PX * PY) ) )

  } else {
    ### If vectors are of reasonable enough size
    x_index  <- lapply(sort(unique(x)), function(g) which(x == g))
    change_x <- lapply(values_x, function(l) which(x == l))
    all_xPerm <- matrix(NA, nrow=length(x), ncol=length(x_perms))
    for (i in 1:ncol(all_xPerm)) {
      for (j in 1:length(change_x))
        all_xPerm[change_x[[j]],i] <- x_perms[[i]][j]
    }
    ## All possible y
    values_y <- sort(unique(y))
    y_perms  <- combinat::permn(values_y)
    y_index  <- lapply(sort(unique(y)), function(g) which(y == g))
    change_y <- lapply(values_y, function(l) which(y == l))
    all_yPerm <- matrix(NA, nrow=length(y), ncol=length(y_perms))
    for (i in 1:ncol(all_yPerm)) {
      for (j in 1:length(change_y))
        all_yPerm[change_y[[j]],i] <- y_perms[[i]][j]
    }
    # Fixed Y maximal dependence
    PX <- sapply(1:ncol(all_xPerm), function(g) H(x, y, md=T, ymax=all_xPerm[,g]))
    HX <- prod(PX) ^ (1/length(PX))
    # Fixed X maximal dependence
    PY <- sapply(1:ncol(all_yPerm), function(g) H(y, x, md=T, ymax=all_yPerm[,g]))
    HY <- prod(PY) ^ (1/length(PY))
    # Wijayatunga Coefficient
    return( H(x, y, mv=mv) / sqrt( (HX * HY) ) )
  }
}
