## Hellinger distance
H <- function(x, y, mv=NULL, md=F, ymax) {
  if (is.null(mv)) {mv = max(max(x),max(y))}
  ## Hellinger distance if data is Discrete
  level = seq_len(mv)
  if (md == T) {
    freqs = table(factor(x, levels=level), factor(ymax, levels=level))
  } else {
    freqs = table(factor(x, levels=level), factor(y, levels=level))
  }
  indep <- suppressWarnings( chisq.test(table(factor(x, levels=level),
                                              factor(y, levels=level)))$expected )
  # Bhattacharyya coefficient
  BC <- sum(sqrt(as.vector(prop.table(indep)) * as.vector(prop.table(freqs))))
  return(sqrt( 1 - BC ))
}
