## Theil's U [uncertainty coefficient]
U <- function(x, y) {
  Ucx <- infotheo::mutinformation(x, y) / infotheo::entropy(x)
  Ucy <- infotheo::mutinformation(y, x) / infotheo::entropy(y)

  return(
    (infotheo::entropy(x) * Ucx) + (infotheo::entropy(y) * Ucy) /
      (infotheo::entropy(x) + infotheo::entropy(y))
  )
}
