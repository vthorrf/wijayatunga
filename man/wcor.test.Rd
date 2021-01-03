\name{wcor.test}
\alias{wcor.test}
\title{Permutation test for Wijayatunga coefficient}
\description{
Estimates de p-value for calculated Wijayatunga coefficients (Wijayatunga, 2016) for the given variables.
}
\usage{
wcor.test(x, y=NULL, mv=NULL, iterations=100, seed=1234)
}
\arguments{
   \item{x}{A vector or a matrix/data frame of categorical variables.}
   \item{y}{A vector of categorical values. It is not necessary if x is a matrix or a data frame with more than one column.}
   \item{mv}{The maximum number of different categories (it is considered to be the same for all variables in x, or for x and y). Should be used only the maximum possible value was not used in any of the variables in x, or in x or y.}
   \item{iterations}{The number of iterations to be calculated. Defaults to 100 (but in practical research it should be at least 1000).}
   \item{seed}{The random seed for replicability purposes.}
}
\value{
A matrix of p-values for the Wijayatunga coefficients.
}
\examples{
### Data generating process
DGP <- function(seed, n) {
  set.seed(seed)

  tmp <- copula::indepCopula(dim = 2)                  # Independence copula
  cop <- copula::rCopula(n, tmp)                       # Uncorrelated variables
  cp3 <- pnorm(qnorm( sqrt(cop[,1] * cop[,2]) ))       # Collider
  data <- data.frame( "v1"=qbinom(cop[,1], 4, .7) + 1, # Combine in same dataset
                      "v2"=qbinom(cop[,2], 4, .5) + 1,
                      "v3"=qbinom(cp3, 4, .3) + 1 )
  return(data)
}

### Simulate data
data <- DGP(1, 200)

### Get W coefficients
W_pvalue <- wcor.test(data, iterations=100, seed=169)
}
\references{
  Wijayatunga, P. (2016). "A geometric view on Pearson's correlation
  coefficient and a generalization of it to non-linear dependencies".
  *Ratio Mathematica*, 30, 3-21.
}
