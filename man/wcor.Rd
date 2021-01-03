\name{wcor}
\alias{wcor}
\title{Wijayatunga coefficient}
\description{
Calculates the Wijayatunga coefficient (Wijayatunga, 2016) for the given variables.
}
\usage{
wcor(x, y=NULL, mv=NULL)
}
\arguments{
   \item{x}{A vector or a matrix/data frame of categorical variables.}
   \item{y}{A vector of categorical values. It is not necessary if x is a matrix or a data frame with more than one column.}
   \item{mv}{The maximum number of different categories (it is considered to be the same for all variables in x, or for x and y). Should be used only the maximum possible value was not used in any of the variables in x, or in x or y.}
}
\value{
A matrix of Wijayatunga coefficients (or a numeric vector of length 1 if both x and y were provided).
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
W <- wcor(data)
}
\references{
  Wijayatunga, P. (2016). "A geometric view on Pearson's correlation
  coefficient and a generalization of it to non-linear dependencies".
  *Ratio Mathematica*, 30, 3-21.
}
