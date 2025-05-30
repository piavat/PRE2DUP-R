% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/purchases_example.R
\docType{data}
\name{purchases_example}
\alias{purchases_example}
\title{Example drug purchase data}
\format{
A `data.frame` and `data.table` with the following variables:
\describe{
  \item{id}{Person identifier (integer)}
  \item{vnr}{Package identifier of the purchased product, Nordic article number (Vnr) in this example (character)}
  \item{ATC}{Anatomical Therapeutic Chemical (ATC) classification code (character)}
  \item{purchase_date}{Date of the purchase (Date)}
  \item{n_packages}{Amount of purchased product, as number of packages in this example (numeric)}
  \item{amount}{Estimated total DDDs of the purchase (numeric)}
}
}
\source{
Simulated data created for package documentation.
}
\usage{
purchases_example
}
\description{
A synthetic dataset containing drug purchase records for five individuals and two medications,
intended for demonstration and testing of the PRE2DUP-R package.
}
\details{
See more about ATC classification at \url{https://atcddd.fhi.no/atc/structure_and_principles/} and about DDDs at \url{https://atcddd.fhi.no/ddd/definition_and_general_considera/}.
}
\seealso{
\code{\link{pre2dup}}, \code{\link{check_purchases}}
}
\keyword{datasets}
