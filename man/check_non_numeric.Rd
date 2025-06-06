% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_helpers.R
\name{check_non_numeric}
\alias{check_non_numeric}
\title{Check Non-Numeric Values}
\usage{
check_non_numeric(col)
}
\arguments{
\item{col}{a character vector to be checked for non-numeric values.}
}
\description{
This function checks a character vector for non-numeric values. It returns the indices of elements that are either NA or do not match the alphanumeric pattern. It is used internally in the package to validate character columns that are expected to contain only alphanumeric values.
}
\keyword{internal}
