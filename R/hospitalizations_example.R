#' Example hospitalization data
#'
#' A synthetic dataset containing hospitalization periods for two individuals. Intended for testing
#' how hospital stays affect drug exposure estimation in the PRE2DUP-R package.
#'
#' @format A `data.table` with the following variables:
#' \describe{
#'   \item{id}{Person identifier (integer)}
#'   \item{hospital_start}{Start date of the hospitalization period (Date)}
#'   \item{hospital_end}{End date of the hospitalization period (Date)}
#' }
#'
#' @details
#'
#' This data can be used to test how PRE2DUP-R handles inpatient periods when outpatient medications are not used.
#'
#' @seealso
#' \code{\link{pre2dup}}, \code{\link{check_hospitalizations}}

#'
#' @source Simulated data created for package documentation.
"hospitalizations_example"
