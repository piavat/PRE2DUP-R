#' Example drug purchase data
#'
#' A synthetic dataset containing drug purchase records for five individuals and two medications,
#' intended for demonstration and testing of the PRE2DUP-R package.
#'
#' @format A `data.frame` and `data.table` with the following variables:
#' \describe{
#'   \item{id}{Person identifier (integer)}
#'   \item{vnr}{Package identifier of the purchased product, Nordic article number (Vnr) in this example (character)}
#'   \item{ATC}{Anatomical Therapeutic Chemical (ATC) classification code (character)}
#'   \item{purchase_date}{Date of the purchase (Date)}
#'   \item{n_packages}{Amount of purchased product, as number of packages in this example (numeric)}
#'   \item{amount}{Estimated total DDDs of the purchase (numeric)}
#' }
#'
#' @details
#'
#' See more about ATC classification at \url{https://atcddd.fhi.no/atc/structure_and_principles/} and about DDDs at \url{https://atcddd.fhi.no/ddd/definition_and_general_considera/}.
#'
#' @seealso
#' \code{\link{pre2dup}}, \code{\link{check_purchases}}
#'
#' @source Simulated data created for package documentation.
"purchases_example"
