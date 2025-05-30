#' Example of package parameters
#'
#' An example dataset of package parameters.
#'
#' @format A `data.frame` with the following variables:
#' \describe{
#'   \item{vnr}{Package identifier of the purchased product, Nordic article number (Vnr) in this example (numeric)}
#'   \item{ATC}{Anatomical Therapeutic Chemical (ATC) classification code (character)}
#'   \item{product_name}{Product name (character)}
#'   \item{strength}{Strength of drug in format register may deliver, including e.g. the drug strength and value type (e.g. mg) (character)}
#'   \item{strength_num}{Numeric strength of drug, extracted from \code{strength} (numeric)}
#'   \item{packagesize}{Package size in format register may deliver, including e.g. number and type of units (character)}
#'   \item{packsize_num}{Number of units extracted from \code{packagesize} (numeric)}
#'   \item{drug_form_harmonized}{Harmonized drug format (e.g. 'tabs' and 'pills' recorded as 'TABLET') (character)}
#'   \item{ddd_per_pack}{Number of Defined Daily Doses (DDDs) in package (numeric)}
#'   \item{minimum_dur}{Minimum duration of package (numeric)}
#'   \item{usual_dur}{Usual duration of package (numeric)}
#'   \item{maximum_dur}{Maximum duration of package (numeric)}
#'   \item{lower_ddd}{Lower limit of daily dose of package as DDDs (numeric)}
#'   \item{usual_ddd}{Usual daily dose of package as DDDs (numeric)}
#' }
#'
#' @details
#' PRE2DUP-R uses package-specific information to estimate drug exposure. Example parameters can be used as a template for creation of study specific package parameter file. PRE2DUP-R requires the package identifier, minimum, usual and maximum duration of the package, and daily dose's lower limit and the usual daily dose, both as daily DDDs. Other included information is for creating the required variables. See Package Parameters in Tutorials for more guidance.
#'
#' See more about ATC classification at \url{https://atcddd.fhi.no/atc/structure_and_principles/} and about DDDs at \url{https://atcddd.fhi.no/ddd/definition_and_general_considera/}.
#'
#' @seealso
#' \code{\link{pre2dup}}, \code{\link{check_package_parameters}}


#' @source Package parameters created for PRE2DUP-R example.
"package_parameters_example"
