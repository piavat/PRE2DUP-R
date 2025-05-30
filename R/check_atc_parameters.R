##' Validate ATC Parameters
#'
#' Anatomical Therapeutic Chemical (ATC) Classification System parameters are used to define the classification of drugs and their daily defined doses (DDD).
#' This function checks the structure and content of a given ATC parameter file (data.frame or data.table) for use in \code{\link{pre2dup}} workflows.
#' The validation covers required columns, uniqueness, data types, value ranges, and logical consistency.
#' The function stops execution if critical errors are found.
#'
#' @param dt data.frame or data.table containing the ATC parameters to validate.
#' @param atc_class Character. Name of the column containing the ATC class or code.
#' @param atc_ddd_low Character. Name of the column with the minimum daily DDD value.
#' @param atc_ddd_usual Character. Name of the column with the usual daily DDD value.
#' @param atc_dur_min Character. Name of the column with the minimum duration (in days).
#' @param atc_dur_max Character. Name of the column with the maximum duration (in days).
#' @param print_all Logical. If TRUE, all warnings are printed.
#' @param return_data Logical. If TRUE, the function returns the validated data. If FALSE, only a message is printed.
#'
#' @return
#' If \code{return_data = TRUE}, returns a data.table containing only the validated columns, with converted types.
#' If errors are detected, the function stops and prints error messages.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item Existence and naming of required columns
#'   \item Uniqueness of ATC class values (no duplicates)
#'   \item Validity of ATC codes (no missing or invalid values, correct type)
#'   \item Validity of DDD and duration values (no negative or missing values, logical value ranges)
#'   \item Correct order of minimum and maximum values (duration and DDD)
#' }
#'
#' If any errors are found, the function stops execution and prints all error messages.
#'
#' @examples
#' atc_data <- data.frame(
#'   atc_class = c("A01", "A02", "A03"),
#'   atc_ddd_low = c(0.1, 0.2, 0.3),
#'   atc_ddd_usual = c(1, 2, 3),
#'   atc_dur_min = c(10, 28, 35),
#'   atc_dur_max = c(40, 56, 100)
#' )
#' # Validate and return the data if all checks pass
#' check_atc_parameters(
#'   dt = atc_data,
#'   atc_class = "atc_class",
#'   atc_ddd_low = "atc_ddd_low",
#'   atc_ddd_usual = "atc_ddd_usual",
#'   atc_dur_min = "atc_dur_min",
#'   atc_dur_max = "atc_dur_max",
#'   print_all = TRUE,
#'   return_data = TRUE
#' )
#'
#' @export
check_atc_parameters <- function(dt,
                                 atc_class = NULL,
                                 atc_ddd_low = NULL,
                                 atc_ddd_usual = NULL,
                                 atc_dur_min = NULL,
                                 atc_dur_max = NULL,
                                 print_all = FALSE,
                                 return_data = FALSE) {
  data_name <- deparse(substitute(dt))
  # Checks that stops the execution of the function-----
  if (nrow(dt) == 0) {
    stop(paste0("No data in the dataset ", sQuote(data_name), "."), call. = FALSE)
  }
  # Stops if arguments are not filled
  check_arguments(
    dt = dt,
    data_name = data_name,
    atc_class = atc_class,
    atc_ddd_low = atc_ddd_low,
    atc_ddd_usual = atc_ddd_usual,
    atc_dur_min = atc_dur_min,
    atc_dur_max = atc_dur_max,
    required_columns = names(formals())[2:6]
  )
  # Stops if the dataset is missing required columns
  required_columns = c(atc_class,
                       atc_ddd_low,
                       atc_ddd_usual,
                       atc_dur_min,
                       atc_dur_max)
  missing_columns <- setdiff(required_columns, names(dt))
  if (length(missing_columns) > 0) {
    emessage <- err_message(missing_columns, "missing from the dataset.", arg_or_col = "column")
    stop(emessage, call. = FALSE)
  }
  if (!is.data.table(dt))
    dt <- as.data.table(dt)
  # Checks, that will be converted to warnings, stops later ----
  warning_list <- list(
  )

  # Checks if there are duplicated values in ATC class
  if (length(unique(dt[[atc_class]])) != nrow(dt)) {
    warning_list <- append(warning_list, list(paste(
      sQuote(atc_class), "has duplicated values"
    )))
  }

  # ATC check
  errows <- dt[, .I[check_non_numeric(col)], env  = list(col = as.name(atc_class))]
  warning_list <- append(
    warning_list,
    make_warning(
      errows,
      colvars = atc_class,
      warning_message = mess_invalid_missing,
      print_all = print_all
    )
  )
  # For DDD lower limit zero is accepted, but not missing or negative values
  errows <- dt[, .I[check_numeric(col, allow_zero = TRUE)], env  = list(col = as.name(atc_ddd_low))]
  warning_list <- append(
    warning_list,
    make_warning(
      errows,
      colvars = atc_ddd_low,
      warning_message = mess_invalid_missing,
      print_all = print_all
    )
  )

  # For usual DDD and package durs missing, zero or negative values are not allowed
  numeric_cols <- c(atc_ddd_usual, atc_dur_min, atc_dur_max)
  for (col in numeric_cols) {
    errows <- dt[, .I[check_numeric(col)], env  = list(col = as.name(col))]
    warning_list <- append(
      warning_list,
      make_warning(
        errows,
        colvars = col,
        warning_message = mess_invalid_missing,
        print_all = print_all
      )
    )
  }
  # Warns if package durations are in wrong order
  errows <- dt[, .I[check_order(lower = col1, upper = col2)],
               env  = list(col1 = as.name(atc_dur_min),
                           col2 = as.name(atc_dur_max))]
  warning_list <- append(
    warning_list,
    make_warning(
      errows,
      colvars = c(atc_dur_min, atc_dur_max),
      warning_message = mess_invalid_order,
      print_all = print_all
    )
  )
  # Warns if DDD limits are in wrong order
  errows <- dt[, .I[check_order(lower = col1, upper = col2)],
               env  = list(col1 = as.name(atc_ddd_low),
                           col2 = as.name(atc_ddd_usual))]
  warning_list <- append(
    warning_list,
    make_warning(
      errows,
      colvars = c(atc_ddd_low, atc_ddd_usual),
      warning_message = mess_invalid_order,
      print_all = print_all
    )
  )
  # Stops if there are any warnings
  if (length(warning_list) > 0) {
    for (warning in warning_list) {
      message(warning)
    }
    stop(
      paste0(
        "Errors in dataset assigned to ",
        sQuote(data_name) ,
        ". See listing above for details."
      ),
      call. = FALSE
    )
  } else {
    message(paste0("Checks passed for ", sQuote(data_name), "."))
    if (return_data) {
      dt <- dt[, .SD, .SDcols = required_columns]
      dt[, (atc_class) := lapply(.SD, as.character), .SDcols = atc_class]
      dt[, c(atc_ddd_low, numeric_cols) := lapply(.SD, as.numeric)
         , .SDcols = c(atc_ddd_low, numeric_cols)]
      return(dt)
    }
  }
}
