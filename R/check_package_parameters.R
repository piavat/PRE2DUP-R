#' Validate Package Parameters
#'
#' This function checks the structure and content of a given package parameter file (data.frame or data.table) for use in \code{\link{pre2dup}} workflows.
#' The validation covers required columns, uniqueness, data types, value ranges, and logical consistency.
#' The function stops execution and prints error messages if critical errors are found.
#'
#' @param dt data.frame or data.table containing the package parameters to validate.
#' @param pack_atc Character. Name of the column containing the Anatomical Therapeutic Chemical (ATC) Classification code.
#' @param pack_id Character. Name of the column containing the package identifier (e.g., vnr code).
#' @param pack_ddd_low Character. Name of the column with the minimum daily DDD value.
#' @param pack_ddd_usual Character. Name of the column with the usual daily DDD value.
#' @param pack_dur_min Character. Name of the column with the minimum package duration (in days).
#' @param pack_dur_usual Character. Name of the column with the usual package duration (in days).
#' @param pack_dur_max Character. Name of the column with the maximum package duration (in days).
#' @param print_all Logical. If TRUE, all warnings are printed. If FALSE, only the first 5 problematic rows are printed.
#' @param return_data Logical. If TRUE and no errors are detected, returns a data.table with the validated columns and proper types. If FALSE, only a message is printed.
#'
#' @return
#' If \code{return_data = TRUE}, returns a data.table containing only the validated columns, with converted types.
#' If errors are detected, the function stops and prints error messages.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item Existence and naming of required columns
#'   \item Uniqueness of package identifiers (no duplicates)
#'   \item Validity of ATC codes (no missing or invalid values, correct type)
#'   \item Validity of package IDs (should be numeric, no missing values)
#'   \item Validity of DDD and duration values (no negative or missing values, logical value ranges)
#'   \item Correct order of minimum, usual, and maximum durations
#'   \item Correct order of minimum and usual DDD values
#' }
#'
#' If any errors are found, the function stops execution and prints all error messages.
#'
#' @examples
#' ATC <- c("N05AA01", rep("N05AH03", 3), "N05AX13")
#' vnr <- c(141473, 80307, 145698, 457780 , 412581)
#' lower_ddd = c(0.167, 0.500, 0.500, 0.375, 0.714)
#' usual_ddd <- c(0.33, 1.00, 1.00, 0.75, 1.00)
#' minimum_duration <- c(33, 9.3, 9.3, 33.3, 14)
#' usual_duration <- c(100, 28, 28, 100, 30)
#' maximum_duration <- c(200, 56, 56, 200, 42)
#' df_pack_params <- data.frame(ATC, vnr, lower_ddd, usual_ddd,
#'   minimum_duration, usual_duration, maximum_duration)
#'
#' package_parameters <- check_package_parameters(
#'   dt = df_pack_params,
#'   pack_atc = "ATC",
#'   pack_id = "vnr",
#'   pack_ddd_low = "lower_ddd",
#'   pack_ddd_usual = "usual_ddd",
#'   pack_dur_min = "minimum_duration",
#'   pack_dur_usual = "usual_duration",
#'   pack_dur_max = "maximum_duration",
#'   return_data = TRUE
#' )
#' package_parameters
#'
#' @export
check_package_parameters <- function(dt,
                                     pack_atc = NULL,
                                     pack_id = NULL,
                                     pack_ddd_low = NULL,
                                     pack_ddd_usual = NULL,
                                     pack_dur_min = NULL,
                                     pack_dur_usual = NULL,
                                     pack_dur_max = NULL,
                                     print_all = FALSE,
                                     return_data = FALSE) {

  data_name <- deparse(substitute(dt))
  # Checks that stops the execution of the function-----
  if(nrow(dt) == 0){
    stop(paste0("No data in the dataset ", sQuote(data_name), "."),
         call. = FALSE)
  }
  # Stops if arguments are not filled
  check_arguments(
    dt = dt,
    data_name = data_name,
    pack_atc = pack_atc,
    pack_id = pack_id,
    pack_ddd_low = pack_ddd_low,
    pack_ddd_usual = pack_ddd_usual,
    pack_dur_min = pack_dur_min,
    pack_dur_usual = pack_dur_usual,
    pack_dur_max = pack_dur_max,
    required_columns = names(formals())[2:8]
  )
  # Stops if the dataset is missing required columns
  required_columns = c(pack_atc,
                       pack_id,
                       pack_ddd_low,
                       pack_ddd_usual,
                       pack_dur_min,
                       pack_dur_usual,
                       pack_dur_max)
  missing_columns <- setdiff(required_columns, names(dt))
  # Stops if the dataset is missing required columns
  if (length(missing_columns) > 0) {
    emessage <- err_message(missing_columns, "missing from the dataset.", arg_or_col = "column")
    stop(emessage, call. = FALSE)
  }

  if (!is.data.table(dt))
    dt <- as.data.table(dt)
  # Checks, that will be converted to warnings, stops later ----
  warning_list <- list()

  # Checks if there are duplicated values in package IDs
  if (length(unique(dt[[pack_id]])) != nrow(dt)) {
    warning_list <- append(warning_list, list(paste(sQuote(pack_id), "has duplicated values")))
  }
  # ATC check
  errows <- dt[, .I[check_non_numeric(col)],
               env  = list(col = as.name(pack_atc))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pack_atc,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  # package ID
  errows <- dt[, .I[check_numeric(col)],
               env  = list(col = as.name(pack_id))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pack_id,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  # For DDD lower limit zero is accepted, but not missing or negative values
  errows <- dt[, .I[check_numeric(col, allow_zero = TRUE)],
              env  = list(col = as.name(pack_ddd_low))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pack_ddd_low,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))

  # For usual DDD and package durs missing, zero or negative values are not allowed
  numeric_cols <- c(pack_ddd_usual,
                    pack_dur_min,
                    pack_dur_usual,
                    pack_dur_max)
  for(col in numeric_cols) {
    errows <- dt[, .I[check_numeric(col)],
                 env  = list(col = as.name(col))]
    warning_list <- append(warning_list,
                           make_warning(errows, colvars = col,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  }
  # Warns if package durations are in wrong order
  errows <- dt[, .I[check_order(lower = col1, usual = col2, upper = col3)],
               env  = list(col1 = as.name(pack_dur_min),
                           col2 = as.name(pack_dur_usual),
                           col3 = as.name(pack_dur_max))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = c(pack_dur_min,
                                                          pack_dur_usual,
                                                          pack_dur_max),
                                      warning_message = mess_invalid_order,
                                      print_all = print_all))
  # Warns if DDD limits are in wrong order
  errows <- dt[, .I[check_order(lower = col1, upper = col2)],
               env  = list(col1 = as.name(pack_ddd_low),
                           col2 = as.name(pack_ddd_usual))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = c(pack_ddd_low, pack_ddd_usual),
                                      warning_message = mess_invalid_order,
                                      print_all = print_all))
  # Stops if there are any warnings
  if (length(warning_list) > 0) {
    for (warning in warning_list) {
      message(warning)
    }
    stop(paste0(
      "Errors in dataset assigned to ",
      sQuote(data_name) ,
      ". See listing above for details."
    ), call. = FALSE)
  } else {
    message(paste("Checks passed for", sQuote(data_name)))
    if(return_data) {
      dt <- dt[, .SD, .SDcols = required_columns]
      dt[, (pack_atc) := lapply(.SD, as.character), .SDcols = pack_atc]
      dt[, (pack_id) := lapply(.SD, as.integer), .SDcols = pack_id]
      dt[, c(pack_ddd_low, numeric_cols) := lapply(.SD, as.numeric), .SDcols = c(pack_ddd_low, numeric_cols)]
      return(dt)
    }
  }
}
