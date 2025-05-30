library(data.table)
#' Checks Missing or Unvalid Arguments in Function Calls
#'
#' @description
#' This function checks if the required arguments are present in the function call. It compares the given arguments to main function argument list. It raises an error if any required arguments are missing, NULL, or empty strings and stops execution of check function. It is used used internally in the package to ensure that the necessary arguments are provided when calling the check functions.
#' @param dt required argument in the function call of any the datasets.
#' @param required_columns a character vector of required column names that must be present in the function call.
#' @param data_name required argument in the function call of any the datasets.
#' @param ... other arguments passed to the function call.
#'
#' @keywords internal
check_arguments <- function(dt, required_columns, data_name = NULL, ...) {
  arguments <- list(...)
  # Missing, NULL or empty arguments are not allowed
  arg_names <- names(arguments)
  # Missing from function call
  missing_arguments <- setdiff(required_columns, arg_names)
  # NULL values
  nulls <- names(arguments[vapply(arguments, is.null, TRUE)])
  # Empty strings
  empty_strings <- names(arguments[arguments == ""])
  # Return all missing arguments at once
  missing_arguments <- c(missing_arguments, nulls, empty_strings)
  if (length(missing_arguments) > 0) {
    emessage <- err_message(missing_arguments, "missing from function call.", arg_or_col = "argument")
    stop(emessage, call. = FALSE)
  }
}

#' Creation of Error Message
#'
#' @description
#' This function creates an error message for missing arguments or columns in the function call. It is used internally in the package to provide informative error messages when required arguments or columns are not provided.
#'
#' @param miss a character vector of missing arguments or columns.
#' @param error a character string describing the error.
#' @param arg_or_col a character string indicating whether the missing items are arguments or columns. It can be either "argument" or "column". The default is "argument"
#'
#' @returns a character string containing the formatted error message.
#' @keywords internal
err_message <- function(miss, error, arg_or_col = c("argument", "column")){
  arg_or_col <- match.arg(arg_or_col)
  if(arg_or_col == "argument") {
    paste(ngettext(length(miss), "argument", "arguments"),
          paste(sQuote(miss), collapse = ", "),
          ngettext(length(miss), "is", "are"), error, "\n")
  } else {
    paste(ngettext(length(miss), "column", "columns"),
          paste(sQuote(miss), collapse = ", "),
          ngettext(length(miss), "is", "are"), error, "\n")
  }
}
#' Check Date Range
#'
#' @description Checks if the provided date range is valid. It ensures that the date range contains exactly two valid dates and that the starting date is before the ending date. It is used internally in the package to validate date ranges provided by the user.
#' @param date_range a character vector of length 2 containing the start and end dates.
#'
#' @keywords internal
check_date_range <- function(date_range) {
  range1 <- date_to_integer(date_range[1])
  range2 <- date_to_integer(date_range[2])
  if (length(date_range) != 2 | is.na(range1) | is.na(range2)) {
    stop("two valid dates required for date range.",
         call. = FALSE)
  } else {
    if (range1 >= range2) {
      stop("starting date must be before ending date in date range.",
           call. = FALSE)
    }
  }
}

#' Make Warning Message
#'
#' @description This function creates a warning message for a specific condition in the data. It formats the message to include the column names involved, the number of rows affected, and the specific row numbers where the warning applies. It is used internally in the package to provide informative warnings when certain conditions are met.
#'
#' @param errorws a vector of row numbers where the warning applies.
#' @param colvars a character vector of column names that are involved in the warning.
#' @param warning_message a character string containing the warning message to be included in the output.
#' @param print_all a logical value indicating whether to print all rows with warnings or truncate the output to the first 5 rows. Default is TRUE.
#'
#' @keywords internal
make_warning <- function(errorws, colvars, warning_message, print_all = TRUE) {
  if (length(errorws) == 0) {
    return(NULL)  # No warning needed if there are no errors
  }
  # Convert column name(s) to a properly formatted string
  colvar_str <- if (length(colvars) > 1) {
    paste0("Columns ", paste(sQuote(colvars), collapse = ", "))
  } else {
    paste0("Column ", sQuote(colvars))
  }

  rows <- if (length(errorws) > 5 & isFALSE(print_all)) {
    paste0(paste(errorws[1:5], collapse = ", "), " ...")  # Truncate after 5
  } else {
    paste(errorws, collapse = ", ")
  }
  return(paste(colvar_str, warning_message, "in", length(errorws), "rows:", rows))
}

#' Convert Date to Integer
#'
#' @description This function converts a date-like object (Date, character, factor, or numeric) to an integer representing the number of days since 1970-01-01. It handles various formats and returns NA_integer_ for NA inputs. It is used internally in the package to ensure consistent date handling.
#'
#' @param x a date-like object (Date, character, factor, or numeric).
#'
#' @returns an integer representing the date in days since 1970-01-01, or NA_integer_ if the input is NA.
#' @keywords internal
date_to_integer <- function(x) {

  if (is.na(x)) {
    return(NA_integer_)
  }
  if (is.numeric(x)) {
    return(as.integer(x))
  }
  if (inherits(x, "Date")) {
    return(as.integer(x))
  }
  if (is.character(x) || is.factor(x)) {
    parsed_date <- suppressWarnings(as.Date(
      as.character(x),
      tryFormats = c(
        "%Y-%m-%d",
        "%d/%m/%Y",
        "%d.%m.%Y",
        "%Y%m%d",
        "%Y/%m/%d",
        "%Y.%m.%d",
        "%d%m%Y"
      ),
      optional = TRUE
    ))
    return(as.integer(parsed_date))
  }
}

#' Check Non-Numeric Values
#'
#' @description This function checks a character vector for non-numeric values. It returns the indices of elements that are either NA or do not match the alphanumeric pattern. It is used internally in the package to validate character columns that are expected to contain only alphanumeric values.
#'
#' @param col a character vector to be checked for non-numeric values.
#'
#' @keywords internal
check_non_numeric <- function(col) {
  return(which(is.na(col) | !grepl("^[[:alnum:]]+$", col)))
}

#' Check Numeric Values
#'
#' @description This function checks a numeric vector for invalid values. It identifies indices of elements that are infinite, negative, NA (if not allowed), or zero (if not allowed). It is used internally in the package to validate numeric columns that are expected to contain valid non-negative numbers.
#'
#' @param col a numeric vector to be checked for invalid values.
#' @param allow_na logical value indicating whether NA values are allowed. Default is FALSE.
#' @param allow_zero logical value indicating whether zero values are allowed. Default is FALSE.
#'
#' @returns a vector of indices where the numeric values are invalid (i.e., infinite, negative, NA, or zero if not allowed).
#' @keywords internal
check_numeric <- function(col, allow_na = FALSE, allow_zero = FALSE) {
  condition <- is.infinite(col) | col < 0
  if (isFALSE(allow_na)) {
    condition <- condition | is.na(col)
  }
  if (isFALSE(allow_zero)) {
    condition <- condition | (col == 0)
  }
  return(which(condition))
}

#' Check Order of Values
#'
#' @description This function checks the order of maximum of three numeric vectors: lower, usual, and upper. It returns the indices where the order is incorrect (i.e., lower > upper, lower > usual, or usual > upper). It is used internally in the package to validate the order of values in ATC and package parameters.
#'
#' @param lower a numeric vector representing the lower limit.
#' @param usual a numeric vector representing the usual value (optional).
#' @param upper a numeric vector representing the upper limit.
#'
#' @returns a vector of indices where the order of values is incorrect (i.e., lower > upper, lower > usual, or usual > upper).
#' @keywords internal
check_order <- function(lower, usual = NULL, upper){
  if(is.null(usual)){
    stopifnot(length(lower) == length(upper))
    condition <- lower > upper
  } else {
    stopifnot(length(lower) == length(usual) & length(usual) == length(upper))
    condition <- lower > upper | lower > usual | usual > upper
  }
  which(condition)
}

#' Find Multiple ATC Codes
#'
#' @description This function checks if each package has a single ATC code. It counts the unique ATC codes for each package and raises an error if any package has more than one ATC code. It is used internally in the package to ensure data integrity. Function is for internal use for creating error messages in the package.
#'
#' @param atc_col a character vector containing ATC codes.
#' @param package_col an integer vector containing package identifiers corresponding to the ATC codes.
#'
#' @returns invisible NULL if all packages have a single ATC code, otherwise raises an error with a message listing the packages that have multiple ATC codes.
#' @keywords internal
find_multiple_atcs <- function(atc_col, package_col) {
  # Count unique ATC-codes for each package
  unique_atcs_per_package <- tapply(atc_col, package_col, function(x)
    paste(unique(x), collapse = ", "))
  # Count how many unique ATC-codes per package
  n_unique_atcs <- sapply(unique_atcs_per_package, function(x)
    length(strsplit(x, ", ")[[1]]))
  # Form strings that combine package numbers and their ATC-codes
  atcs_combined <- paste(names(unique_atcs_per_package),
                         unique_atcs_per_package,
                         sep = ": ")
  # Check if there are more than one ATC-code in packages
  if (any(n_unique_atcs > 1)) {
    # Create an error message for packages with more than one ATC-code
    error_message <- paste0(
      "Expected only one ATC-code by package number, exceptions found: ",
      gsub('\"|c(|)', " ", paste(atcs_combined[n_unique_atcs > 1], collapse = "; "))
      , ".")
    stop(error_message, call. = FALSE)
  } else {
    return(invisible(NULL))
  }
}

#' Combine Overlapping Hospitalizations
#'
#' @param personid a vector of person identifiers in hospitalization data.
#' @param admission a vector of admission dates.
#' @param discharge a vector of discharge dates.
#'
#' @returns a data.table with combined hospitalizations, where overlapping hospitalizations are merged into single records.
#' @keywords internal
#' @import data.table
combine_overlaps <- function(personid, admission, discharge){

  dt <- data.table::data.table(pid_hosp = personid, admission = admission, discharge = discharge)
  n_before <- nrow(dt)

  # Sort the data by pid_hosp and admission date
  setorderv(dt, c("pid_hosp", "admission"))

  # Create a copy of the data to use in foverlaps
  dt_copy <- copy(dt)

  # Set keys for foverlaps
  keycols = c("pid_hosp","admission","discharge")
  setkeyv(dt, keycols)
  setkeyv(dt_copy, keycols)

  hospitalizations <- foverlaps(dt, dt_copy, by.x= keycols, type="any")
  hospitalizations[, admission_date := min(admission, i.admission), by = .(pid_hosp, discharge)]
  hospitalizations[, discharge_date := max(discharge, i.discharge), by = .(pid_hosp, admission_date)]
  hospitalizations[, admission_date := min(admission_date), by = .(pid_hosp, discharge_date)]
  hospitalizations[, c("admission", "discharge", "i.admission", "i.discharge") := NULL]
  hospitalizations <- unique(hospitalizations, by = c("pid_hosp", "admission_date", "discharge_date"))
  n_after <- nrow(hospitalizations)
  emessage <- paste("Number of overlapping hospitalizations detected and and will be merged: ", n_before - n_after)
  if(n_before != n_after) message(emessage)
  return(hospitalizations)
}

#' Check Coverage of Values by ATC Codes
#'
#' @param ATC a character vector of ATC codes.
#' @param val a numeric vector of values corresponding to the ATC codes.
#' @param limit a numeric value representing the upper limit for coverage percentage. Default is 90.
#' @param opti a character string indicating the type of coverage check to perform. It can be either "missing" (to check for non-missing values) or "zero" (to check for values greater than zero). Default is "missing".
#' 
#' @returns a character string listing ATC codes that do not meet the coverage limit, formatted as "ATC (coverage%)". If all ATC codes meet the limit, returns NULL.
#' @keywords internal
#' @import data.table
check_coverage <- function(ATC,
                           val,
                           limit = 90,
                           opti = c("missing", "zero")) {
  opti <- match.arg(opti)
  # Combine inputs into a data.table for efficient processing
  dt_info <- data.table::data.table(ATC, val)

  if (opti == "missing") {
    summary_dt <- dt_info[, .(coverage = sum(!is.na(val)) / .N * 100), by = ATC]
  }
  if (opti == "zero") {
    summary_dt <- dt_info[, .(coverage = sum(val > 0) / .N * 100), by = ATC]
  }

  # Filter ATC codes not meeting the upper limit
  summary_dt <- summary_dt[coverage < limit]

  if (nrow(summary_dt) > 0) {
    # Format for output
    summary_dt[, covr := paste0(ATC, " (", sprintf("%.1f", coverage), "%)")]
    return(paste(summary_dt$covr, collapse = ", "))
  } else {
    return(NULL)
  }
}

#' Extract ATC Codes from Text
#'
#' @description This function extracts ATC codes from a character vector containing text formatted as "ATC code (percentage)". It uses a regular expression to find the ATC codes before the parentheses. Function is for internal use for creating error messages in the package.
#'
#' @param text a character vector containing text from which ATC codes need to be extracted. The expected format is "ATC code (percentage)".
#'
#' @returns a character vector of extracted ATC codes. If no matches are found, it returns an empty character vector.
#' @keywords internal
extract_atc_codes <- function(text) {
  # Find any word before parentheses, e.g. "C01BA01 (85.7%)"
  pattern <- "\\b[[:alnum:]]+\\b(?= \\(\\d{1,3}\\.\\d%\\))"
  # Find matches
  matches <- regmatches(text, gregexpr(pattern, text, perl=TRUE))[[1]]
}
