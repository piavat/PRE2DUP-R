#' Validate Hospitalization Data
#'
#' This function checks the structure and content of hospitalization data (data.frame or data.table) for use in \code{\link{pre2dup}} workflows.
#' It validates required columns, data types, date consistency, and chronological logic (admission before discharge).
#' If all checks pass, it can return a cleaned data.table with the required columns and types.
#'
#' @param dt data.frame or data.table containing hospitalization records.
#' @param hosp_person_id character. Column name for the person identifier.
#' @param hosp_admission character. Column name for hospital admission date.
#' @param hosp_discharge character. Column name for hospital discharge date.
#' @param date_range character vector of length 2. Date range for hospitalizations (e.g., c("1995-01-01", "2025-12-31")). Default is NULL (no date range check).
#' @param print_all logical. If TRUE, all row numbers that caused warnings are printed; if FALSE, only the first 5 problematic rows are printed.
#' @param return_data logical. If TRUE and no errors are detected, returns a data.table with the validated columns and proper types. If FALSE, only a message is printed.
#'
#' @return
#' If \code{return_data = TRUE}, returns a data.table containing only the validated columns, with dates converted to integer and overlapping hospitalizations combined.
#' If errors are detected, the function stops and prints error messages.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item Existence and naming of required columns
#'   \item Validity of person identifiers (numeric or non-numeric, no missing values)
#'   \item Admission and discharge dates are present and convertible to date
#'   \item Admission date is strictly before discharge date
#'   \item All dates are within the specified range (if given)
#'   \item Overlapping hospitalizations are combined
#' }
#'
#' If any errors are found, the function stops execution and prints all error messages.
#'
#' @import data.table
#'
#' @importFrom intervals Intervals interval_union
#'
#' @examples
#' PID <- c(1, 1, 2, 2)
#' Entry <- c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")
#' Leave <- c("2023-01-15", "2023-02-15", "2023-01-10", "2023-02-10")
#' hospital_data <- data.frame(PID, Entry, Leave)
#'
#' hospitalizations <- check_hospitalizations(
#'   hospital_data,
#'   hosp_person_id = "PID",
#'   hosp_admission = "Entry",
#'   hosp_discharge = "Leave",
#'   return_data = TRUE
#' )
#' hospitalizations
#'
#' @export
check_hospitalizations <- function(dt,
                            hosp_person_id = NULL,
                            hosp_admission = NULL,
                            hosp_discharge = NULL,
                            date_range = NULL,
                            print_all = FALSE,
                            return_data = FALSE) {

  data_name <- deparse(substitute(dt))

  # Checks that stops the execution of the function -----

  if(nrow(dt) == 0){
    stop(paste0("No data in the dataset ", sQuote(data_name)), ".",
         call. = FALSE)
  }

  # Stops if arguments are not filled
  check_arguments(
    dt = dt,
    data_name = data_name,
    hosp_person_id = hosp_person_id,
    hosp_admission = hosp_admission,
    hosp_discharge = hosp_discharge,
    required_columns = names(formals())[2:4]
  )

  # Stops if the dataset is missing required columns
  required_columns = c(hosp_person_id, hosp_admission, hosp_discharge)
  missing_columns <- setdiff(required_columns, names(dt))
  if (length(missing_columns) > 0) {
    emessage <- err_message(missing_columns, "missing from the dataset.", arg_or_col = "column")
    stop(emessage, call. = FALSE)
  }

  if (!is.data.table(dt))
    dt <- as.data.table(dt)

  # Checks, that will be converted to warnings, stops later ----
  warning_list <- list()
  # Person_id check
  if(is.numeric(dt[[hosp_person_id]])) {
    errows <- dt[, .I[check_numeric(col)],
                 env  = list(col = as.name(hosp_person_id))]
    warning_list <- append(warning_list,
                           make_warning(errows, colvars = hosp_person_id,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  } else {
    errows <- dt[, .I[check_non_numeric(col)],
                 env  = list(col = as.name(hosp_person_id))]

    warning_list <- append(warning_list,
                           make_warning(errows, colvars = hosp_person_id,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  }

  # Dates must be convertible to integer
  adm_date <- sapply(dt[, col,  env = list(col = as.name(hosp_admission))], date_to_integer, USE.NAMES = FALSE)
  dis_date <- sapply(dt[, col,  env = list(col = as.name(hosp_discharge))], date_to_integer, USE.NAMES = FALSE)

  errows <- which(is.na(adm_date))
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = hosp_admission,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  errows <- which(is.na(dis_date))
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = hosp_discharge,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  errows <- which(adm_date >= dis_date)
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = hosp_admission,
                                      warning_message = "has admission date later or in a same date than discharge date",
                                      print_all = print_all))

  if (!is.null(date_range)) {
    check_date_range(date_range)
    if(!any(is.na(adm_date))) {
      errows <- which(adm_date < date_to_integer(date_range[1]) |
                        adm_date > date_to_integer(date_range[2]))
      warning_list <- append(warning_list,
                             make_warning(errows, colvars = hosp_admission,
                                          warning_message = "has values outside date range",
                                          print_all = print_all))
      if(!any(is.na(dis_date))) {
        errows <- which(dis_date < date_to_integer(date_range[1]) |
                          dis_date > date_to_integer(date_range[2]))
        warning_list <- append(warning_list,
                               make_warning(errows, colvars = hosp_discharge,
                                            warning_message = "has values outside date range",
                                            print_all = print_all))
      }
    }
  }

  # Stops if there are warnings
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
      message("Preparing hospitalization data and merging overlapping hospitalizations.")
      dt <- dt[, .SD, .SDcols = c(required_columns)]
      setnames(dt, c(hosp_person_id, hosp_admission, hosp_discharge), c("pid_hosp", "admission_date", "discharge_date"))
      dt[, pid_hosp := as.factor(pid_hosp)]
      # Set to integer with date to integer to uniform dates before overlap check
      dt$admission_date <- sapply(dt$admission_date, date_to_integer, USE.NAMES = FALSE)
      dt$discharge_date <- sapply(dt$discharge_date, date_to_integer, USE.NAMES = FALSE)
      # Combine overlapping hospitalizations
      n_before <- nrow(dt)
      .merge_intervals <- function(start_dates, end_dates) {
        interv <- Intervals(cbind(start_dates, end_dates), closed = c(TRUE, TRUE))
        merged <- interval_union(interv)
        merged <- as.data.table(merged)
        setnames(merged, c("V1", "V2"), c("admission_date", "discharge_date"))
        return(merged)
      }
      dt <- dt[, .merge_intervals(admission_date, discharge_date), by = pid_hosp]
      # Interval return dates as numeric, re-set to integer
      dt$admission_date <- sapply(dt$admission_date, date_to_integer, USE.NAMES = FALSE)
      dt$discharge_date <- sapply(dt$discharge_date, date_to_integer, USE.NAMES = FALSE)
      n_after <- nrow(dt)
      emessage <- paste("Number of overlapping hospitalizations detected and and will be merged: ", n_before - n_after)
      if(n_before != n_after) message(emessage)
      return(dt)
    }
  }
}
