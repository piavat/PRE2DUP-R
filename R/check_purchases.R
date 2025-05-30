#' Validate Drug Purchase Data
#'
#' This function checks the structure and content of drug purchase data (data.frame or data.table) for use in \code{\link{pre2dup}} workflows.
#' It helps users detect errors in advance, such as missing or invalid records, incorrect formats, or dates outside a specified range.
#' If all checks pass, the function can return a validated data.table with the required columns and proper types.
#'
#' @param dt data.frame or data.table containing drug purchase records.
#' @param pre_person_id Character. Column name for the person identifier.
#' @param pre_atc Character. Column name for the ATC code.
#' @param pre_package_id Character. Column name for the package identifier (e.g., Vnr in Nordic data).
#' @param pre_date Character. Column name for the drug purchase date.
#' @param pre_ratio Character. Column name for the amount of drug purchased: for whole packages, number of packages; for partial supplies, the proportion of a package (e.g., 0.5 for 14 tablets from a 28-tablet package).
#' @param pre_ddd Character. Column name for defined daily dose (DDD) of the purchase.
#' @param date_range Character vector of length 2. Date range for purchase dates (e.g., c("1995-01-01", "2018-12-31")). Default is NULL (no date range check).
#' @param print_all Logical. If TRUE, all row numbers that caused warnings are printed; if FALSE, only the first 5 problematic rows are printed.
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
#'   \item No missing or duplicated records
#'   \item Each package has a unique ATC code
#'   \item Validity of person identifiers (numeric or non-numeric, no missing values)
#'   \item Validity of ATC codes (no missing or invalid values)
#'   \item Validity of package IDs and purchase ratio (numeric, no missing values)
#'   \item DDD values: missing allowed, but not zero or negative
#'   \item All purchase dates must be present, convertible, and within the specified range (if given)
#'   \item Sufficient DDD coverage per ATC (with user confirmation if below threshold)
#' }
#'
#' If any errors are found, the function stops execution and prints all error messages.
#'
#' @examples
#' ID <- c(rep(100001, 3), rep(100002, 3))
#' ATC <- c(rep("N06AX11", 3), rep("N05AH03", 3))
#' vnr <- c(rep(48580, 3), rep(145698, 3))
#' dates <- as.Date(c("1998-07-04","1998-07-27","1998-08-28", "2000-01-12", "2000-02-05","2000-02-24"))
#' ratios <- c(0.5, 2, 2, 1, 0.5, 2)
#' ddds <- c(7.5, 30, 30, 28, 14, 56)
#' purchases <- data.frame(ID, ATC, vnr, dates, ratios, ddds)
#'
#' check_purchases(
#'   dt = purchases,
#'   pre_person_id = "ID",
#'   pre_atc = "ATC",
#'   pre_package_id = "vnr",
#'   pre_date = "dates",
#'   pre_ratio = "ratios",
#'   pre_ddd = "ddds",
#'   date_range = c("1995-01-01", "2018-12-31"),
#'   print_all = TRUE,
#'   return_data = TRUE
#' )
#'
#' @export
check_purchases <- function(dt,
                            pre_person_id = NULL,
                            pre_atc = NULL,
                            pre_package_id = NULL,
                            pre_date = NULL,
                            pre_ratio = NULL,
                            pre_ddd = NULL,
                            date_range = NULL,
                            print_all = FALSE,
                            return_data = FALSE) {

  data_name <- deparse(substitute(dt))
  # Checks that stops the execution of the function-----
  ##  Stops if the dataset is empty, arguments are missing or
  if(nrow(dt) == 0){
    stop(paste("No data in the dataset", sQuote(data_name)))
  }

  # Stops if arguments are not filled
  check_arguments(
    dt = dt,
    data_name = data_name,
    pre_person_id = pre_person_id,
    pre_atc = pre_atc,
    pre_package_id = pre_package_id,
    pre_date = pre_date,
    pre_ratio = pre_ratio,
    pre_ddd = pre_ddd,
    required_columns = names(formals())[2:7]
  )
  # Stops if the dataset is missing required columns
  required_columns = c(pre_person_id, pre_atc,
                       pre_package_id, pre_date,
                       pre_ratio, pre_ddd)
  missing_columns <- setdiff(required_columns, names(dt))
  if (length(missing_columns) > 0) {
    emessage <- err_message(missing_columns, "missing from the dataset.", arg_or_col = "column")
    stop(emessage, call. = FALSE)
  }
  # Stops if a package has different ATCs
  find_multiple_atcs(dt[[pre_atc]], dt[[pre_package_id]])

  if (!is.data.table(dt))
    dt <- as.data.table(dt)

  # Checks, that will be converted to warnings, stops later ----
  warning_list <- list()
  # Person_id check
  if(is.numeric(dt[[pre_person_id]])) {
    errows <- dt[, .I[check_numeric(col)],
                 env  = list(col = as.name(pre_person_id))]
    warning_list <- append(warning_list,
                           make_warning(errows, colvars = pre_person_id,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  } else {
    errows <- dt[, .I[check_non_numeric(col)],
                 env  = list(col = as.name(pre_person_id))]

    warning_list <- append(warning_list,
                           make_warning(errows, colvars = pre_person_id,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  }
  # ATC check
  errows <- dt[, .I[check_non_numeric(col)],
               env  = list(col = as.name(pre_atc))]

  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pre_atc,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  # Package_id and ratio should not have missing values
  numeric_cols <- c(pre_package_id, pre_ratio)
  for(col in numeric_cols) {
    errows <- dt[, .I[check_numeric(col)],
                 env  = list(col = as.name(col))]
    warning_list <- append(warning_list,
                           make_warning(errows, colvars = col,
                                        warning_message = mess_invalid_missing,
                                        print_all = print_all))
  }
  # DDD check: missing is accepted, but not zero or negative values
  # Notification about the zeros in DDD is a heads up
  errows <- dt[, .I[check_numeric(col, allow_na = TRUE)],
               env  = list(col = as.name(pre_ddd))]
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pre_ddd,
                                      warning_message = mess_invalid,
                                      print_all = print_all))

  # Date check: dates must not missing, convertible to integer and within range
  # At this point make separate vector for integers to not to mess the original data.
  date_pre <- sapply(dt[, col,  env = list(col = as.name(pre_date))], date_to_integer, USE.NAMES = FALSE)
  errows <- which(is.na(date_pre))
  warning_list <- append(warning_list,
                         make_warning(errows, colvars = pre_date,
                                      warning_message = mess_invalid_missing,
                                      print_all = print_all))
  if (!is.null(date_range)) {
    check_date_range(date_range)
  }
  if(!is.null(date_range) &&
     !any(is.na(date_pre))) {
    errows <- which(date_pre < date_to_integer(date_range[1]) |
                      date_pre > date_to_integer(date_range[2]))
    warning_list <- append(warning_list,
                           make_warning(errows, colvars = pre_date,
                                        warning_message = "has values outside date range",
                                        print_all = print_all))
  }

  DDD_covr <- check_coverage(dt[[pre_atc]], dt[[pre_ddd]], limit = limit, opti = "missing")
  if(!is.null(DDD_covr)) {
    emessage <- paste0("Coverage of DDD records in drug purchases in less than approved (", limit, "%) in following ATC(s): ", DDD_covr, ".")
    message(emessage)
    continue <- readline(prompt = atc_question)
    if (tolower(continue) != "y") {
      warning_list <- append(warning_list,
                             paste0("Coverage of DDD records in drug purchases is less than approved (",
                                    limit, "%) in following ATC(s): ",
                                    DDD_covr, "."))
    } else {
      atc_drops <- extract_atc_codes(DDD_covr)
      dt <- dt[pre_atc %notin% atc_drops, env = list(pre_atc = as.name(pre_atc))]
      message("Excluded ATC(s) with insufficient DDD information: ", paste(atc_drops, collapse = ", "), ". Process continues with the rest of the data.")
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
      dt <- dt[, .SD, .SDcols = c(required_columns)]
      dt[, (pre_person_id) := lapply(.SD, as.factor), .SDcols = pre_person_id]
      dt[, (pre_atc) := lapply(.SD, as.character), .SDcols = pre_atc]
      dt[, (pre_package_id) := lapply(.SD, as.integer), .SDcols = pre_package_id]
      dt$date_pre <-  sapply(dt[, col,  env = list(col = as.name(pre_date))], date_to_integer, USE.NAMES = FALSE)
      dt[, c(pre_ratio, pre_ddd) := lapply(.SD, as.numeric), .SDcols = c(pre_ratio, pre_ddd)]
      return(dt)
    }
  }
}
