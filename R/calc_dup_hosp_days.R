#' Calculate Hospital Days in Drug Use Periods
#'
#' Calculates the total number of hospital days during drug use periods. This function is for internal use of PRE2DUP-R.
#'
#' @param period_no Integer vector for period identification.
#' @param pre_id Vector of person identifiers for drug purchases.
#' @param dup_start Integer vector for the start of drug use periods.
#' @param dup_end Integer vector for the end of drug use periods.
#' @param hosp_id Vector of person identifiers for hospitalizations.
#' @param hosp_in Integer vector for hospitalization admission dates.
#' @param hosp_out Integer vector for hospitalization discharge dates.
#'
#' @return A data.table with two columns:
#' \describe{
#'   \item{period}{Period identifier.}
#'   \item{dup_hospital_days}{Number of hospitalization days during the period. Discharge dates are not included in the hospital stay.}
#' }

#' @import data.table
#' @keywords internal
#'
calc_dup_hosp_days <- function(period_no, pre_id, dup_start, dup_end, hosp_id, hosp_in, hosp_out) {
  purch_dt <- data.table(period_no, pid = pre_id, dup_start,  dup_end = as.integer(dup_end))
  hosp_dt <- data.table(pid = hosp_id, hosp_in, hosp_out)
  
  hosp_dt[, hosp_out := as.integer(hosp_out - 1)]
  setkey(purch_dt, pid, dup_start, dup_end)
  setkey(hosp_dt, pid, hosp_in, hosp_out)
  
  overlap_data <- foverlaps(
    hosp_dt, purch_dt,
    by.x = c("pid", "hosp_in", "hosp_out"),
    by.y = c("pid", "dup_start", "dup_end"),
    type = "any",
    nomatch = NA
  )
  overlap_data[, start_date := pmax(dup_start, hosp_in), by = period_no]
  overlap_data[, end_date := pmin(dup_end, hosp_out), by = period_no]
  overlap_data[, hosp_days := end_date - start_date]
  overlap_data[, hosp_days := fifelse(is.na(hosp_days), 0, hosp_days)]
  overlap_data[, hosp_days := sum(hosp_days), by = period_no]
  overlap_data <- overlap_data[!is.na(period_no)]
  return(overlap_data[!duplicated(period_no), .(period = period_no, dup_hospital_days = hosp_days)])
}
