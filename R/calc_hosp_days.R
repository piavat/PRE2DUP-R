#' Calculate Hospitalization Days Overlapping with Drug Purchases
#'
#' Calculates the total number of hospitalization days (`hosp_days_all`) and the number of hospitalization days that started during the drug exposure (`hosp_days_exp`) for each purchase period. The calculation is based on overlapping time windows between purchase events and hospitalization periods, taking into account the discharge date (which is not included in the hospital stay).
#'
#' @param pre_id Integer or character vector. Person identifier in drug purchases data.
#' @param pre_atc Character vector. ATC codes for each purchase.
#' @param curr_purc_start Integer or Date vector. Start date of the drug purchase period.
#' @param exp_end Integer or Date vector. End date of the drug exposure period.
#' @param next_start Integer or Date vector. Start date of the next purchase period (may be NA for the last purchase).
#' @param hosp_id Integer or character vector. IDs for each hospitalization (should match `pre_id`).
#' @param hosp_in Integer or Date vector. Admission date for each hospitalization.
#' @param hosp_out Integer or Date vector. Discharge date for each hospitalization.
#'
#' @return A data.table with two columns:
#' \describe{
#'   \item{hosp_days_all}{Total number of hospitalization days between purchase and next purchase (not including discharge date).}
#'   \item{hosp_days_exp}{Number of hospitalization days that started during drug exposure.}
#' }
#'
#' @details
#' The function merges drug purchase and hospitalization data by person ID, then calculates the overlap between purchase periods and hospitalizations. Hospital discharge date is not included in the total stay. Hospitalization days are summed for each purchase period. 
#'
#' @import data.table
#' @keywords internal
calc_hosp_days <- function(pre_id, pre_atc, curr_purc_start, exp_end, next_start, hosp_id, hosp_in, hosp_out) {

  purch_dt <- data.table(pid = pre_id, pre_atc, curr_purc_start,  exp_end = as.integer(exp_end), next_start)
  hosp_dt <- data.table(pid = hosp_id, hosp_in, hosp_out)

  # Merge
  merged_data <- merge(
    purch_dt, hosp_dt,
    by = "pid",
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  # Disharge date is not included in hospital stay
  merged_data[, hosp_out := as.integer(hosp_out - 1)]

  # Calculate all hospital days between purchase and next purchase
  merged_data[, start_date_all := fcase(
    hosp_in >= curr_purc_start & (!is.na(next_start) & hosp_in < next_start), pmax(curr_purc_start, hosp_in),
    hosp_in >= curr_purc_start & (is.na(next_start)) & hosp_in < exp_end, pmax(curr_purc_start, hosp_in),
    hosp_in < curr_purc_start & hosp_out > curr_purc_start, curr_purc_start,
    hosp_in < curr_purc_start & hosp_out <= curr_purc_start, NA_integer_
  )]

  merged_data[, end_date_all := fcase(
    is.na(next_start) & hosp_out > curr_purc_start & hosp_in < exp_end, hosp_out,
    hosp_out >= next_start, next_start,
    hosp_out < curr_purc_start, NA_integer_,
    hosp_out < next_start, hosp_out)]

  merged_data[, hosp_days_all := end_date_all - start_date_all]
  merged_data[, hosp_days_all := fifelse(is.na(hosp_days_all), 0, hosp_days_all)]

  # Hospital days that have started during exposure
  merged_data[, hosp_days_exp := fifelse(hosp_in < exp_end, hosp_days_all, 0)]
  merged_data[, hosp_days_all := sum(hosp_days_all), by = c("pid", "pre_atc", "curr_purc_start")]
  merged_data[, hosp_days_exp := sum(hosp_days_exp), by = c("pid", "pre_atc", "curr_purc_start")]
  merged_data[, hosp_days_exp := fifelse(is.na(hosp_days_exp), 0, hosp_days_exp)]
  merged_data <- unique(merged_data, by = c("pid", "pre_atc", "curr_purc_start"))

  return(merged_data[, .(hosp_days_all, hosp_days_exp)])
}
