#' Estimate Drug Use Periods from Drug Purchase Data
#'
#' Estimates drug use periods based on individual drug purchase data, supporting the estimation for individuals with varied purchase patterns and stockpiling. The estimation uses package-specific and Anatomical Therapeutic Chemical (ATC) Classification code -level parameters (latter provided with the package). Optionally, hospitalization data can be incorporated.
#'
#' @param pre_data  a data.frame or data.table containing drug purchases.
#' @param pre_person_id character. Name of the column containing person id.
#' @param pre_atc character. Name of the column containing ATC code.
#' @param pre_package_id character. Name of the column containing package id.
#' @param pre_date character. Name of the column containing purchase date.
#' @param pre_ratio character. Name of the column containing ratio of packages purchased (e.g., number of packages).
#' @param pre_ddd character. Name of the column containing defined daily doses (DDD) of the purchase.
#' @param package_parameters a data.frame or data.table containing package parameters.
#' @param pack_atc character. Name of the column containing ATC code.
#' @param pack_id character. Name of the column containing package id.
#' @param pack_ddd_low character. Name of the column containing lower limit of daily DDD.
#' @param pack_ddd_usual character. Name of the column containing usual daily DDD.
#' @param pack_dur_min character. Name of the column containing minimum duration of the package.
#' @param pack_dur_usual character. Name of the column containing usual duration of the package.
#' @param pack_dur_max character. Name of the column containing maximum duration of the package.
#' @param atc_parameters a data.frame or data.table containing ATC parameters.
#' @param atc_class character. Name of the column containing ATC class.
#' @param atc_ddd_low character. Name of the column containing lower limit of daily DDD for the ATC class.
#' @param atc_ddd_usual character. Name of the column containing usual daily DDD for the ATC class.
#' @param atc_dur_min character. Name of the column containing minimum duration for the ATC class.
#' @param atc_dur_max character. Name of the column containing maximum duration for the ATC class.
#' @param hosp_data a data.frame or data.table containing hospitalizations.
#' @param hosp_person_id character. Name of the column containing person id.
#' @param hosp_admission character. Name of the column containing admission date.
#' @param hosp_discharge character. Name of the column containing discharge date.
#' @param date_range character. A vector of two dates, expected start and end dates in the drug purchase data.
#' @param global_gap_max numeric. Maximum time between purchases that can be considered as continuous use. Default is 300.
#' @param global_min numeric. Minimum duration of a drug purchase. Default is 5.
#' @param global_max numeric. Maximum duration of a drug purchase. Default is 300.
#' @param global_max_single numeric. Maximum duration of a single purchase. Default is 150.
#' @param global_ddd_high numeric. Maximum daily DDD for a purchase per day for any ATC. Default is 10.
#' @param global_hosp_max numeric. Maximum number of hospital days to be considered when estimating the exposure duration. Default is 30.
#' @param days_covered numeric. Maximum number of days to be added to the exposure duration to cover the gap between purchases. Default is 5.
#' @param weight_past numeric. Weight for the past purchase in sliding average calculation. Default is 1.
#' @param weight_current numeric. Weight for the current purchase in sliding average calculation. Default is 4.
#' @param weight_next numeric. Weight for the next purchase in sliding average calculation. Default is 1.
#' @param weight_first_last numeric. Weight for the first and last purchase in sliding average calculation. Default is 5.
#' @param post_process_perc numeric. Starting percentage for the gap duration to be used in post-processing. If the gap between consecutive drug use periods is at most the specified percentage of the duration of the preceding period, the periods will be connected. The percentage decreases by 0.1 in each iteration. Default is 1.
#' @param drop_atcs logical. If TRUE the ATC codes without sufficient DDD or package parameter coverage will be ignored and the process continues with rest of the ATC codes, if FALSE, function execution stops. Default is FALSE.
#' @param data_to_return character. Defines the data to return: drug use periods, updated package parameters, or both. The "periods" returns drug use periods, "parameters" returns updated package parameter file and "both" returns both of the datasets. Default is "periods".
#'
#' @return a dataset or datasets selected at \code{data_to_return}. If \code{data_to_return = "periods"} function returns a data.table consisting drug use periods with period number, person identifier, ATC, period start and end dates, period duration in days, days spent in hospital during the period, number of purchases, total purchased DDDs and average daily DDD over the period. If \code{data_to_return = "parameters"}, function returns the original package parameter file with an additional column \code{common_duration} that contains them most common duration of each package in drug purchase data. If \code{data_to_return = "both"}, both datasets are returned as a list of two objects.
#'
#' @details
#'
#' Function validates the input data and arguments before the assessment of drug use periods.
#' It will stop execution if issues are detected, with the following exceptions:
#'
#' \itemize{
#'   \item Up to 10\% of missing DDD values per ATC class in the drug purchase data is allowed.
#'   \item Up to 10\% of missing package parameter records per ATC class is allowed.
#' }
#' If either threshold is exceeded and \code{drop_atcs = FALSE} function stops with error, but with \code{drop_atcs = TRUE} ATC classes with insufficient data are ignored, and the function proceeds with the remaining data.
#'
#' There are five available methods for estimating the duration of each purchase, presented in the order of preference:
#' \itemize{
#'   \item Continuous use: Based on purchased daily doses (DDDs), temporal average of daily DDDs, and individual purchase patterns.
#'   \item Package-based methods:
#'     \itemize{
#'       \item Package DDD method: Based on purchased DDDs and the usual daily DDD for the specific package.
#'       \item Package duration method: Based on the usual duration of the package, considering the number of the packages (or a proportion of partial package) purchased.
#'     }
#'   \item ATC-based methods:
#'     \itemize{
#'       \item ATC-level DDD method: Based on purchased DDDs and usual daily DDDs at the ATC level.
#'       \item Minimum ATC duration method: Based on the minimum duration defined for the ATC group.
#'     }
#' }
#'
#' Periods that are close in time are joined in a post-processing step controlled by \code{post_process_perc}. Post processing percentage reduces by 0.1 at each estimation round to prevent long calculation times for large datasets.
#'
#' Selecting \code{data_to_return = "parameters"} the \code{pre2dup} calculates the most common package duration for each package from the drug purchase data. Package parameter's usual package duration and usual daily DDD (Total DDDs in package/usual duration) can be updated based on common duration, and pre2dup can be re-run to calculate drug use periods using the updated package parameters.
#'
#' @import data.table
#'
#' @seealso
#' Drug purchases, parameter files and hospitalizations has their own check functions. The \code{pre2dup} runs the checks internally, but checking the validity before running the program is recommended for faster and easier error detection and handling.
#'
#' \code{\link{check_purchases}}, \code{\link{check_hospitalizations}}, \code{\link{check_package_parameters}}, \code{\link{check_atc_parameters}}
#'
#' @examples
#' period_data <-pre2dup(pre_data = purchases_example, pre_person_id = "id",
#'  pre_atc = "ATC", pre_package_id = "vnr", pre_date = "purchase_date",
#'   pre_ratio = "n_packages", pre_ddd = "amount",
#'    package_parameters = package_parameters_example,
#'     pack_atc = "ATC", pack_id = "vnr", pack_ddd_low = "lower_ddd",
#'      pack_ddd_usual ="usual_ddd", pack_dur_min = "minimum_dur",
#'       pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur",
#'        atc_parameters = ATC_parameters, atc_class = "partial_atc",
#'        atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
#'         atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc",
#'          hosp_data = hospitalizations_example, hosp_person_id = "id",
#'           hosp_admission = "hospital_start", hosp_discharge = "hospital_end",
#'            date_range = c("2025-01-01", "2025-12-31"),
#'             global_gap_max = 300, global_min = 5, global_max = 300,
#'              global_max_single = 150, global_ddd_high = 10,
#'               global_hosp_max = 30,days_covered = 5, weight_past = 1,
#'                weight_current = 4, weight_next = 1, weight_first_last = 5,
#'                 drop_atcs = FALSE,
#'                 data_to_return = "periods",
#'                  post_process_perc = 1)
#'
#'period_data
#'
#' @export
pre2dup <- function(pre_data,
                    pre_person_id,
                    pre_atc,
                    pre_package_id,
                    pre_date,
                    pre_ratio,
                    pre_ddd,
                    package_parameters,
                    pack_atc,
                    pack_id,
                    pack_ddd_low,
                    pack_ddd_usual,
                    pack_dur_min,
                    pack_dur_usual,
                    pack_dur_max,
                    atc_parameters,
                    atc_class,
                    atc_ddd_low,
                    atc_ddd_usual,
                    atc_dur_min,
                    atc_dur_max,
                    hosp_data = NULL,
                    hosp_person_id = NULL,
                    hosp_admission = NULL,
                    hosp_discharge = NULL,
                    date_range = NULL,
                    global_gap_max = 300,
                    global_min = 5,
                    global_max = 300,
                    global_max_single = 150,
                    global_ddd_high = 10,
                    global_hosp_max = 30,
                    days_covered = 5,
                    weight_past  = 1,
                    weight_current = 4,
                    weight_next = 1,
                    weight_first_last = 5,
                    drop_atcs = FALSE,
                    data_to_return = "periods",
                    post_process_perc = 1) {
  # Dataset checks ----

  ## Check global arguments --------------------------------------------------
  message("Step 1/6: Checking parameters and datasets...")

  .check_positive <- function(numvar) {
    if (!is.numeric(numvar) || numvar < 0) {
      err_mess <- "Input non numeric or below 0 in argument"
      stop(paste(err_mess, sQuote(substitute(numvar))), call. = FALSE)
    }
  }
  .check_positive(global_gap_max)
  .check_positive(global_min)
  .check_positive(global_max)
  .check_positive(global_max_single)
  .check_positive(global_ddd_high)
  .check_positive(weight_past)
  .check_positive(weight_current)
  .check_positive(weight_next)
  .check_positive(weight_first_last)
  .check_positive(days_covered)
  .check_positive(post_process_perc)
  if (!is.null(hosp_data))
    .check_positive(global_hosp_max)

  if (global_max <= global_min)
    stop(
      "Expected Global maximum (global_max) longer than global minimum (global_min).",
      call. = FALSE
    )
  if (global_max > global_gap_max)
    stop(
      "Expected Maximum distance (global_gap_max) to be longer than or equal to global maximum (global_max).",
      call. = FALSE
    )
  if (global_max_single > global_max)
    stop(
      "Expected Global maximum of single purchase (global_max_single) shorter than or equal to Global maximum (global_max).",
      call. = FALSE
    )
  if (!is.logical(drop_atcs) ||
      is.na(drop_atcs)) {
    stop("Argument 'drop_atcs' must be either TRUE or FALSE.",
         call. = FALSE)
  }
  data_to_return <- match.arg(data_to_return, choices = c("periods", "parameters", "both"))

  ## Check and modify datasets -----------------------------------------

  # Check drug purchases data
  pre_data <- check_purchases(
    pre_data,
    pre_person_id = pre_person_id,
    pre_atc = pre_atc,
    pre_package_id = pre_package_id,
    pre_date = pre_date,
    pre_ratio = pre_ratio,
    pre_ddd = pre_ddd,
    date_range = date_range,
    return_data = TRUE,
    drop_atcs = drop_atcs # fix
  )

  # After checks, rename columns for easier internal use (data was handled in check)
  oldnames <- c(pre_person_id, pre_atc, pre_package_id, pre_ratio, pre_ddd)
  newnames <- c("pid_pre", "atc_pre", "packid_pre", "ratio_pre", "ddd_pre")
  setnames(pre_data, oldnames, newnames)
  # Package parameters
  if (!is.null(package_parameters) &
      data_to_return != "periods")
    package_parameters_orig <- package_parameters

  # Check package parameters
  package_parameters <- check_package_parameters(
    dt = package_parameters,
    pack_atc = pack_atc,
    pack_id = pack_id,
    pack_ddd_low = pack_ddd_low,
    pack_ddd_usual = pack_ddd_usual,
    pack_dur_min = pack_dur_min,
    pack_dur_usual = pack_dur_usual,
    pack_dur_max = pack_dur_max,
    return_data = TRUE
  )
  # After copy and checks, rename columns for easier internal use
  oldnames <- c(
    pack_atc,
    pack_id,
    pack_ddd_low,
    pack_ddd_usual,
    pack_dur_min,
    pack_dur_usual,
    pack_dur_max
  )
  newnames <- c(
    "atc_code",
    "pack_no",
    "ddd_low_pack",
    "ddd_usual_pack",
    "dur_min_pack",
    "dur_usual_pack",
    "dur_max_pack"
  )
  setnames(package_parameters, oldnames, newnames)

  # Check ATC parameters
  atc_parameters <- check_atc_parameters(
    dt = atc_parameters,
    atc_class = atc_class,
    atc_ddd_low = atc_ddd_low,
    atc_ddd_usual = atc_ddd_usual,
    atc_dur_min = atc_dur_min,
    atc_dur_max = atc_dur_max,
    return_data = TRUE
  )
  # After checks, rename columns for easier internal use
  oldnames <- c(atc_class,
                atc_ddd_low,
                atc_ddd_usual,
                atc_dur_min,
                atc_dur_max)
  newnames <- c("part_atc",
                "ddd_low_atc",
                "ddd_usual_atc",
                "dur_min_atc",
                "dur_max_atc")
  setnames(atc_parameters, oldnames, newnames)

  # If hospitalizations exists, check
  if (!is.null(hosp_data)) {
    hosp_data <- check_hospitalizations(
      hosp_data,
      hosp_person_id = hosp_person_id,
      hosp_admission = hosp_admission,
      hosp_discharge = hosp_discharge,
      return_data = TRUE
    )
  }

  ## Find multiple ATCs combining purchases and parameters ----
  pre_pairs <- pre_data[, .(packid = packid_pre, atc = atc_pre)]
  pack_pairs <- package_parameters[, .(packid = pack_no, atc = atc_code)]
  pre_pack_pairs <- rbind(pre_pairs, pack_pairs)
  pre_pack_pairs <- unique(pre_pack_pairs, by = names(pre_pack_pairs))
  find_multiple_atcs(atc_col = pre_pack_pairs$atc,
                     package_col = pre_pack_pairs$packid,
                     location = "when matching drug purchases with package parameters")

  # Prepare data ------------------------------------------------------------
  # Combine same packages of the day ----
  pre_data[is.na(ddd_pre), ddd_pre := 0]
  # Group by person, ATC, date and package
  pre_data[, bl_pack := as.integer(factor(paste(
    pid_pre, atc_pre, date_pre, packid_pre, sep = "_"
  )))]

  pre_data[, let(ddd_pre = sum(ddd_pre),
                 ratio_pre = sum(ratio_pre)), by = list(bl_pack)]

  # Aggregate
  pre_data <- unique(pre_data, by = c("bl_pack"))

  # Merge datasets ----------------------------------------------------------

  # Find the closest matching (partial) in ATC parameters and prescriptions and
  # add it to prescriptions for merging
  .find_closest_atc <- function(atc, opt_atcs) {
    while (nchar(atc) > 0) {
      if (atc %in% opt_atcs) {
        return(atc)
      }
      atc <- substr(atc, 1, nchar(atc) - 1)
    }
    return(NA)
  }
  opt_atcs <- atc_parameters$part_atc

  pre_data[, atc_short := sapply(atc_pre, .find_closest_atc, opt_atcs)]
  if (pre_data[is.na(atc_short), .N] > 0)
    stop(
      "Every ATC class in drug purhcases should exist at least with one character level in ATC parameters. Please check the ATC parameters and try again.",
      call. = FALSE
    )

  ## Merge files -------------------------------------------------------
  pre_data <- merge(
    pre_data,
    package_parameters[, .(pack_no,
                           ddd_low_pack,
                           ddd_usual_pack,
                           dur_min_pack,
                           dur_usual_pack,
                           dur_max_pack)],
    by.x = "packid_pre",
    by.y = "pack_no",
    all.x = TRUE
  )
  pre_data <- merge(
    pre_data,
    atc_parameters,
    by.x = "atc_short",
    by.y = "part_atc",
    all.x = TRUE
  )

  # Check how many purchases have DDDs recorded or vnr-info (using  usual DDD) ----
  package_info_covr <- check_coverage(pre_data$atc_pre,
                                      pre_data$dur_usual_pack,
                                      limit = limit,
                                      opti = "missing")
  if (!is.null(package_info_covr)) {
    emessage <- paste0(
      "Coverage of package parameter information in less than required (at least ",
      limit,
      "%) in following ATC(s): ",
      package_info_covr,
      "."
    )
    message(emessage)
    if (isFALSE(drop_atcs)) {
      stop("Process interrupted, dropping ATC codes with insufficient data not selected (drop_atcs = FALSE).", call. = FALSE)
    } else {
      atc_drops <- extract_atc_codes(package_info_covr)
      pre_data <- pre_data[atc_pre %notin% atc_drops, ]
      message(
        "Excluded ATC(s) with insufficient package parameter information, the process continues with the rest of the data."
      )
    }
  }
  if (nrow(pre_data) == 0)
    stop(
      "No records left after deleting ATCs without sufficient package parameter information.",
      call. = FALSE
    )

  # pid_atc_date is unit for persons' purchases for the same ATC per day
  pre_data[, pid_atc_date := as.integer(factor(paste(pid_pre, atc_pre, date_pre, sep = "_")))]
  message("Step 2/6: Calculating purchase durations...")

  ### Set DDD and duration limits ----
  # If any of package based DDD lower limit is missing,
  # take the ATC based DDD lower limit for common limit
  pre_data[, min_ddd := ifelse(all(!is.na(ddd_low_pack)), min(ddd_low_pack), ddd_low_atc), by = pid_atc_date]

  # Minimum duration can be calculated based on either package minimum duration (preferred)
  # or ATC minimum duration (always available). If several packages per day,
  # then take the maximum of minimum durations.
  pre_data[, min_dur := ifelse(!is.na(dur_min_pack),
                               ratio_pre * dur_min_pack,
                               ratio_pre * dur_min_atc)]
  pre_data[, min_dur := max(min_dur), by = pid_atc_date]

  # Because the durations were multiplied, they can be
  # shorter than global minimum or longer than
  # global maximum, reject:
  pre_data[, min_dur := pmax(pmin(min_dur, global_max), global_min)]

  # Maximum duration is the maximum of maximum durations.
  # First calculate the maximum duration for each package:
  # Use package duration, if available,
  # otherwise use ATC duration.
  # Either will be multiplied with the ratio.
  pre_data[, max_dur := ifelse(!is.na(dur_max_pack),
                               ratio_pre * dur_max_pack,
                               ratio_pre * dur_max_atc)]
  # Take the maximum of all days packages
  pre_data[, max_dur := max(max_dur), by = pid_atc_date]

  # Because the durations were multiplied, they can be
  # shorter than global minimum or longer than
  # global maximum, limit:
  pre_data[, max_dur := pmin(pmax(max_dur, global_min), global_max)]

  # Count package based ERFLs --------------------------------------------------
  .count_erfl <- function(rat,
                          usu_dur,
                          ddd,
                          usu_ddd_p,
                          usu_ddd_a,
                          min_d_a) {
    # Package's daily DDD based ERFLs are calculated based on the following rules:
    # usual duration of all packages are available
    # purchases DDD is available ( = greater than 0) for all rows
    if (all(!is.na(usu_ddd_p) & all(ddd > 0))) {
      method <- "erfl_ddd_pack"
      value <- max(rat) / sum(rat) * sum(ddd / usu_ddd_p)
      # If DDDs or usual DDD of all packages are not available
      # use packages' usual durations
    } else if (all(!is.na(usu_dur))) {
      method <- "erfl_dur_pack"
      value <- max(rat) / sum(rat) * sum(rat * usu_dur)
      # If DDDs are available but package parameters are not available
      # calculate using usual ATC DDDs
    } else if (all(!is.na(usu_ddd_a) & all(ddd > 0))) {
      method <- "erfl_ddd_atc"
      value <- sum(ddd) / unique(usu_ddd_a)
    } else {
      # Return 'min_d_a' if other conditions are not met
      method <- "erfl_min_atc"
      value <- unique(min_d_a)
    }
    return(list(method = method, value = value))
  }

  pre_data[, c("erfl_met", "erfl_pack_based") := .count_erfl(
    rat = ratio_pre,
    usu_dur = dur_usual_pack,
    ddd = ddd_pre,
    usu_ddd_p = ddd_usual_pack,
    usu_ddd_a = ddd_usual_atc,
    min_d_a = dur_min_atc
  ), by = pid_atc_date]

  ### Aggregate to person - ATC - date ----
  pre_data[, ddd_pre := sum(ddd_pre), by = pid_atc_date]

  # Count number of different packages per day,
  # if several, then will be omitted from mode calculation
  pre_data[, n_vnr := .N, by = pid_atc_date]
  pre_data <- unique(pre_data, by = "pid_atc_date")

  ###  Count time between purchases and initialize block ----

  setorder(pre_data, pid_pre, atc_pre, date_pre) # Sort by person, ATC and date
  # Init group by pid and ATC
  pre_data[, bl_init := as.integer(factor(paste(pid_pre, atc_pre, sep = "_")))]

  pre_data[, btw := date_pre - shift(date_pre, type = "lag", fill = date_pre[1L]), by = bl_init]

  # Define periods start points
  setorder(pre_data, bl_init, pid_pre, atc_pre)
  pre_data[, let(
    pid_lag = shift(pid_pre, type = "lag"),
    atc_lag = shift(atc_pre, type = "lag")
  ), by = bl_init]

  pre_data[, startp := pid_lag != pid_pre | atc_lag != atc_pre]
  pre_data[is.na(startp), startp := TRUE]

  # Set blocks based on global gap
  pre_data[, startp := ifelse(btw > global_gap_max, TRUE, startp)]
  pre_data[, block := cumsum(startp)]
  pre_data[, block_i := seq_len(.N), by = block]
  pre_data[, block_length := .N, by = block]

  # ERFL based on sliding average ------------------------------------------

  ### Count sliding average and personal purchase pattern ----
  .count_sliding_average <- function(ddd,
                                     time,
                                     w_past,
                                     w_present,
                                     w_future,
                                     w_limit,
                                     max_refill) {
    if (length(ddd) < 2) {
      return(NA)
    } else {
      # Estimate the time for the last purchase
      time_est <- min(ddd[length(ddd)] / (ddd[length(ddd) - 1] / time[length(ddd)]), max_refill)
      sliding_average <- numeric(length(ddd))
      for (i in 1:length(ddd)) {
        if (i == 1) {
          # Handle the first element
          sliding_average[i] <- sum(w_limit * ddd[i], w_future * ddd[i + 1], na.rm = TRUE) /
            sum(w_limit * time[i + 1], w_future * time[i + 2], na.rm = TRUE)
        } else if (i > 1 && i < length(ddd)) {
          # Handle the middle elements
          use_time <- ifelse(i + 2 > length(ddd), time_est, time[i + 2])
          sliding_average[i] <- sum(w_past * ddd[i - 1], w_present * ddd[i], w_future * ddd[i + 1], na.rm = TRUE) /
            sum(w_past * time[i],
                w_present * time[i + 1],
                w_future * use_time,
                na.rm = TRUE)
        } else {
          # Handle the last element
          sliding_average[i] <- sum(w_limit * ddd[i], w_limit * ddd[i - 1], na.rm = TRUE) /
            sum(w_limit * time_est, w_limit * time[i], na.rm = TRUE)
        }
      }
      sliding_average
    }
  }

  # Most operations are for purchases with at least two purchases
  idx <- pre_data$block_length > 2

  # Calculate the sliding average for the purchases
  pre_data[idx, sliding_average := .count_sliding_average(
    ddd = ddd_pre,
    time = btw,
    w_past = weight_past,
    w_present = weight_current,
    w_future = weight_next,
    w_limit = weight_first_last,
    max_refill = global_max
  ), by = block]

  ### Relative standard deviation ----
  # Limit the sliding average to 99
  pre_data[, sliding_average := pmin(sliding_average, 99)]
  .f_rsd <- function(x) {
    pmin(0.5, pmax(0.2, sqrt(sum(((x - mean(x))^2) / (length(x))
    )) / mean(x)))
  }
  pre_data[idx , rsd := .f_rsd(sliding_average), by = block]

  ## Erfl_ddd: based on temporal average of daily DDD ----
  pre_data[, sliding_average := pmin(pmax(sliding_average, min_ddd), global_ddd_high)]
  pre_data[, erfl_ddd := ddd_pre * (1 + rsd) / sliding_average]

  # Choose ERFL and limit to min and max ---------------------------------------
  pre_data[, erfl := ifelse(pre_ddd > 0  &
                              !is.na(erfl_ddd) & erfl_ddd > 0 ,
                            erfl_ddd,
                            erfl_pack_based)]
  # Save ERFL methods
  pre_data[, erfl_method := ifelse(pre_ddd > 0  &
                                     !is.na(erfl_ddd) &
                                     erfl_ddd > 0,
                                   "erfl_ddd",
                                   erfl_met)]

  # Number/proportion of packages shall not impact the ERFL based on ATC minimum
  # others will be limited to min and max
  pre_data[, erfl := ifelse(erfl_method == "erfl_min_atc", erfl, pmin(pmax(min_dur, erfl), max_dur))]

  # If ERFL doesn't reach the next purcase, restart period   ----------------
  pre_data[shift(erfl, type = "lag") < btw &
             !(startp), startp := T]

  # Check if extended ERFL reaches the next purchase ---------------------------
  ### Count hospital days between purchases ----
  pre_data[, purc_end := date_pre + erfl]
  setorder(pre_data, pid_pre, atc_pre, date_pre)
  pre_data[, next_purchase := shift(date_pre, type = "lead"), by = list(pid_pre, atc_pre)]

  if (!is.null(hosp_data)) {
    hospital_times <- calc_hosp_days(
      pre_id = pre_data$pid_pre,
      pre_atc = pre_data$atc_pre,
      curr_purc_start = pre_data$date_pre,
      exp_end = pre_data$purc_end,
      next_start = pre_data$next_purchase,
      hosp_id = hosp_data$pid_hosp,
      hosp_in = hosp_data$admission_date,
      hosp_out = hosp_data$discharge_date
    )
    # Hospital days between purchases
    pre_data$hosp_days_any <- hospital_times$hosp_days_all
    # Hospital days that started during exposure
    pre_data$hosp_days <- hospital_times$hosp_days_exp
  } else {
    # If no hospital data, set to 0
    pre_data[, hosp_days := 0]
    pre_data[, hosp_days_any := 0]
  }

  ### Extend over gap if hospitalizations and/or allowing few extra days covers the gap ----
  pre_data[, erfl_extended := erfl + pmin(hosp_days, global_hosp_max) + days_covered]
  pre_data[, prev_extended := shift(erfl_extended, type = "lag"), by = block]
  pre_data[startp == TRUE &
             btw > 0 &
             btw <= global_gap_max & !is.na(prev_extended) &
             prev_extended >= btw, startp := FALSE]

  # Stockpiling -------------------------------------------------------------

  message("Step 3/6: Stockpiling assessment...")

  # Shift previous and next temporal averages
  pre_data[, let(
    pre_ta = shift(sliding_average, type = "lag"),
    pas_ta = shift(sliding_average, type = "lead")
  ), by = block]

  # Find dropping temporal average doses
  pre_data[idx, drop := fifelse(
    !is.na(pre_ta) & !is.na(pas_ta) &
      pre_ta > sliding_average &
      (
        pas_ta > sliding_average |
          (sliding_average > pas_ta & block_i == (block_length - 1))
      ),
    TRUE,
    FALSE
  ), by = block]

  # Detect possible stockpiling
  pre_data[idx, pos_stock := fifelse(drop == TRUE &
                                       shift(startp, type = "lead") == TRUE, TRUE, FALSE), by = block]

  # Combine purchased total DDDs and times between purchases
  pre_data[idx, let(
    sp_ddd = shift(ddd_pre, type = "lag") + ddd_pre,
    sp_btw = btw + shift(btw, type = "lead")
  ), by = block]

  # Calculate sliding average for stockpiling purchases
  pre_data[idx, sp_erfl := sp_ddd * (1 + rsd) / sliding_average]

  # Maximum of stockpiling ERFLs is the summed maximum of the connected purchases
  pre_data[idx, sp_max := shift(max_dur, type = "lag") + max_dur]
  pre_data[idx, sp_erfl := pmin(sp_erfl, sp_max), by = block]

  # Count hospital days between purchases
  pre_data[idx, sp_hosp_days := shift(hosp_days, type = "lag") + hosp_days
           , by = block]
  pre_data[idx, sp_erfl_extended := sp_erfl +
             pmin(sp_hosp_days, global_hosp_max) + days_covered]

  # Final decision on stockpiling
  pre_data[idx, stock := fifelse(pos_stock == TRUE &
                                   sp_erfl_extended >= sp_btw,
                                 TRUE,
                                 FALSE)]

  # Continue period if stockpiling
  pre_data[shift(stock, type = "lag"), startp := FALSE, by = block]
  idx <- NULL

  # Create periods ----------------------------------------------------------

  pre_data[, period := cumsum(startp)]
  pre_data[, period_length := .N, by = period]

  # Use package length for periods with < 3 purchases -----------------------
  idx <- pre_data$period_length < 3
  pre_data[idx, let(erfl = erfl_pack_based, erfl_method = erfl_met)]
  pre_data[idx, purc_end := date_pre + erfl]

  # If package length is greater than length based on temporal average DDD was,
  # hospitalization may start during exposure and connect to the next purchase
  if (!is.null(hosp_data)) {
    hospital_times <- calc_hosp_days(
      pre_id = pre_data[idx]$pid_pre,
      pre_atc = pre_data[idx]$atc_pre,
      curr_purc_start = pre_data[idx]$date_pre,
      exp_end = pre_data[idx]$purc_end,
      next_start = pre_data[idx]$next_purchase,
      hosp_id = hosp_data$pid_hosp,
      hosp_in = hosp_data$admission_date,
      hosp_out = hosp_data$discharge_date
    )
    # Hospital days that started during exposure
    pre_data[idx]$hosp_days <- hospital_times$hosp_days_exp
  }

  ### Extend over gap if hospitalizations and/or allowing few extra days covers the gap ----
  pre_data[idx, erfl_extended := erfl + pmin(hosp_days, global_hosp_max) + days_covered]
  pre_data[, prev_dur := shift(erfl_extended, type = "lag"), by = block]
  pre_data[idx, startp := fifelse(!is.na(prev_dur) & prev_dur >= btw, FALSE, startp)]
  pre_data[, `:=`(period, cumsum(startp))]
  pre_data[, `:=`(period_length, .N), by = period]
  idx <- NULL

  # Index of last purchase in period
  lidx <- pre_data[, .I[.N], by = .(period)]$V1

  # Re-calculate package durations ------------------------------------------

  if (data_to_return != "periods") {
    message("Step 4/6: Calculating common package durations in data...")

    # Pick valid purchases for calculation of times between purchases
    # At least six purchases in period
    pre_data[, valid := ifelse(period_length > 5, TRUE, FALSE)]

    # hospital days
    pre_data[hosp_days_any > 0, valid := FALSE]

    # If last, time till next purchase is not valid
    pre_data[lidx, valid := FALSE]

    # Only one kind of packages per ATC
    pre_data[n_vnr > 1, valid := FALSE]

    # Only whole packages
    pre_data[ratio_pre %% 1 != 0, valid := FALSE]

    # At least five users for package in valid data
    pre_data[valid == TRUE, n_users := length(unique(pid_pre)), by = packid_pre]
    pre_data[n_users < 5, valid := FALSE]

    # Make temporary data with valid purchases
    # Lift the duration
    pre_data[, btw_next := shift(btw, type = "lead"), by = period]

    tmp <- pre_data[valid == TRUE, .(packid_pre, btw_next, ratio_pre)]

    # Calculate the mode of the refill lengths
    if (nrow(tmp) > 0) {
      modes <- mode_calculation(
        pack_id = tmp$packid_pre,
        pre_ratio = tmp$ratio_pre,
        dur = tmp$btw_next
      )

      # If package parameters have already a column named common_duration,
      # rename it to avoid overwriting. Data might be data.frame or tibble,
      # Use base R for renaming
      if ("common_duration" %in% names(package_parameters_orig)) {
        message(
          "Column name 'common_duration' already exists in package parameters. Renaming to 'common_duration.x'."
        )
        names(package_parameters_orig)[grep("common_duration", names(package_parameters_orig))] <- paste0(names(package_parameters_orig)[grep("common_duration", names(package_parameters_orig))], ".x")
      }
      # Report the packages in modes that are not in package parameters
      param_packs <- package_parameters_orig[[pack_id]]
      missing_packs <- modes[!pack_id %in% param_packs]
      n <- nrow(missing_packs)
      if (n > 0) {
        id_text <- paste0(missing_packs$pack_id,
                          " (",
                          missing_packs$common_duration,
                          ")")
        id_list <- paste(id_text, collapse = ", ")
        message(
          paste0(
            "Common duration was calculated for ",
            n,
            " package(s) not listed in package parameters; package ID (common duration in days): ",
            id_list,
            "."
          )
        )
        modes <- modes[pack_id %in% param_packs]
        rm(missing_packs, id_text, id_list)
      }
      # Merge modes to package parameters
      package_parameters_new <- merge(
        package_parameters_orig,
        modes[, .(pack_id, common_duration)],
        by.x = pack_id,
        by.y = "pack_id",
        all = TRUE
      )
      # Don't let the new values be smaller or greater than the original minimum and maximum
      package_parameters_new[["common_duration"]] <- with(
        package_parameters_new,
        ifelse(
          !is.na(common_duration) & common_duration < get(pack_dur_min),
          get(pack_dur_min),
          common_duration
        )
      )
      package_parameters_new[["common_duration"]] <- with(
        package_parameters_new,
        ifelse(
          !is.na(common_duration) & common_duration > get(pack_dur_max),
          get(pack_dur_max),
          common_duration
        )
      )
    } else {
      package_parameters_new <- NULL
    }
    if (data_to_return == "parameters" &&
        !is.null(package_parameters_new)){
      message("Common package durations calculated, returning updated package parameters.")
      return(package_parameters_new[])
    } else {
      if(data_to_return != "periods" &&
         is.null(package_parameters_new))
        message("Common package durations couldn't be re-estimated, probably due to too small data size.")
    }
  } else
    message(
      "Step 4/6: Common package duration calculation was not selected in function call; skipping this step."
    )
  if(data_to_return != "parameters"){
    # Prepare exposure periods ------------------------------------------------
    message("Step 5/6: Preparing drug use periods...")
    pre_data[, let(
      dup_start = min(date_pre),
      dup_last_purchase = max(date_pre),
      dup_n_purchases = .N
    ), by = period]
    # Calculate end dates of drug use periods ----
    pre_data[lidx, duration := ifelse(dup_n_purchases  > 2 & !is.na(rsd) &
                                        ddd_pre > 0, (ddd_pre *
                                                        (1 + rsd) / (sliding_average * (
                                                          1 + exp(-dup_n_purchases)
                                                        ))), erfl)]

    # ### Limit between minimum and maximum ----
    pre_data[lidx, duration := pmin(pmax(duration, min_dur), max_dur)]
    pre_data[, duration := ifelse(dup_n_purchases == 1,
                                  pmin(duration, global_max_single),
                                  duration)]
    pre_data[lidx, last_ends := dup_last_purchase + duration]

    # Calculate hospital days for hospitalizatios started during exposure
    if (!is.null(hosp_data)) {
      pre_data[lidx, let(
        hosp_days_end = calc_hosp_days(
          pre_id = pid_pre,
          pre_atc = atc_pre,
          curr_purc_start = dup_last_purchase,
          exp_end = last_ends,
          next_start = NA_integer_,
          hosp_id = hosp_data$pid_hosp,
          hosp_in = hosp_data$admission_date,
          hosp_out = hosp_data$discharge_date
        )$hosp_days_exp
      )]
    } else
      pre_data[, hosp_days_end := 0]

    # Add hospital days to exposure time, limited by allowed maximum
    pre_data[lidx, last_dur_hosp := duration + pmin(hosp_days_end, global_hosp_max)]
    pre_data[lidx, dup_end := dup_last_purchase + last_dur_hosp]

    message("Step 6/6: Post-processing drug use periods...")

    # Combine periods if gap is small or end exceeds the next period start
    combinable <- TRUE
    while (combinable) {
      message(paste("Current post processing percentage:", post_process_perc))
      pre_data[lidx, prev_start := shift(dup_start, type = "lag"), by = .(pid_pre, atc_pre)]
      pre_data[lidx, prev_end := shift(dup_end, type = "lag"), by = .(pid_pre, atc_pre)]
      pre_data[lidx, combine_to_past := ifelse((dup_start - prev_end) <= post_process_perc / 100 * (prev_end -
                                                                                                      prev_start),
                                               1,
                                               0)]
      pre_data[, combine_to_past := ifelse(is.na(combine_to_past), 0, combine_to_past)]
      pre_data[, combine_to_past := max(combine_to_past), by = period]

      # To avoid excess looping, reduce the percentage by 0.1 each time
      post_process_perc <- post_process_perc - 0.1

      if (nrow(pre_data[combine_to_past == 1]) == 0) {
        combinable <- FALSE
      } else {
        pre_data[combine_to_past == 1 , let(period = NA_integer_, dup_start = NA_integer_)]
        pre_data[, period := nafill(period, type = "locf"), by = .(pid_pre, atc_pre)]
        pre_data[, dup_start := nafill(dup_start, type = "locf"), by = .(pid_pre, atc_pre)]
        lidx <- pre_data[, .I[.N], by = .(period)]$V1
        pre_data[, combine_to_past := NULL]
      }
    }

    pre_data[, let(
      dup_n_purchases = .N,
      dup_last_purchase = max(date_pre),
      dup_total_DDD = sum(ddd_pre)
    ), by = period]

    # Collect periods
    pre2dupdata <- pre_data[lidx, .(
      period,
      pid_pre,
      atc_pre,
      dup_start,
      dup_end,
      dup_n_purchases,
      dup_last_purchase,
      dup_total_DDD
    )]

    # Period number is messed, make new and define exposure days and mean daily DDD.
    pre2dupdata[, period := 1:.N]
    pre2dupdata[, dup_days := round(dup_end - dup_start)]
    pre2dupdata[, dup_temporal_average_DDDs := round(dup_total_DDD / dup_days, 3)]

    # Calculate hospital days for period
    if (!is.null(hosp_data)) {
      hospital_times_dup <- calc_dup_hosp_days(
        pre2dupdata$period,
        pre2dupdata$pid_pre,
        pre2dupdata$dup_start,
        pre2dupdata$dup_end,
        hosp_data$pid_hosp,
        hosp_data$admission_date,
        hosp_data$discharge_date
      )
      pre2dupdata <- merge(pre2dupdata, hospital_times_dup, by = "period", all.x = TRUE)
      pre2dupdata[is.na(dup_hospital_days), dup_hospital_days := 0]
    } else
      pre2dupdata[, dup_hospital_days := 0]

    cols <- c("dup_start", "dup_last_purchase", "dup_end")
    pre2dupdata[, (cols) := lapply(.SD, as.Date, origin = "1970-01-01"), .SDcols = cols]

    npids <- length(unique(pre2dupdata$pid_pre))
    setcolorder(
      pre2dupdata,
      c(
        "period",
        "pid_pre",
        "atc_pre",
        "dup_start",
        "dup_end",
        "dup_days",
        "dup_hospital_days",
        "dup_n_purchases",
        "dup_last_purchase",
        "dup_total_DDD",
        "dup_temporal_average_DDDs"
      )
    )
    setnames(pre2dupdata,
             c("pid_pre", "atc_pre"),
             c(pre_person_id, pre_atc))

    # Inform user
    message(
      "Drug use periods calculated. ",
      nrow(pre2dupdata),
      " periods created for ",
      npids,
      " persons."
    )

    # Return results ----------------------------------------------------------
    if(data_to_return == "periods"){
      message("Returning drug use periods.")
      return(pre2dupdata[])
    } else {
      if (data_to_return == "both" && !is.null(package_parameters_new)) {
        message("Returning a list of two: drug use periods (periods) and updated package parameters (package parameters).")
        return(list(periods = pre2dupdata[], package_parameters = package_parameters_new[]))
      } else {
        message("Returning drug use periods.")
        return(pre2dupdata[])
      }
    }
  }
}
