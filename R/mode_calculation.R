#' Calculates the most common duration between purchases
#'
#' Calculates the mode of time intervals between purchases for each package.
#' Intervals within 2 days are grouped together.
#' The most common (modal) interval is selected from the grouped values.
#' If multiple modes exist, the one closest to the median is chosen;
#' if there's still a tie, the one closest to the mean is selected.
#' Intended for internal use in the PRE2DUP-R package.
#'
#' @param pack_id vector of package identifiers
#' @param pre_ratio vector of package ratios, i.e. number of packages because partial packages are not allowed
#' @param dur vector of times between purchases
#'
#' @returns data.table with the most common duration between purchases
#'
#' @keywords internal
#' @importFrom stats median
mode_calculation <- function(pack_id, pre_ratio, dur, report_iters = 50) {

  # Create a data.table with each duration
  dt <- data.table(pack_id,  pre_ratio, dur)

  # Estimate the duration of one package
  dt[, dur := round(dur/pre_ratio)]

  # Sum packages by duration and pack_id
  dt[, pre_ratio := sum(pre_ratio), by = .(dur, pack_id)]

  # Count purchases per duration
  dt[, n_purchases := .N, by = .(dur, pack_id)]

  # Keep only unique rows for each duration per package
  dt <- unique(dt, by = c("dur", "pack_id"))[order(pack_id, dur)]

  # Compute median and mean durations (weighted by number of packages)
  dt[, median_time := median(rep(dur, times = pre_ratio)), by = pack_id]
  dt[, mean_time := mean(rep(dur, times = pre_ratio)), by = pack_id]
  # Process combines close times to each other. They are combined
  # if there are maximum 2 days difference and one of them is more common than other.
  iter <- 0L
  repeat {
    iter <- iter + 1L
    if (iter %% report_iters == 0) message("Merging... iteration ", iter)

    # Create lead/lag variables
    dt[, `:=`(
      prev_dur = shift(dur, type = "lag"),
      next_dur = shift(dur, type = "lead"),
      prev_ratio = shift(pre_ratio, type = "lag"),
      next_ratio = shift(pre_ratio, type = "lead")
    ),
    by = pack_id]

    dt[, `:=`(
      con_prev = !is.na(prev_dur) & (dur - prev_dur) < 3 & (pre_ratio != prev_ratio),
      con_next = !is.na(next_dur) & (next_dur - dur) < 3 & (pre_ratio != next_ratio),
      ratio_less = !is.na(next_ratio) & !is.na(prev_ratio) & (next_ratio < prev_ratio)
    ), by = pack_id]

    ## Exit if no more combinable durations
    if (!any(dt$con_prev | dt$con_next)) break

    # Merge connectable durations within two days:
    # If several choices prioritize the closest
    # if neighboring durations are equally close
    # choose the one with more packages.
    min_N <- min(dt[con_prev | con_next]$pre_ratio)
    dt[, new_dur := fifelse(
      pre_ratio == min_N & con_prev & con_next,
      fifelse(
        abs(next_dur - dur) < abs(dur - prev_dur), next_dur,
        fifelse(
          abs(next_dur - dur) > abs(dur - prev_dur), prev_dur,
          fifelse(
            next_ratio > prev_ratio, next_dur, prev_dur
          )
        )
      ),
      fifelse(
        pre_ratio == min_N & con_prev & !con_next, prev_dur,
        fifelse(
          pre_ratio == min_N & !con_prev & con_next, next_dur,
          dur
        )
      )
    )]

    # Break if durations doesn't change
    if (all(dt$new_dur == dt$dur)) break

    dt[, dur := new_dur][, new_dur := NULL]

    # Recalculate totals for combined durations
    dt[, let(pre_ratio = sum(pre_ratio), n_purchases = sum(n_purchases)), by = .(pack_id, dur)]
    dt <- unique(dt, by = c("pack_id", "dur"))
  }

  # Select the most common duration (by number of purchases)
  dt[, max_purchases := max(n_purchases), by = pack_id]
  dt <- dt[n_purchases == max_purchases]

  # Handle ties: use median and mean distances
  dt[, n_cases := .N, by = pack_id]

    if (nrow(dt[n_cases > 1]) == 0) {
    dt <- dt[order(pack_id, pre_ratio, n_purchases), .SD[1], by = pack_id]
  } else {
    dt[, diff_median := abs(dur - median_time)]
    dt[, diff_mean := abs(dur - mean_time)]
    dt <- dt[order(pack_id, diff_median, diff_mean, dur), .SD[1], by = pack_id]
  }
  # Return only results with at least 10 purchases
  dt <- dt[n_purchases >= 10]
  dt <- dt[, .(pack_id, common_duration = dur, n_purchases, n_pack = pre_ratio)]
  return(dt)
}
