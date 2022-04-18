#' Count sliding average
#'
#' This function counts sliding average of Defined Daily
#' Dose (DDD) from puchased DDDs and times between purchases.
#'
#' @param  ddd Amount of DDDs purchased.
#' @param  time Time from previous purchase. For first purchase 0.
#' @param  w_past Weight for purchase before current purchase.
#' @param  w_present Weight for current purchase.
#' @param  w_future Weight for purchase after current purchase.
#' @param  w_limit Weight for first and last purchase.
#' @param  max_refill Maximum time allowed between purchases.
#'
#' @return This function returns a number vector
#' with sliding averages
#'
#' @examples \dontrun{
#' ddds <- c(15, 15, 15, 15, 15)
#' times <- c(0, 59, 54, 69, 51)
#' count_sliding_average(ddds, times)
#' }
#'
#'
#' @export
count_sliding_average <- function(ddd, time, w_past = 1, w_present = 4, w_future = 1, w_limit = 5, max_refill = 300){
  time_est <- min(ddd[length(ddd)]/(ddd[length(ddd)-1]/time[length(ddd)]), max_refill)
  sliding_average <- c()
  for(i in 1:length(ddd)){
    if(i == 1) {
      sliding_average[i] <- sum(w_limit*ddd[i], w_future*ddd[i + 1], na.rm = TRUE)/
        sum(w_limit*time[i + 1], w_future*time[i + 2], na.rm = TRUE)
    } else {
      if(i > 1 && i < length(ddd)){
        use_time <- ifelse(i+2 > length(ddd), time_est, time[i + 2])
        sliding_average[i] <- sum(w_past*ddd[i - 1], w_present*ddd[i], w_future*ddd[i + 1], na.rm = TRUE)/
          sum(w_past*time[i], w_present*time[i + 1], w_future*use_time, na.rm = TRUE)
      } else
        sliding_average[i] <- sum(w_limit*ddd[i], ddd[i - 1], na.rm = TRUE)/sum(w_limit*time_est, time[i], na.rm = TRUE)
    }}
  sliding_average
}

#' Make block
#'
#' This function makes blocks of drug purchases based on
#' maximum allowed time between purchases
#'
#' @param  btw Time between purchases.
#' @param  max_length Maximum allowed time between purchases.
#'
#' @return This function returns a vector of numbers.
#'
#' @examples \dontrun{
#' times <- c(0, 31, 44, 22, 110, 350, 33, 42, 54, 100, 500, 24, 33, 37)
#' make_block(times)
#' }
#'
#'
#' @note For the first purchase time between purchases should be 0.
#'
#' @export
make_block <- function(btw,  max_length = 300){
  stopifnot(is.numeric(max_length))
  block <- ifelse(btw == 0, 1, 0)
  ind <- which(btw > max_length)
  block[ind] <- 1
  block <- cumsum(block)
  block
}

