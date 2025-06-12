library(data.table)
test_that("calc_dup_hosp_days calculates a hospitalization inside period", {
  dup_data <- data.table(
    period = c(1, 2, 3),
    pid = c(1, 1, 1),
    dup_start = as.Date(c("2023-01-01", "2024-02-01", "2025-03-01")),
    dup_end = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")))
  
  hosp_data <- data.table(
    pid_hosp = c(1),
    admission_date = as.Date("2023-02-01"),
    discharge_date = as.Date("2023-03-01"))
  
  hosps <- calc_dup_hosp_days(
    period_no = dup_data$period,
    pre_id = dup_data$pid,
    dup_start = as.integer(dup_data$dup_start),
    dup_end = as.integer(dup_data$dup_end),
    hosp_id = hosp_data$pid_hosp,
    hosp_in = as.integer(hosp_data$admission_date),
    hosp_out = as.integer(hosp_data$discharge_date)
  )
  
  expect_equal(nrow(hosps), 1)
  expect_equal(hosps$period, 1)
  expect_equal(hosps$dup_hospital_days, 27)
})

test_that("calc_dup_hosp_days calculates a hospitalization if always on hospital", {
  dup_data <- data.table(
    period = c(1, 2, 3),
    pid = c(1, 1, 1),
    dup_start = as.Date(c("2023-01-01", "2024-02-01", "2025-03-01")),
    dup_end = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")))
  
  hosp_data <- data.table(
    pid_hosp = c(1),
    admission_date = as.Date("2022-02-01"),
    discharge_date = as.Date("2026-03-01"))
  
  hosps <- calc_dup_hosp_days(
    period_no = dup_data$period,
    pre_id = dup_data$pid,
    dup_start = as.integer(dup_data$dup_start),
    dup_end = as.integer(dup_data$dup_end),
    hosp_id = hosp_data$pid_hosp,
    hosp_in = as.integer(hosp_data$admission_date),
    hosp_out = as.integer(hosp_data$discharge_date)
  )
  
  expect_equal(nrow(hosps), 3)
  expect_equal(hosps$period, c(1, 2, 3))
  expect_equal(hosps$dup_hospital_days, c(160, 191, 284))
})

test_that("calc_dup_hosp_days calculates a hospitalization partly inside period", {
  dup_data <- data.table(
    period = c(1, 2, 3),
    pid = c(1, 1, 1),
    dup_start = as.Date(c("2023-01-01", "2024-02-01", "2025-03-01")),
    dup_end = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")))
  
  hosp_data <- data.table(
    pid_hosp = c(1),
    admission_date = as.Date("2023-02-01"),
    discharge_date = as.Date("2023-07-01"))
  
  hosps <- calc_dup_hosp_days(
    period_no = dup_data$period,
    pre_id = dup_data$pid,
    dup_start = as.integer(dup_data$dup_start),
    dup_end = as.integer(dup_data$dup_end),
    hosp_id = hosp_data$pid_hosp,
    hosp_in = as.integer(hosp_data$admission_date),
    hosp_out = as.integer(hosp_data$discharge_date)
  )
  
  expect_equal(nrow(hosps), 1)
  expect_equal(hosps$period, 1)
  expect_equal(hosps$dup_hospital_days, 129)
})

test_that("calc_dup_hosp_days ignores hospitalizations starting at end date", {
  dup_data <- data.table(
    period = c(1, 2, 3),
    pid = c(1, 1, 1),
    dup_start = as.Date(c("2023-01-01", "2024-02-01", "2025-03-01")),
    dup_end = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")))
  
  hosp_data <- data.table(
    pid_hosp = c(1),
    admission_date = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")),
    discharge_date = as.Date(c("2023-10-10", "2024-10-10", "2026-01-10")))
  
  hosps <- calc_dup_hosp_days(
    period_no = dup_data$period,
    pre_id = dup_data$pid,
    dup_start = as.integer(dup_data$dup_start),
    dup_end = as.integer(dup_data$dup_end),
    hosp_id = hosp_data$pid_hosp,
    hosp_in = as.integer(hosp_data$admission_date),
    hosp_out = as.integer(hosp_data$discharge_date)
  )
  
  expect_equal(sum(hosps$dup_hospital_days), 0)
})

test_that("calc_dup_hosp_days handles a hospitalization covering two periods", {
  dup_data <- data.table(
    period = c(1, 2, 3),
    pid = c(1, 1, 1),
    dup_start = as.Date(c("2023-01-01", "2024-02-01", "2025-03-01")),
    dup_end = as.Date(c("2023-06-10", "2024-08-10", "2025-12-10")))
  
  hosp_data <- data.table(
    pid_hosp = c(1),
    admission_date = as.Date(c("2023-01-01")),
    discharge_date = as.Date(c("2024-02-07")))
  
  hosps <- calc_dup_hosp_days(
    period_no = dup_data$period,
    pre_id = dup_data$pid,
    dup_start = as.integer(dup_data$dup_start),
    dup_end = as.integer(dup_data$dup_end),
    hosp_id = hosp_data$pid_hosp,
    hosp_in = as.integer(hosp_data$admission_date),
    hosp_out = as.integer(hosp_data$discharge_date)
  )
  
  expect_equal(nrow(hosps), 2)
  expect_equal(hosps$period, c(1, 2))
  expect_equal(hosps$dup_hospital_days, c(160, 5))
})

