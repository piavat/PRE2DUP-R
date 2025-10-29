library(intervals)
PID <- c(1, 1, 2, 2)
Entry <- c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")
Leave <- c("2023-01-15", "2023-02-15", "2023-01-10", "2023-02-10")
hospital_data <- data.frame(PID, Entry, Leave)

test_that("check_hospitalizations works as expected for errorless hospitalization data", {

  # All data is correct
  outdata <- suppressWarnings(suppressMessages(check_hospitalizations(
    hospital_data,
    hosp_person_id = "PID",
    hosp_admission = "Entry",
    hosp_discharge = "Leave",
    return_data = TRUE
  )))
  expect_true(is.data.table(outdata))
  expect_true(is.factor(outdata$pid_hosp))
  expect_true(is.integer(outdata$admission_date))
  expect_true(is.integer(outdata$discharge_date))
  expect_equal(outdata$admission_date, c(19358, 19389, 19358, 19389))
  expect_equal(outdata$discharge_date, c(19372, 19403, 19367, 19398))
})

test_that("check_hospitalizations detects missing arguments", {
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      date_range = c("2026-01-01", "2024-02-01")
    ),
    error = TRUE
  )
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID"),
    error = TRUE
  )
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("2026-01-01", "2024-02-01")
    ),
    error = TRUE
  )
})

test_that("check_hospitalizations detects missing data columns", {
  # Required columns are missing
  hospital_data$Entry <- NULL

  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave"
    ),
    error = TRUE
  )
  hospital_data$Leave <- NULL
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave"
    ),
    error = TRUE
  )
  hospital_data$Leave <- Leave
  hospital_data$Entry <- Entry
})

test_that("check_hospitalizations detects errorneus date ranges", {
  # Date range
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("2026-01-01", "2024-02-01")
    ),
    error = TRUE
  )
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("")
    ),
    error = TRUE
  )
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("2026-01-01")
    ),
    error = TRUE
  )
})

test_that("check_hospitalizations detects dates outside date ranges", {
  # Dates outside date range
  hospital_data$Entry[1] <- "1980-01-01"
  hospital_data$Leave[4] <- "2244-02-10"
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("2020-01-01", "2024-02-01")
    ),
    error = TRUE
  )
  hospital_data$Entry <- Entry
  hospital_data$Leave <- Leave
})

test_that("check_hospitalizations detects dates in wrong order", {
  # Hospital admission date is after or at discharge date
  hospital_data$Leave[3] <- "2021-01-10"

  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave",
      date_range = c("2020-01-01", "2024-02-01")
    ),
    error = TRUE
  )
  hospital_data$Leave <- Leave
})

test_that("check_hospitalizations detects overlapping hospitalizations", {

  # No overlapping hospitalizations
  expect_snapshot(
    check_hospitalizations(
    hospital_data,
    hosp_person_id = "PID",
    hosp_admission = "Entry",
    hosp_discharge = "Leave",
    date_range = c("2020-01-01", "2024-02-01")
  ),
  error = FALSE)

  # Hospitalizations are overlapping
  hospital_data$Leave[1] <- "2023-02-03"
  hospital_data$Leave[3] <- "2023-02-03"

  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave"
    ),
    error = FALSE
  )

  pid = c(
    "pid1",
    "pid1",
    "pid2",
    "pid2",
    "pid2",
    "pid2",
    "pid3",
    "pid3",
    "pid4",
    "pid4",
    "pid5",
    "pid5",
    "pid5"
  )
  admission = c(
    "2011-01-31",
    "2011-02-05",
    "2011-01-27",
    "2011-02-07",
    "2011-02-13",
    "2011-02-14",
    "2011-01-27",
    "2011-01-29",
    "2011-01-27",
    "2011-02-06",
    "2023-01-25",
    "2023-01-28",
    "2023-02-01"
  )
  discharge = c(
    "2011-02-06",
    "2011-02-15",
    "2011-01-30",
    "2011-02-17",
    "2011-02-20",
    "2011-02-25",
    "2011-02-06",
    "2011-02-01",
    "2011-02-06",
    "2011-02-16",
    "2023-01-28",
    "2023-02-01",
    "2023-02-06"
  )
  test_data <- data.frame(pid, admission, discharge)

  expect_snapshot(
    check_hospitalizations(
      test_data,
      hosp_person_id = "pid",
      hosp_admission = "admission",
      hosp_discharge = "discharge"
    ),
    error = FALSE
  )

  # Make test data
  expect_snapshot(
    outdata <- check_hospitalizations(
      test_data,
      hosp_person_id = "pid",
      hosp_admission = "admission",
      hosp_discharge = "discharge",
      return_data = TRUE
    ),
    error = FALSE
  )

  expect_true(is.data.table(outdata))
  expect_true(is.factor(outdata$pid_hosp))
  expect_true(is.integer(outdata$admission_date))
  expect_true(is.integer(outdata$discharge_date))
  expect_equal(as.character(as.Date(outdata$admission_date)), c("2011-01-31", "2011-01-27", "2011-02-07", "2011-01-27", "2011-01-27", "2023-01-25"))
  expect_equal(as.character(as.Date(outdata$discharge_date)), c("2011-02-15", "2011-01-30", "2011-02-25", "2011-02-06", "2011-02-16", "2023-02-06"))
})

test_that("check_hospitalizations reports error if the hospitalization data has not records", {
  hospital_data <- data.frame(
    PID = character(),
    Entry = as.Date(character()),
    Leave = as.Date(character())
  )
  expect_snapshot(
    check_hospitalizations(
      hospital_data,
      hosp_person_id = "PID",
      hosp_admission = "Entry",
      hosp_discharge = "Leave"),,
    error = TRUE
  )
})

test_that("check_hospitalizations connects consecutive hospitalizations", {

  # All data is correct
  hospital_data <- data.table(
    PID = c(1, 1, 1, 1),
    Entry = as.Date(c("2020-01-01", "2020-01-30", "2020-03-01", "2020-03-30")),
    Leave = as.Date(c("2020-01-30", "2020-03-01", "2020-03-30", "2020-04-10"))
  )
  outdata <- suppressMessages(check_hospitalizations(
    hospital_data,
    hosp_person_id = "PID",
    hosp_admission = "Entry",
    hosp_discharge = "Leave",
    return_data = TRUE
  ))
  expect_true(is.data.table(outdata))
  expect_true(is.factor(outdata$pid_hosp))
  expect_true(is.integer(outdata$admission_date))
  expect_true(is.integer(outdata$discharge_date))
  expect_equal(outdata$admission_date, 18262)
  expect_equal(outdata$discharge_date, 18362)
})

test_that("check_hospitalizations connects consecutive hospitalizations for two person", {

  # All data is correct
  hospital_data <- data.table(
    PID = c(1, 1, 2, 2),
    Entry = as.Date(c("2020-01-01", "2020-01-30", "2020-03-01", "2020-03-30")),
    Leave = as.Date(c("2020-01-30", "2020-03-01", "2020-03-30", "2020-04-10"))
  )
  outdata <- suppressMessages(check_hospitalizations(
    hospital_data,
    hosp_person_id = "PID",
    hosp_admission = "Entry",
    hosp_discharge = "Leave",
    return_data = TRUE
  ))

  expect_true(is.data.table(outdata))
  expect_true(is.factor(outdata$pid_hosp))
  expect_true(is.integer(outdata$admission_date))
  expect_true(is.integer(outdata$discharge_date))
  expect_equal(outdata$admission_date, c(18262, 18322))
  expect_equal(outdata$discharge_date,c(18322, 18362))
})
