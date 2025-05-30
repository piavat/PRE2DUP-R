ATC_class <- c("N05AA01", "N05A", "N05AA03", "N05AA04", "N05AA05")
lower_ddd = c(0.167, 0.500, 0.500, 0.375, 0.714)
usual_ddd <- c(0.33, 1.00, 1.00, 0.75, 1.00)
minimum_duration <- c(33, 9.3, 9.3, 33.3, 14)
maximum_duration <- c(200, 56, 56, 200, 42)
df_atc_params <- data.frame(ATC_class,lower_ddd,
                            usual_ddd,
                            minimum_duration,
                            maximum_duration)
test_that("check_atc_parameters works as expected with valid data", {
  # Data without errors, all relevant checks passed. Function returns data.table.

  # Passed package parameters
  outdata <- check_atc_parameters(
    dt = df_atc_params,
    atc_class = "ATC_class",
    atc_ddd_low = "lower_ddd",
    atc_ddd_usual = "usual_ddd",
    atc_dur_min = "minimum_duration",
    atc_dur_max = "maximum_duration",
    return_data = TRUE
  )
  expect_true(is.data.table(outdata))
  expect_true(all(dim(outdata) == dim(df_atc_params)))
  expect_true(typeof(outdata$ATC_class) == "character")
  expect_true(all(typeof(outdata$lower_ddd) == "double"))
  expect_true(all(typeof(outdata$usual_ddd) == "double"))
  expect_true(all(typeof(outdata$minimum_duration) == "double"))
  expect_true(all(typeof(outdata$maximum_duration) == "double"))
  expect_true(all(outdata$lower_ddd < outdata$usual_ddd))
  expect_true(all(outdata$minimum_duration < outdata$usual_duration))
  expect_true(all(outdata$usual_duration < outdata$maximum_duration))
})

test_that("check_atc_parameters notices missing arguments", {
  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params,
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd",
      atc_dur_min = "minimum_duration",
      # atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params,
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd",
      # atc_dur_min = "minimum_duration",
      # atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})

test_that("check_atc_parameters notices the columns that are missing", {
  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params[, -2],
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd",
      atc_dur_min = "minimum_duration",
      atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )

  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params[, -c(2, 3)],
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd",
      atc_dur_min = "minimum_duration",
      atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})

test_that("check_atc_parameters warns if durations or DDD limits are in wrong order", {
  df_atc_params$minimum_duration_error <- c(300, 100, 28, 20, 500)
  df_atc_params$usual_ddd_error <- c(0.1, 0.5, 0.2, 0.2, 1)
  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params,
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd_error",
      atc_dur_min = "minimum_duration_error",
      atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})
test_that("check_atc_parameters warns if duplicated ATC classes are found", {
  df_atc_params$ATC_class[2] <- df_atc_params$ATC_class[1]
  expect_snapshot(
    check_atc_parameters(
      dt = df_atc_params,
      atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd",
      atc_ddd_usual = "usual_ddd",
      atc_dur_min = "minimum_duration",
      atc_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})

test_that("check_atc_parameters gives error if ATC parameters don't have any data", {
  # Data without errors, all relevant checks passed. Function returns data.table.
  atc_params_empty <- data.frame(ATC_class = character(),
                                 lower_ddd = numeric(),
                                 usual_ddd = numeric(),
                                 minimum_duration = numeric(),
                                 maximum_duration = numeric())
  expect_snapshot(check_atc_parameters(
    dt = atc_params_empty,
    atc_class = "ATC_class",
    atc_ddd_low = "lower_ddd",
    atc_ddd_usual = "usual_ddd",
    atc_dur_min = "minimum_duration",
    atc_dur_max = "maximum_duration"
  ), error = TRUE)
})
test_that("check_atc_parameters reports that check was passed", {
  # Data without errors, all relevant checks passed. Function returns data.table.
  expect_snapshot(check_atc_parameters(
    dt = ATC_parameters,
    atc_class = "partial_atc",
    atc_ddd_low = "lower_ddd_atc",
    atc_ddd_usual = "usual_ddd_atc",
    atc_dur_min = "minimum_dur_atc",
    atc_dur_max = "maximum_dur_atc"
  ))
})
