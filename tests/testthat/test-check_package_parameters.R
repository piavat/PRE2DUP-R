ATC <- c("N05AA01", rep("N05AH03", 3), "N05AX13")
vnr <- c(141473, 80307, 145698, 457780 , 412581)
lower_ddd = c(0.167, 0.500, 0.500, 0.375, 0.714)
usual_ddd <- c(0.33, 1.00, 1.00, 0.75, 1.00)
minimum_duration <- c(33, 9.3, 9.3, 33.3, 14)
usual_duration <- c(100, 28, 28, 100, 30)
maximum_duration <- c(200, 56, 56, 200, 42)
df_pack_params <- data.frame(ATC,
                             vnr,
                             lower_ddd,
                             usual_ddd,
                             minimum_duration,
                             usual_duration,
                             maximum_duration)

# Test indata_check function for package parameters
test_that("check_package_parameters works as expected with valid data", {
  # Data without errors, all relevant checks passed. Function returns data.table.

  # Passed package parameters
  outdata <- check_package_parameters(
    dt = df_pack_params,
    pack_atc = "ATC",
    pack_id = "vnr",
    pack_ddd_low = "lower_ddd",
    pack_ddd_usual = "usual_ddd",
    pack_dur_min = "minimum_duration",
    pack_dur_usual = "usual_duration",
    pack_dur_max = "maximum_duration",
    return_data = TRUE
  )
  expect_true(is.data.table(outdata))
  expect_true(all(dim(outdata) == dim(df_pack_params)))
  expect_true(typeof(outdata$ATC) == "character")
  expect_true(typeof(outdata$vnr) == "integer")
  expect_true(all(typeof(outdata$lower_ddd) == "double"))
  expect_true(all(typeof(outdata$usual_ddd) == "double"))
  expect_true(all(typeof(outdata$minimum_duration) == "double"))
  expect_true(all(typeof(outdata$usual_duration) == "double"))
  expect_true(all(typeof(outdata$maximum_duration) == "double"))
  expect_true(all(outdata$lower_ddd < outdata$usual_ddd))
  expect_true(all(outdata$minimum_duration < outdata$usual_duration))
  expect_true(all(outdata$usual_duration < outdata$maximum_duration))
})

test_that("check_package_parameters notices missing arguments", {

  expect_snapshot(
    check_package_parameters(
      dt = df_pack_params,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      # pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration",
      pack_dur_max = "maximum_duration",
      return_data = TRUE
    ),
    error = TRUE
  )
  expect_snapshot(
    check_package_parameters(
      dt = df_pack_params,
      # pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      # pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration",
      pack_dur_max = "maximum_duration",
      return_data = TRUE
    ),
    error = TRUE
  )
})

test_that("check_package_parameters notices the columns that are missing", {
  expect_snapshot(
    check_package_parameters(df_pack_params[, -1],
                             pack_atc = "ATC",
                             pack_id = "vnr",
                             pack_ddd_low = "lower_ddd",
                             pack_ddd_usual = "usual_ddd",
                             pack_dur_min = "minimum_duration",
                             pack_dur_usual = "usual_duration",
                             pack_dur_max = "maximum_duration"),
    error = TRUE
  )
  expect_snapshot(
    check_package_parameters(df_pack_params[, -c(1, 5)],
                             pack_atc = "ATC",
                             pack_id = "vnr",
                             pack_ddd_low = "lower_ddd",
                             pack_ddd_usual = "usual_ddd",
                             pack_dur_min = "minimum_duration",
                             pack_dur_usual = "usual_duration",
                             pack_dur_max = "maximum_duration"),
    error = TRUE
  )
})
test_that("check_package_parameters warns if durations or DDD limits are in wrong order", {
  df_pack_params$vnr[2] <- ""
  df_pack_params$usual_duration_error <- c(100, 28, 28, 20, 50)
  df_pack_params$usual_ddd_error <- c(0.1, 0.5, 0.2, 0.2, 1)
  expect_snapshot(
  check_package_parameters(
    dt = df_pack_params,
    pack_atc = "ATC",
    pack_id = "vnr",
    pack_ddd_low = "lower_ddd",
    pack_ddd_usual = "usual_ddd_error",
    pack_dur_min = "minimum_duration",
    pack_dur_usual = "usual_duration_error",
    pack_dur_max = "maximum_duration"
  ),
    error = TRUE
  )
})

test_that("check_package_parameters warns if duplicated package parameters are found", {
  df_pack_params$vnr[2] <- df_pack_params$vnr[1]
  expect_snapshot(
    check_package_parameters(
      dt = df_pack_params,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration",
      pack_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})

test_that("check_package_parameters warns if there are no records in parameter file", {
  df_pack_params_empty <- df_pack_params[0, ]
  expect_snapshot(
    check_package_parameters(
      dt = df_pack_params_empty,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration",
      pack_dur_max = "maximum_duration"
    ),
    error = TRUE
  )
})

test_that("check_package_parameters informs that data has passed check", {
  expect_snapshot(
    check_package_parameters(
      dt = df_pack_params,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration",
      pack_dur_max = "maximum_duration"
    )
  )
})
