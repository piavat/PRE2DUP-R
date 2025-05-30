library(mockery)
# These tests are for the check_purchases function
test_data <- data.frame(
  person_id = rep(100001, 7),
  atc = rep("N05AH02", 7),
  package_vnr = rep(12345, 7),
  purchase_date = as.Date("2022-01-01") + 30*0:6,
  ratio = c(1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0),
  total_ddd = c(100, 100, 100, 100, 200, 50, 100)
)

test_data_errors <- data.frame(
  person_id = c(rep(100001, 3), NA, 0, -100001, Inf),
  atc = c("    ", " ", "@@", "%", NA, "12345", ""),
  package_vnr = c(12345, 12346, NA, 12347, -12345, 0, 12348),
  purchase_date = c("2022-01-05", "  ", NA, "2022&01&04", "", "2022-01-06", "2022-01-07"),
  ratio = c(1.0, 1.0, 0, -1.0, NA, NaN, Inf),
  total_ddd = c(100, 100, 0, 100,  50, 10, 3)
)




# Test indata_check function for package parameters
test_that("check_purchases works as expected with valid data", {
  # Data without errors, all relevant checks passed. Function returns data.table.

  # Passed package parameters
  outdata <- check_purchases(test_data,
                             pre_person_id = "person_id",
                             pre_atc = "atc",
                             pre_package_id = "package_vnr",
                             pre_date = "purchase_date",
                             pre_ratio = "ratio",
                             pre_ddd = "total_ddd",
                             return_data = TRUE)

  expect_true(is.data.table(outdata))
  # expect_true(all(dim(outdata) == dim(test_data)))
  expect_true(is.factor(outdata$person_id))
  expect_true(typeof(outdata$atc) == "character")
  expect_true(typeof(outdata$package_vnr) == "integer")
  expect_true(all(typeof(outdata$date_pre) == "integer"))
  expect_true(all(typeof(outdata$ratio) == "double"))
  expect_true(all(typeof(outdata$total_ddd) == "double"))
})

test_that("check_purchases informs that test is passed", {
  # Data without errors, all relevant checks passed. Function returns data.table.

  # Passed package parameters
  expect_snapshot(
    check_purchases(test_data,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd",
                    print_all = TRUE),
    error = FALSE
  )
})

# Test cases
test_that("check_purchases stops if dataset is empty", {
  expect_error(check_purchases(data.frame()), "No data in the dataset")
})

test_that("check_purchases notices missing arguments", {
  expect_snapshot(
    check_purchases(test_data,
                    #pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd"
    ),
    error = TRUE
  )
  expect_snapshot(
    check_purchases(test_data,
                    #pre_person_id = "person_id",
                    # pre_atc = "atc",
                    # pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio"
                    # pre_ddd = "total_ddd"
    ),
    error = TRUE
  )
})

test_that("check_purchases stops if required columns are missing", {
  expect_snapshot(
    check_purchases(test_data[, -2 ],
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd"),
    error = TRUE
  )
  expect_snapshot(
    check_purchases(test_data[, -c(2, 4) ],
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd"),
    error = TRUE
  )
})

test_that("check_purchases gives warnings about invalid values", {
  expect_snapshot(
    check_purchases(test_data_errors,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd"),
    error = TRUE
  )
})

test_that("check_purchases accepts character person ID and returns it as factor", {
  test_data$person_id_char <- as.character(test_data$person_id)
  outdata <- check_purchases(test_data,
                             pre_person_id = "person_id_char",
                             pre_atc = "atc",
                             pre_package_id = "package_vnr",
                             pre_date = "purchase_date",
                             pre_ratio = "ratio",
                             pre_ddd = "total_ddd",
                             return_data = TRUE)
  expect_true(is.factor(outdata$person_id))
})


test_that("check_purchases accepts factor person ID and returns it as factor", {
  test_data$person_id_fac <- as.factor(test_data$person_id)
  outdata <- check_purchases(test_data,
                             pre_person_id = "person_id_fac",
                             pre_atc = "atc",
                             pre_package_id = "package_vnr",
                             pre_date = "purchase_date",
                             pre_ratio = "ratio",
                             pre_ddd = "total_ddd",
                             return_data = TRUE)
  expect_true(is.factor(outdata$person_id))
})

test_that("check_purchases accepts numeric person ID and returns it as factor", {
  outdata <- check_purchases(test_data,
                             pre_person_id = "person_id",
                             pre_atc = "atc",
                             pre_package_id = "package_vnr",
                             pre_date = "purchase_date",
                             pre_ratio = "ratio",
                             pre_ddd = "total_ddd",
                             return_data = TRUE)
  expect_true(is.factor(outdata$person_id))
})

test_that("check_purchases warns about wrongly set date range", {
  expect_snapshot(
    check_purchases(test_data,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd",
                    date_range = c("2018-01-01", "2017-12-31")),
    error = TRUE
  )
})

test_that("check_purchases warns about purchases outside set date range", {
  expect_snapshot(
    check_purchases(test_data,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd",
                    date_range = c("2022-01-05", "2022-06-20")),
    error = TRUE
  )
})

test_that("check_purchases stops if one package has several ATCs", {
  test_data2 <- test_data
  test_data2$atc[2:3] <- "N05AH"
  test_data2$package_vnr <- c(rep(12345, 2), rep(54321, 5))
  expect_snapshot(
    check_purchases(test_data2,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd",
                    date_range = c("2022-01-05", "2022-06-20")),
    error = TRUE
  )
})
test_that("check_purchases informs ATCs without sufficient information and continues with the rest", {
  test_data_missing_ddd <- data.frame(
    person_id = rep(100001, 10),
    atc = c(rep("N05AH02", 10), rep("N05AH03", 10)),
    package_vnr = c(rep(12345, 10), rep(123456, 10)),
    purchase_date = as.Date("2022-01-01") + 30*0:19,
    ratio = rep(1, 20),
    total_ddd = c(rep(100, 8), NA, NA, rep(100, 10))
  )
  mock_readline <- mock("y")
  stub(check_purchases, "readline", mock_readline)

  expect_snapshot(
   outdata <-  check_purchases(test_data_missing_ddd,
                    pre_person_id = "person_id",
                    pre_atc = "atc",
                    pre_package_id = "package_vnr",
                    pre_date = "purchase_date",
                    pre_ratio = "ratio",
                    pre_ddd = "total_ddd",
                    return_data = TRUE)
   )
  expect_true(is.data.table(outdata))
  expect_true(all(outdata$atc == "N05AH03"))
})

test_that(
  "check_purchases informs ATCs without sufficient information and stops with promt 'no'",
  {
    test_data_missing_ddd <- data.frame(
      person_id = rep(100001, 10),
      atc = c(rep("N05AH02", 10), rep("N05AH03", 10)),
      package_vnr = c(rep(12345, 10), rep(123456, 10)),
      purchase_date = as.Date("2022-01-01") + 30 * 0:19,
      ratio = rep(1, 20),
      total_ddd = c(rep(100, 8), NA, NA, rep(100, 10))
    )
    mock_readline <- mock("n")
    stub(check_purchases, "readline", mock_readline)

    expect_snapshot(
      check_purchases(
        test_data_missing_ddd,
        pre_person_id = "person_id",
        pre_atc = "atc",
        pre_package_id = "package_vnr",
        pre_date = "purchase_date",
        pre_ratio = "ratio",
        pre_ddd = "total_ddd",
        return_data = TRUE
      ), error = TRUE
    )
  }
)
test_that(
  "check_purchases returns an error when vnr is missing after proceeding with missing DDD",
  {
    test_data_missing_vnr <- data.frame(
      person_id = rep(100001, 10),
      atc = c(rep("N05AH02", 10), rep("N05AH03", 10)),
      package_vnr = c(rep(12345, 9), NA, rep(123456, 10)),
      purchase_date = as.Date("2022-01-01") + 30 * 0:19,
      ratio = rep(1, 20),
      total_ddd = c(rep(100, 8), NA, NA, rep(100, 10))
    )
    mock_readline <- mock("y")
    stub(check_purchases, "readline", mock_readline)

    expect_snapshot(
      check_purchases(
        test_data_missing_vnr,
        pre_person_id = "person_id",
        pre_atc = "atc",
        pre_package_id = "package_vnr",
        pre_date = "purchase_date",
        pre_ratio = "ratio",
        pre_ddd = "total_ddd",
        return_data = TRUE
      ), error = TRUE
    )
  }
)
