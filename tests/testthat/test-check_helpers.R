# Test check_arguments
test_that("check_arguments stops if one argument is missing", {
  required_columns <- c("x", "y")
  expect_snapshot(check_arguments(df, data_name = "df", x = "x", Y = "y",
                                  required_columns), error = TRUE)
})
test_that("check_arguments stops if several arguments are missing", {
  required_columns <- c("x", "y")
  expect_snapshot(check_arguments(df, data_name = "df", a = "x", Y = "y",
                                  required_columns), error = TRUE)
})
# Test err_message
test_that("err_message reports one missing argument correcly", {
  expect_snapshot(err_message("x", error = "not found", arg_or_col = "argument"))
})
test_that("err_message reports multiple missing arguments correctly", {
  expect_snapshot(err_message(c("x", "y"), error = "not found", arg_or_col = "argument"))
})
test_that("err_message reports one missing column correctly", {
  expect_snapshot(err_message(c("x"), error = "not found", arg_or_col = "column"))
})
test_that("err_message reports multiple missing columns correctly", {
  expect_snapshot(err_message(c("x", "y"), error = "not found", arg_or_col = "column"))
})
# Test check_date_range
test_that("check_date_range requires two dates", {
  expect_snapshot(check_date_range(c("21-12-2007")), error = TRUE)
})
test_that("check_date_range warns if dates are in wrong orgder", {
  expect_snapshot(check_date_range(c("21-12-2007", "10-11-2007")), error = TRUE)
})

# Test make_warning
test_that("make_warning returns all error rows for and lists one column", {
  expect_snapshot(make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11),
                               colvars = c("test_col"),
                               warning_message = "test warning message",
                               print_all= TRUE))
})
test_that("make_warning returns only 5 error rows for and lists one column", {
  expect_snapshot(make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11),
                               colvars = c("test_col"),
                               warning_message = "test warning message",
                               print_all= FALSE))
})
test_that("make_warning returns all error rows for and lists two columns", {
  expect_snapshot(make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11),
                               colvars = c("test_col", "test2"),
                               warning_message = "test warning message",
                               print_all= TRUE))
})
test_that("make_warning returns only 5 error rows for and lists two columns", {
  expect_snapshot(make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11),
                               colvars = c("test_col", "test2"),
                               warning_message = "test warning message",
                               print_all= FALSE))
})
test_that("make_warning returns NULL if not errors to report", {
  expect_equal(make_warning(errorws = NULL,
                            colvars = c("test_col"),
                            warning_message = "test warning message",
                            print_all= TRUE), NULL)
})

# Test date_to_integer
### Date_to_integer tests -----
test_that("date_to_integer works with different inputs", {
  expect_equal(date_to_integer(c("20220105")), as.integer(as.Date("2022-01-05")))  # YyyyMMdd-muoto
  expect_equal(date_to_integer(c("05/01/2022")), as.integer(as.Date("2022-01-05")))  # dd/MM/yyyy
  expect_equal(date_to_integer(c("2022-01-05")), as.integer(as.Date("2022-01-05")))  # yyyy-MM-dd
  expect_equal(date_to_integer(c("2022/01/05")), as.integer(as.Date("2022-01-05")))  # yyyy/MM/dd
  expect_equal(date_to_integer(c("05.01.2022")), as.integer(as.Date("2022-01-05")))  # dd.MM.yyyy
  expect_equal(date_to_integer(c("2022.01.05")), as.integer(as.Date("2022-01-05")))  # yyyy.MM.dd
  expect_equal(date_to_integer(c("05012022")), as.integer(as.Date("2022-01-05")))  # ddMMyyyy
})

test_that("date_to_integer returns NA with unvalid inputs", {
  expect_true(is.na(date_to_integer("")))
  expect_true(is.na(date_to_integer("  ")))
  expect_true(is.na(date_to_integer(NA)))
  expect_true(is.na(date_to_integer("2022&01&04")))
  expect_true(is.na(date_to_integer("randomstring")))
})

test_that("date_to_integer works with vectors", {
  dates <- c("2022-01-05", "05/01/2022", "", NA, "2022.01.05")
  expected <- as.integer(as.Date(c("2022-01-05", "2022-01-05", NA, NA, "2022-01-05")))
  expect_equal(sapply(dates, date_to_integer, USE.NAMES = FALSE), expected)
})

test_that("date_to_integer käsittelee ääriarvot oikein", {
  expect_equal(date_to_integer("1970-01-01"), as.integer(as.Date("1970-01-01")))
  expect_equal(date_to_integer("1900-01-01"), as.integer(as.Date("1900-01-01")))
  expect_equal(date_to_integer("2100-12-31"), as.integer(as.Date("2100-12-31")))
})

test_that("date_to_integer handles numeric input", {
  expect_equal(sapply(c(20240101, 18993), date_to_integer, USE.NAMES = FALSE), c(20240101, 18993))
})

test_that("date_to_integer handles valid date strings", {
  expect_equal(sapply(c("2024-01-01", "01/01/2024", "20240101", "2024/12/25"), date_to_integer, USE.NAMES = FALSE),
               as.integer(as.Date(c("2024-01-01", "2024-01-01", "2024-01-01", "2024-12-25"))))
})

test_that("date_to_integer handles Date objects", {
  expect_equal(sapply(as.Date(c("2024-01-01", "2024-02-01")), date_to_integer, USE.NAMES = FALSE),
               as.integer(as.Date(c("2024-01-01", "2024-02-01"))))
})

test_that("date_to_integer handles NAs", {
  expect_equal(sapply(as.Date(c(NA, "2024-02-01", "2024-04-10")), date_to_integer, USE.NAMES = FALSE),
               as.integer(as.Date(c(NA, "2024-02-01", "2024-04-10"))))
})

test_that("date_to_integer handles mixed valid and invalid inputs", {
  expect_equal(sapply(c("2024-01-01", "3344/014/20244", "2024.12.25", "not-a-date"), date_to_integer, USE.NAMES = FALSE),
               c(as.integer(as.Date("2024-01-01")),
                 NA_integer_,
                 as.integer(as.Date("2024-12-25")),
                 NA_integer_))
})

# Test check_non_numeric
non_num_values <- c("a", "1", "#", "", " ", "ABC123", NA, "2@")
test_that("check_non_numeric works", {
  expect_equal(check_non_numeric(non_num_values), c(3, 4, 5, 7, 8))
})
test_that("check_non_numeric works for factors", {
  expect_equal(check_non_numeric(as.factor(non_num_values)), c(3, 4, 5, 7, 8))
})
test_that("check_non_numeric works with empty", {
  expect_equal(check_non_numeric(character()), integer(0))
})
test_that("check_non_numeric works with numeric", {
  expect_equal(check_non_numeric(c(1, 2, 3)), integer(0))
})
test_that("check_non_numeric works with NA", {
  expect_equal(check_non_numeric(c(NA, NA, NA)), c(1, 2, 3))
})
test_that("check_non_numeric returns nothing with correct alphanumeric values", {
  expect_equal(check_non_numeric(c("a","abcdefg", "123abc", "3")), integer(0))
})

# Test find_multiple ATCs
test_that("find_multiple_atcs returns correct results with one package", {
  expect_snapshot(find_multiple_atcs(atc_col = c("N03AB10", "N03AB20", "N03AB30")
                                     , package_col = c(12345, 12345, 333455))
                  , error = TRUE)
})

test_that("find_multiple_atcs returns correct results with several packages", {
  expect_snapshot(find_multiple_atcs(
    atc_col = c("N03AB10", "N03AB20", "N03AB30", "N03AB40")
    , package_col = c(12345, 12345, 22345, 22345)), error = TRUE)
})
test_that("find_multiple_atcs returns NULL if not errors", {
  expect_equal(find_multiple_atcs(atc_col = c("N03AB10", "N03AB20"), package_col = c(12345, 22345)), NULL)
})


# Test check_numeric
values <- c(10, NA, -3, Inf, 5, 0)
test_that("check_numeric works with default settings", {
  expect_equal(check_numeric(values), c(2, 3, 4, 6))
})

test_that("check_numeric allows NA, but not zero", {
  expect_equal(check_numeric(values, allow_na = TRUE), c(3, 4, 6))
})

test_that("check_numeric allows zero, but not NA", {
  expect_equal(check_numeric(values, allow_zero = TRUE), c(2, 3, 4))
})

test_that("check_numeric allows both NA and zero", {
  expect_equal(check_numeric(values, allow_na = TRUE, allow_zero = TRUE), c(3, 4))
})

test_that("check_numeric works with empty", {
  expect_equal(check_numeric(numeric()), integer(0))
})

test_that("check_numeric works rigth with Inf", {
  expect_equal(check_numeric(c(Inf, -Inf, 5)), c(1, 2))
})

test_that("check_numeric does not list allowed values when NA and zero are allowed", {
  expect_equal(check_numeric(c(1, 2, 3), allow_na = TRUE, allow_zero = TRUE), integer(0))
})
test_that("check_numeric does not list allowed values with default settings", {
  expect_equal(check_numeric(c(1, 2, 3)), integer(0))
})

# Test check_coverage -----
test_that("check_coverage returns NULL if coverage reaches the limit in missing values", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 10), rep(200, 10))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "missing")
  expect_equal(coverage, NULL)
})
test_that("check_coverage returns the ATC without sufficient coverage", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 8), rep(NA, 2), rep(200, 10))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "missing")

  expect_equal(coverage, "N03AB10 (80.0%)")
})


test_that("check_coverage returns several ATCs without sufficient coverage", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 8), rep(NA, 2), rep(200, 7), rep(NA, 3))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "missing")

  expect_equal(coverage, "N03AB10 (80.0%), N03AB20 (70.0%)")
})

test_that("check_coverage returns several ATCs without sufficient coverage", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 6), rep(0, 4), rep(200, 7), rep(0, 3))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "zero")

  expect_equal(coverage, "N03AB10 (60.0%), N03AB20 (70.0%)")
})

test_that("check_coverage ignores zeros if option is to scearch missing values", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 6), rep(0, 4), rep(200, 7), rep(0, 3))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "missing")

  expect_equal(coverage, NULL)
})

test_that("check_coverage ignores NA if option is to scearch zero values", {
  atc_col <- c(rep("N03AB10", 10), rep("N03AB20", 10))
  ddd_col <- c(rep(100, 8), rep(NA, 2), rep(200, 7), rep(NA, 3))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "zero")
  expect_equal(coverage, NULL)
})

test_that("check_coverage approves with exacly 10% missing", {
  atc_col <- c(rep("N03AB10", 100))
  ddd_col <- c(rep(100, 90), rep(NA, 10))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 90,
                             opti = "missing")

  expect_equal(coverage, NULL)
})

test_that("check_coverage reports missing, if 49% of info is available, but 50% demanded", {
  atc_col <- c(rep("N03AB10", 100))
  ddd_col <- c(rep(100, 49), rep(NA, 51))
  coverage <- check_coverage(ATC = atc_col,
                             val = ddd_col,
                             limit = 50,
                             opti = "missing")

  expect_equal(coverage, "N03AB10 (49.0%)")
})


### check_order tests -----
test_that("check_order correctly identifies incorrect ordering", {

  # Case 1: No 'usual', valid order
  expect_equal(check_order(c(1, 2, 3), upper = c(3, 4, 5)), integer(0))

  # Case 2: No 'usual', incorrect order (one element out of place)
  expect_equal(check_order(c(1, 5, 3), upper = c(3, 4, 5)), c(2))

  # Case 3: With 'usual', valid order
  expect_equal(check_order(c(1, 2, 3), c(2, 3, 4), c(3, 4, 5)), integer(0))

  # Case 4: With 'usual', incorrect order (one element out of place)
  expect_equal(check_order(c(1, 4, 3), c(2, 3, 4), c(3, 4, 5)), c(2))

  # Case 5: Multiple incorrect positions
  expect_equal(check_order(c(5, 2, 3), c(1, 6, 2), c(3, 4, 5)), c(1, 2, 3))

  # Case 6: Edge case - single element
  expect_equal(check_order(c(1), c(2), c(3)), integer(0))
  expect_equal(check_order(c(3), c(2), c(1)), c(1))

  # Case 7: Edge case - empty vectors
  expect_equal(check_order(numeric(0), numeric(0), numeric(0)), integer(0))

  # Case 8: Error handling for length mismatch
  expect_snapshot(check_order(c(1, 2), c(3), c(4, 5)), error = TRUE)
})
# Function is internal having always the same input format
test_that("extract_atc_codes returns the ATC code from text formatted like 'C01BA01 (85.7%)'", {
  atc_code <- extract_atc_codes("C02BA01 (85.7%)")
  expect_equal(atc_code, "C02BA01")
})
