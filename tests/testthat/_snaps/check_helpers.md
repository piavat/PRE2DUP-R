# check_arguments stops if one argument is missing

    Code
      check_arguments(df, data_name = "df", x = "x", Y = "y", required_columns)
    Condition
      Error:
      ! argument 'y' is missing from function call. 

# check_arguments stops if several arguments are missing

    Code
      check_arguments(df, data_name = "df", a = "x", Y = "y", required_columns)
    Condition
      Error:
      ! arguments 'x', 'y' are missing from function call. 

# err_message reports one missing argument correcly

    Code
      err_message("x", error = "not found", arg_or_col = "argument")
    Output
      [1] "argument 'x' is not found \n"

# err_message reports multiple missing arguments correctly

    Code
      err_message(c("x", "y"), error = "not found", arg_or_col = "argument")
    Output
      [1] "arguments 'x', 'y' are not found \n"

# err_message reports one missing column correctly

    Code
      err_message(c("x"), error = "not found", arg_or_col = "column")
    Output
      [1] "column 'x' is not found \n"

# err_message reports multiple missing columns correctly

    Code
      err_message(c("x", "y"), error = "not found", arg_or_col = "column")
    Output
      [1] "columns 'x', 'y' are not found \n"

# check_date_range requires two dates

    Code
      check_date_range(c("21-12-2007"))
    Condition
      Error:
      ! two valid dates required for date range.

# check_date_range warns if dates are in wrong orgder

    Code
      check_date_range(c("21-12-2007", "10-11-2007"))
    Condition
      Error:
      ! starting date must be before ending date in date range.

# make_warning returns all error rows for and lists one column

    Code
      make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11), colvars = c("test_col"),
      warning_message = "test warning message", print_all = TRUE)
    Output
      [1] "Column 'test_col' test warning message in 7 rows: 1, 3, 5, 7, 8, 9, 11"

# make_warning returns only 5 error rows for and lists one column

    Code
      make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11), colvars = c("test_col"),
      warning_message = "test warning message", print_all = FALSE)
    Output
      [1] "Column 'test_col' test warning message in 7 rows: 1, 3, 5, 7, 8 ..."

# make_warning returns all error rows for and lists two columns

    Code
      make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11), colvars = c("test_col", "test2"),
      warning_message = "test warning message", print_all = TRUE)
    Output
      [1] "Columns 'test_col', 'test2' test warning message in 7 rows: 1, 3, 5, 7, 8, 9, 11"

# make_warning returns only 5 error rows for and lists two columns

    Code
      make_warning(errorws = c(1, 3, 5, 7, 8, 9, 11), colvars = c("test_col", "test2"),
      warning_message = "test warning message", print_all = FALSE)
    Output
      [1] "Columns 'test_col', 'test2' test warning message in 7 rows: 1, 3, 5, 7, 8 ..."

# find_multiple_atcs returns correct results with one package

    Code
      find_multiple_atcs(atc_col = c("N03AB10", "N03AB20", "N03AB30"), package_col = c(
        12345, 12345, 333455), location = "message in snapshot test")
    Condition
      Error:
      ! Error message in snapshot test, expected only one ATC-code by package number, exceptions found: 12345: N03AB10, N03AB20.

# find_multiple_atcs returns correct results with several packages

    Code
      find_multiple_atcs(atc_col = c("N03AB10", "N03AB20", "N03AB30", "N03AB40"),
      package_col = c(12345, 12345, 22345, 22345), location = "message in snapshot test")
    Condition
      Error:
      ! Error message in snapshot test, expected only one ATC-code by package number, exceptions found: 12345: N03AB10, N03AB20; 22345: N03AB30, N03AB40.

# check_order correctly identifies incorrect ordering

    Code
      check_order(c(1, 2), c(3), c(4, 5))
    Condition
      Error in `check_order()`:
      ! length(lower) == length(usual) & length(usual) == length(upper) is not TRUE

