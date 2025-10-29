# check_purchases informs that test is passed

    Code
      check_purchases(test_data, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", print_all = TRUE)
    Message
      Checks passed for 'test_data'

# check_purchases notices missing arguments

    Code
      check_purchases(test_data, pre_atc = "atc", pre_package_id = "package_vnr",
        pre_date = "purchase_date", pre_ratio = "ratio", pre_ddd = "total_ddd")
    Condition
      Error:
      ! argument 'pre_person_id' is missing from function call. 

---

    Code
      check_purchases(test_data, pre_date = "purchase_date", pre_ratio = "ratio")
    Condition
      Error:
      ! arguments 'pre_person_id', 'pre_atc', 'pre_package_id', 'pre_ddd' are missing from function call. 

# check_purchases stops if required columns are missing

    Code
      check_purchases(test_data[, -2], pre_person_id = "person_id", pre_atc = "atc",
      pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
      pre_ddd = "total_ddd")
    Message
      column 'atc' is missing from the dataset. 
      
      
      
    Condition
      Error:
      ! Errors in drug purchases (pre_data). See listing above for details.

---

    Code
      check_purchases(test_data[, -c(2, 4)], pre_person_id = "person_id", pre_atc = "atc",
      pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
      pre_ddd = "total_ddd")
    Message
      columns 'atc', 'purchase_date' are missing from the dataset. 
      
      
      
    Condition
      Error:
      ! Errors in drug purchases (pre_data). See listing above for details.

# check_purchases gives warnings about invalid values

    Code
      check_purchases(test_data_errors, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd")
    Message
      Column 'person_id' has invalid or missing values in 4 rows: 4, 5, 6, 7
      Column 'atc' has invalid or missing values in 6 rows: 1, 2, 3, 4, 5 ...
      Column 'package_vnr' has invalid or missing values in 3 rows: 3, 5, 6
      Column 'ratio' has invalid or missing values in 5 rows: 3, 4, 5, 6, 7
      Column 'total_ddd' has invalid values in 1 rows: 3
      Column 'purchase_date' has invalid or missing values in 4 rows: 2, 3, 4, 5
      
      
    Condition
      Error:
      ! Errors in drug purchases (pre_data). See listing above for details.

# check_purchases warns about wrongly set date range

    Code
      check_purchases(test_data, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", date_range = c("2018-01-01", "2017-12-31"))
    Condition
      Error:
      ! starting date must be before ending date in date range.

# check_purchases warns about purchases outside set date range

    Code
      check_purchases(test_data, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", date_range = c("2022-01-05", "2022-06-20"))
    Message
      Column 'purchase_date' has values outside date range in 2 rows: 1, 7
      
      
    Condition
      Error:
      ! Errors in drug purchases (pre_data). See listing above for details.

# check_purchases stops if one package has several ATCs

    Code
      check_purchases(test_data2, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", date_range = c("2022-01-05", "2022-06-20"))
    Condition
      Error:
      ! Error in drug purchases data, expected only one ATC-code by package number, exceptions found: 12345: N05AH02, N05AH; 54321: N05AH, N05AH02.

# check_purchases informs ATCs without sufficient information and continues with the rest

    Code
      outdata <- check_purchases(test_data_missing_ddd, pre_person_id = "person_id",
        pre_atc = "atc", pre_package_id = "package_vnr", pre_date = "purchase_date",
        pre_ratio = "ratio", pre_ddd = "total_ddd", drop_atcs = TRUE, return_data = TRUE)
    Message
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N05AH02 (80.0%).
      Excluded ATC(s) with insufficient DDD information, the process continues with the rest of the data.
      Checks passed for 'test_data_missing_ddd'

# check_purchases informs ATCs without sufficient information and stops with drop_ATC:s = FALSE

    Code
      check_purchases(test_data_missing_ddd, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", return_data = TRUE, drop_atcs = FALSE)
    Message
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N05AH02 (80.0%).
    Condition
      Error:
      ! Process interrupted, dropping ATC codes with insufficient data not selected (drop_atcs = FALSE).

# check_purchases returns an error when vnr is missing after proceeding with missing DDD

    Code
      check_purchases(test_data_missing_vnr, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", return_data = TRUE, drop_atcs = TRUE)
    Message
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N05AH02 (80.0%).
      Excluded ATC(s) with insufficient DDD information, the process continues with the rest of the data.
      Column 'package_vnr' has invalid or missing values in 1 rows: 10
      
      
    Condition
      Error:
      ! Errors in drug purchases (pre_data). See listing above for details.

# check_purchases warns that no drug purchase records remained after dropping ATCs

    Code
      check_purchases(test_data_missing_ddd, pre_person_id = "person_id", pre_atc = "atc",
        pre_package_id = "package_vnr", pre_date = "purchase_date", pre_ratio = "ratio",
        pre_ddd = "total_ddd", return_data = TRUE, drop_atcs = TRUE)
    Message
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N05AH02 (50.0%), N05AH03 (50.0%).
    Condition
      Error:
      ! No drug purchase records remain after dropping ATC codes with insufficient DDD data.

