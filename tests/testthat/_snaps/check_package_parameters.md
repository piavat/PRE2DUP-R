# check_package_parameters notices missing arguments

    Code
      check_package_parameters(dt = df_pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_dur_min = "minimum_duration",
        pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration",
        return_data = TRUE)
    Condition
      Error:
      ! argument 'pack_ddd_usual' is missing from function call. 

---

    Code
      check_package_parameters(dt = df_pack_params, pack_id = "vnr", pack_ddd_low = "lower_ddd",
        pack_dur_min = "minimum_duration", pack_dur_usual = "usual_duration",
        pack_dur_max = "maximum_duration", return_data = TRUE)
    Condition
      Error:
      ! arguments 'pack_atc', 'pack_ddd_usual' are missing from function call. 

# check_package_parameters notices the columns that are missing

    Code
      check_package_parameters(df_pack_params[, -1], pack_atc = "ATC", pack_id = "vnr",
      pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration")
    Condition
      Error:
      ! column 'ATC' is missing from the dataset. 

---

    Code
      check_package_parameters(df_pack_params[, -c(1, 5)], pack_atc = "ATC", pack_id = "vnr",
      pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_duration",
      pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration")
    Condition
      Error:
      ! columns 'ATC', 'minimum_duration' are missing from the dataset. 

# check_package_parameters warns if durations or DDD limits are in wrong order

    Code
      check_package_parameters(dt = df_pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd_error", pack_dur_min = "minimum_duration",
        pack_dur_usual = "usual_duration_error", pack_dur_max = "maximum_duration")
    Message
      Column 'vnr' has invalid or missing values in 1 rows: 2
      Columns 'minimum_duration', 'usual_duration_error', 'maximum_duration' has values are in wrong order in 2 rows: 4, 5
      Columns 'lower_ddd', 'usual_ddd_error' has values are in wrong order in 3 rows: 1, 3, 4
    Condition
      Error:
      ! Errors in dataset assigned to 'df_pack_params'. See listing above for details.

# check_package_parameters warns if duplicated package parameters are found

    Code
      check_package_parameters(dt = df_pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_duration",
        pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration")
    Message
      'vnr' has duplicated values
    Condition
      Error:
      ! Errors in dataset assigned to 'df_pack_params'. See listing above for details.

# check_package_parameters warns if there are no records in parameter file

    Code
      check_package_parameters(dt = df_pack_params_empty, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_duration",
        pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration")
    Condition
      Error:
      ! No data in the dataset 'df_pack_params_empty'.

# check_package_parameters informs that data has passed check

    Code
      check_package_parameters(dt = df_pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_duration",
        pack_dur_usual = "usual_duration", pack_dur_max = "maximum_duration")
    Message
      Checks passed for 'df_pack_params'

