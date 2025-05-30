# check_atc_parameters notices missing arguments

    Code
      check_atc_parameters(dt = df_atc_params, atc_class = "ATC_class", atc_ddd_low = "lower_ddd",
        atc_ddd_usual = "usual_ddd", atc_dur_min = "minimum_duration", )
    Condition
      Error:
      ! argument 'atc_dur_max' is missing from function call. 

---

    Code
      check_atc_parameters(dt = df_atc_params, atc_class = "ATC_class", atc_ddd_low = "lower_ddd",
        atc_ddd_usual = "usual_ddd", )
    Condition
      Error:
      ! arguments 'atc_dur_min', 'atc_dur_max' are missing from function call. 

# check_atc_parameters notices the columns that are missing

    Code
      check_atc_parameters(dt = df_atc_params[, -2], atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd", atc_ddd_usual = "usual_ddd", atc_dur_min = "minimum_duration",
      atc_dur_max = "maximum_duration")
    Condition
      Error:
      ! column 'lower_ddd' is missing from the dataset. 

---

    Code
      check_atc_parameters(dt = df_atc_params[, -c(2, 3)], atc_class = "ATC_class",
      atc_ddd_low = "lower_ddd", atc_ddd_usual = "usual_ddd", atc_dur_min = "minimum_duration",
      atc_dur_max = "maximum_duration")
    Condition
      Error:
      ! columns 'lower_ddd', 'usual_ddd' are missing from the dataset. 

# check_atc_parameters warns if durations or DDD limits are in wrong order

    Code
      check_atc_parameters(dt = df_atc_params, atc_class = "ATC_class", atc_ddd_low = "lower_ddd",
        atc_ddd_usual = "usual_ddd_error", atc_dur_min = "minimum_duration_error",
        atc_dur_max = "maximum_duration")
    Message
      Columns 'minimum_duration_error', 'maximum_duration' has values are in wrong order in 3 rows: 1, 2, 5
      Columns 'lower_ddd', 'usual_ddd_error' has values are in wrong order in 3 rows: 1, 3, 4
    Condition
      Error:
      ! Errors in dataset assigned to 'df_atc_params'. See listing above for details.

# check_atc_parameters warns if duplicated ATC classes are found

    Code
      check_atc_parameters(dt = df_atc_params, atc_class = "ATC_class", atc_ddd_low = "lower_ddd",
        atc_ddd_usual = "usual_ddd", atc_dur_min = "minimum_duration", atc_dur_max = "maximum_duration")
    Message
      'ATC_class' has duplicated values
    Condition
      Error:
      ! Errors in dataset assigned to 'df_atc_params'. See listing above for details.

# check_atc_parameters gives error if ATC parameters don't have any data

    Code
      check_atc_parameters(dt = atc_params_empty, atc_class = "ATC_class",
        atc_ddd_low = "lower_ddd", atc_ddd_usual = "usual_ddd", atc_dur_min = "minimum_duration",
        atc_dur_max = "maximum_duration")
    Condition
      Error:
      ! No data in the dataset 'atc_params_empty'.

# check_atc_parameters reports that check was passed

    Code
      check_atc_parameters(dt = ATC_parameters, atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc", atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc")
    Message
      Checks passed for 'ATC_parameters'.

