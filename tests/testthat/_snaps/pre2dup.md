# Pre2dup stops if negative global value provided

    Code
      pre2dup(pre_data = tdata, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", hosp_data = hospitalizations,
        hosp_person_id = "id", hosp_admission = "hosp_start", hosp_discharge = "hosp_end",
        date_range = c("2004-01-01", "2005-12-31"), global_gap_max = 300, global_min = 5,
        global_max = 300, global_max_single = 150, global_ddd_high = -10,
        global_hosp_max = 30, days_covered = 5, weight_past = 1, weight_current = 4,
        weight_next = 1, weight_first_last = 5, data_to_return = "periods",
        drop_atcs = FALSE, post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Input non numeric or below 0 in argument 'global_ddd_high'

# Pre2dup stops if maximum duration for single purchase is longer than usual purchase

    Code
      pre2dup(pre_data = tdata, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", hosp_data = hospitalizations,
        hosp_person_id = "id", hosp_admission = "hosp_start", hosp_discharge = "hosp_end",
        date_range = c("2004-01-01", "2005-12-31"), global_gap_max = 300, global_min = 5,
        global_max = 100, global_max_single = 150, global_ddd_high = 10,
        global_hosp_max = 30, days_covered = 5, weight_past = 1, weight_current = 4,
        weight_next = 1, weight_first_last = 5, data_to_return = "periods",
        drop_atcs = FALSE, post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Global maximum of single purchase (global_max_single) shorter than or equal to Global maximum (global_max).

# Pre2dup stops if global gap max is shorter than global maximum duration

    Code
      pre2dup(pre_data = tdata, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", hosp_data = hospitalizations,
        hosp_person_id = "id", hosp_admission = "hosp_start", hosp_discharge = "hosp_end",
        date_range = c("2004-01-01", "2005-12-31"), global_gap_max = 100, global_min = 5,
        global_max = 300, global_max_single = 150, global_ddd_high = 10,
        global_hosp_max = 30, days_covered = 5, weight_past = 1, weight_current = 4,
        weight_next = 1, weight_first_last = 5, data_to_return = "periods",
        drop_atcs = FALSE, post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Maximum distance (global_gap_max) to be longer than or equal to global maximum (global_max).

# Pre2dup stops if global maximum is not greater than global minimum value provided

    Code
      pre2dup(pre_data = one_purchase_data, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", date_range = c(
          "2004-01-01", "2005-12-31"), global_gap_max = 300, global_min = 5,
        global_max = 3, global_max_single = 150, global_ddd_high = 10,
        global_hosp_max = 30, days_covered = 5, weight_past = 1, weight_current = 4,
        weight_next = 1, weight_first_last = 5, data_to_return = "periods",
        drop_atcs = FALSE, post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Global maximum (global_max) longer than global minimum (global_min).

# Pre2dup reports common durations of packs missing in parameters

    Code
      pre2dup(pre_data = purchases_data2, pre_person_id = "id", pre_atc = "atc",
        pre_package_id = "vnr", pre_date = "dates", pre_ratio = "ratio", pre_ddd = "ddds",
        package_parameters = pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = ATC_parameters,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc",
        data_to_return = "parameters")
    Message
      Step 1/6: Checking parameters and datasets...
      Checks passed for 'pre_data'
      Checks passed for 'package_parameters'
      Checks passed for 'atc_parameters'.
      Step 2/6: Calculating purchase durations...
      Step 3/6: Stockpiling assessment...
      Step 4/6: Calculating common package durations in data...
      Common duration was calculated for 1 package(s) not listed in package parameters; package ID (common duration in days): 11111 (40).
      Common package durations calculated, returning updated package parameters.
    Output
          vnr     ATC product_name strength package_size DDD_pack minimum_dur
      1 30627 N05AH02      LEPONEX      100          100 33.33333          25
      2 41738 N05AH04    KETIPINOR      300          100 75.00000          50
        maximum_dur lower_ddd usual_dur usual_ddd common_duration
      1         100    0.3333     33.33      1.00              40
      2         200    0.3750    100.00      0.75              NA

# Pre2dup warns if drop_atcs is not TRUE or FALSE

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = 0)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Argument 'drop_atcs' must be either TRUE or FALSE.

# Pre2dup stops if no data is left after ingoring ATCs without sufficient level of DDD records

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = TRUE)
    Message
      Step 1/6: Checking parameters and datasets...
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N06D (0.0%).
    Condition
      Error:
      ! No drug purchase records remain after dropping ATC codes with insufficient DDD data.

# Pre2dup stops if there are ATCs without sufficient level of DDD records

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = FALSE)
    Message
      Step 1/6: Checking parameters and datasets...
      Coverage of DDD records in drug purchases is less than required (at least 90%) in following ATC(s): N06D (0.0%).
    Condition
      Error:
      ! Process interrupted, dropping ATC codes with insufficient data not selected (drop_atcs = FALSE).

# Pre2dup stops ATC parameters does not contain all ATCs in pruchases

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = FALSE)
    Message
      Step 1/6: Checking parameters and datasets...
      Checks passed for 'pre_data'
      Checks passed for 'package_parameters'
      Checks passed for 'atc_parameters'.
    Condition
      Error:
      ! Every ATC class in drug purhcases should exist at least with one character level in ATC parameters. Please check the ATC parameters and try again.

# Pre2dup stops if insufficient level of package parameters

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar[1, ], pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = FALSE)
    Message
      Step 1/6: Checking parameters and datasets...
      Checks passed for 'pre_data'
      Checks passed for 'package_parameters'
      Checks passed for 'atc_parameters'.
      Coverage of package parameter information in less than required (at least 90%) in following ATC(s): N04A (0.0%).
    Condition
      Error:
      ! Process interrupted, dropping ATC codes with insufficient data not selected (drop_atcs = FALSE).

# Pre2dup stops all ATCs missed sufficient level of parameter info

    Code
      pre2dup(pre_data = purchases, pre_person_id = "ID", pre_atc = "ATC",
        pre_package_id = "VNR", pre_date = "purc_date", pre_ratio = "ratio", pre_ddd = "ddd",
        package_parameters = packpar, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = atcpar,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc", drop_atcs = TRUE)
    Message
      Step 1/6: Checking parameters and datasets...
      Checks passed for 'pre_data'
      Checks passed for 'package_parameters'
      Checks passed for 'atc_parameters'.
      Coverage of package parameter information in less than required (at least 90%) in following ATC(s): N04A (0.0%), N06D (0.0%).
      Excluded ATC(s) with insufficient package parameter information, the process continues with the rest of the data.
    Condition
      Error:
      ! No records left after deleting ATCs without sufficient package parameter information.

