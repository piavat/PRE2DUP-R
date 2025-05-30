# predup stops if negative global value provided

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
        global_hosp_max = 30, weight_past = 1, weight_current = 4, weight_next = 1,
        weight_first_last = 5, calculate_pack_dur_usual = F, days_covered = 5,
        post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Input non numeric or below 0 in argument 'global_ddd_high'

# predup stops if maximum duration for single purchase is longer than usual purchase

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
        global_hosp_max = 30, weight_past = 1, weight_current = 4, weight_next = 1,
        weight_first_last = 5, calculate_pack_dur_usual = F, days_covered = 5,
        post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Global maximum of single purchase (global_max_single) shorter than or equal to Global maximum (global_max).

# predup stops if global gap max is shorter than global maximum duration

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
        global_hosp_max = 30, weight_past = 1, weight_current = 4, weight_next = 1,
        weight_first_last = 5, calculate_pack_dur_usual = F, days_covered = 5,
        post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Maximum distance (global_gap_max) to be longer than or equal to global maximum (global_max).

# predup stops if calculate_pack_dur_usual in not TRUE or FALSE

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
        global_max = 300, global_max_single = 150, global_ddd_high = 10,
        global_hosp_max = 30, weight_past = 1, weight_current = 4, weight_next = 1,
        weight_first_last = 5, calculate_pack_dur_usual = "X", days_covered = 5,
        post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Argument 'calculate_pack_dur_usual' must be either TRUE or FALSE.

# predup stops if global maximum is not greater than global minimum value provided

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
        global_hosp_max = 30, weight_past = 1, weight_current = 4, weight_next = 1,
        weight_first_last = 5, calculate_pack_dur_usual = F, days_covered = 5,
        post_process_perc = 1)
    Message
      Step 1/6: Checking parameters and datasets...
    Condition
      Error:
      ! Expected Global maximum (global_max) longer than global minimum (global_min).

# pre2dup reports common durations of packs missing in parameters

    Code
      outdata <- pre2dup(pre_data = purchases_data2, pre_person_id = "id", pre_atc = "atc",
        pre_package_id = "vnr", pre_date = "dates", pre_ratio = "ratio", pre_ddd = "ddds",
        package_parameters = pack_params, pack_atc = "ATC", pack_id = "vnr",
        pack_ddd_low = "lower_ddd", pack_ddd_usual = "usual_ddd", pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur", pack_dur_max = "maximum_dur", atc_parameters = ATC_parameters,
        atc_class = "partial_atc", atc_ddd_low = "lower_ddd_atc", atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc", atc_dur_max = "maximum_dur_atc",
        calculate_pack_dur_usual = T)
    Message
      Step 1/6: Checking parameters and datasets...
      Checks passed for 'pre_data'
      Checks passed for 'package_parameters'
      Checks passed for 'atc_parameters'.
      Step 2/6: Calculating purchase durations...
      Step 3/6: Stockpiling assessment...
      Step 4/6: Calculating common package durations in data...
      Common duration was calculated for 1 package(s) not listed in package parameters; package ID (common duration in days): 11111 (40).
      Step 5/6: Preparing drug use periods...
      Step 6/6: Post-processing drug use periods...
      Current post processing percentage: 1
      Drug use periods calculated. 5 periods created for 5 persons.

