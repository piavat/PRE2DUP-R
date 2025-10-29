# Tests for pre2dup
# Make package parameters
packpar <- data.table(
  vnr = 194091,
  ATC = "N06DX01",
  lower_ddd = 0.5,
  usual_ddd = 1,
  minimum_dur = 14,
  usual_dur = 28,
  maximum_dur = 56
)
# make atc parameters
atcpar <- data.table(
  partial_atc = "N06D",
  lower_ddd_atc = 0.2,
  usual_ddd_atc = 0.8,
  minimum_dur_atc = 30,
  maximum_dur_atc = 300
)
# Make hospitalization data
hospitalizations <- data.table(
  id = 100001,
  hosp_start = as.IDate("2005-04-30"),
  hosp_end = as.IDate("2005-05-28")
)
# Make purchases data
ID <- rep(100001, 7)
ATC <- rep("N06DX01", 7)
VNR <- rep(194091, 7)
purc_date <- as.IDate(
  c(
    "2004-06-05",
    "2004-07-03",
    "2004-08-21",
    "2004-10-24",
    "2004-12-01",
    "2005-04-09",
    "2005-06-04"
  )
)
ratio <- c(1, 1, 1, 1, 1, 1, 1)
ddd <- c(28, 28, 28, 28, 28, 28, 28)
tdata <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)

test_that("Pre2dup returns three exposure periods", {
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = tdata,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2004-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(nrow(dup), 3)
  expect_equal(dup$dup_start, as.Date(c(
    "2004-06-05", "2004-10-24", "2005-04-09"
  )))
  expect_equal(dup$dup_end, as.Date(c(
    "2004-10-16", "2004-12-29", "2005-07-02"
  )))
  expect_equal(dup$dup_days, c(133, 66, 84))
  expect_equal(dup$dup_hospital_days, c(0, 0, 27))
  expect_equal(dup$dup_n_purchases, c(3, 2, 2))
  expect_equal(dup$dup_last_purchase, as.Date(c(
    "2004-08-21", "2004-12-01", "2005-06-04"
  )))
  expect_equal(dup$dup_total_DDD, c(84, 56, 56))
  expect_equal(sum(dup$dup_total_DDD), sum(tdata$ddd))
})

test_that(
  "Pre2dup returns two exposure periods if package parameters maximum duration is extended",
  {
    packpar$maximum_dur <- 100
    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = tdata,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end",
        date_range = c("2004-01-01", "2005-12-31"),
        global_gap_max = 300,
        global_min = 5,
        global_max = 300,
        global_max_single = 150,
        global_ddd_high = 10,
        global_hosp_max = 30,
        days_covered = 5,
        weight_past = 1,
        weight_current = 4,
        weight_next = 1,
        weight_first_last = 5,
        data_to_return = "periods",
        drop_atcs = FALSE,
        post_process_perc = 1
      )
    ))

    expect_equal(nrow(dup), 2)
    expect_equal(dup$dup_start, as.Date(c("2004-06-05", "2005-04-09")))
    expect_equal(dup$dup_days, c(253.0, 84))
    expect_equal(dup$dup_hospital_days, c(0, 27))
    expect_equal(dup$dup_n_purchases, c(5, 2))
    expect_equal(dup$dup_last_purchase, as.Date(c("2004-12-01", "2005-06-04")))
    expect_equal(dup$dup_total_DDD, c(140, 56))
    expect_equal(sum(dup$dup_total_DDD), sum(tdata$ddd))
  }
)

test_that("Pre2dup runs smoothly without hospitalizations", {
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = tdata,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      # hosp_data = hospitalizations,
      # hosp_person_id = "id",
      # hosp_admission = "hosp_start",
      # hosp_discharge = "hosp_end",
      date_range = c("2004-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(nrow(dup), 3)
  expect_equal(dup$dup_start, as.Date(c(
    "2004-06-05", "2004-10-24", "2005-04-09"
  )))
  expect_equal(dup$dup_end, as.Date(c(
    "2004-10-16", "2004-12-29", "2005-07-02"
  )))
  expect_equal(dup$dup_days, c(133, 66, 84))
  expect_equal(dup$dup_hospital_days, c(0, 0, 0))
  expect_equal(dup$dup_n_purchases, c(3, 2, 2))
  expect_equal(dup$dup_last_purchase, as.Date(c(
    "2004-08-21", "2004-12-01", "2005-06-04"
  )))
  expect_equal(dup$dup_total_DDD, c(84, 56, 56))
  expect_equal(sum(dup$dup_total_DDD), sum(tdata$ddd))
})

test_that("Pre2dup handles single purchase", {
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = tdata[1],
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2004-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(nrow(dup), 1)
  expect_equal(dup$dup_start, as.Date(c("2004-06-05")))
  expect_equal(dup$dup_end, as.Date(c("2004-07-03")))
  expect_equal(dup$dup_days, packpar$usual_dur)
  expect_equal(dup$dup_hospital_days, c(0))
  expect_equal(dup$dup_n_purchases, c(1))
  expect_equal(dup$dup_last_purchase, as.Date(c("2004-06-05")))
  expect_equal(dup$dup_total_DDD, 28)
  expect_equal(sum(dup$dup_total_DDD), sum(tdata[1]$ddd))
})

test_that("Pre2dup calculates first period as duration*ratio and adds time in hopspital",
          {
            # DDD is missing first purchase, ERFL is calculated ratio*usual_duration
            # Add 4 days hospitalization to first exposure period
            # Purchase does not reach the second purchase, first exposure period will be 18 (0.5*28 + 5-1) days
            # Make hospitalization data
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = as.IDate("2004-06-10"),
              hosp_end = as.IDate("2004-06-15")
            )
            # Make purchases data
            ID <- rep(100001, 10)
            ATC <- rep("N06DX01", 10)
            VNR <- rep(194091, 10)
            purc_date <- (as.IDate("2004-06-05") + c(0:9) * 31)
            ratio <- c(0.5, rep(1, 9))
            ddd <- c(NA, rep(28, 9))
            tdata <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = tdata,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2004-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(nrow(dup), 2)
            expect_equal(dup$dup_start, as.Date(c("2004-06-05", "2004-07-06")))
            expect_equal(dup$dup_end, as.Date(c("2004-06-23", "2005-04-19")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(18, 288))
            expect_equal(dup$dup_hospital_days, c(4, 0))
            expect_equal(sum(dup$dup_n_purchases), nrow(tdata))
            expect_equal(dup$dup_last_purchase, as.Date(c("2004-06-05", "2005-03-11")), tolerance = 0.1)
            expect_equal(sum(dup$dup_total_DDD), sum(na.omit(tdata$ddd)))
          })

test_that("Pre2dup stops if negative global value provided", {
  expect_snapshot(
    pre2dup(
      pre_data = tdata,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2004-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = -10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    ),
    error = TRUE
  )
})

test_that("Pre2dup stops if maximum duration for single purchase is longer than usual purchase",
          {
            expect_snapshot(
              pre2dup(
                pre_data = tdata,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2004-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 100,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              ),
              error = TRUE
            )
          })

test_that("Pre2dup stops if global gap max is shorter than global maximum duration",
          {
            expect_snapshot(
              pre2dup(
                pre_data = tdata,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2004-01-01", "2005-12-31"),
                global_gap_max = 100,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              ),
              error = TRUE
            )
          })

one_purchase_data <- data.frame(
  ID = 100001,
  ATC = "N06DX01",
  VNR = 194091,
  purc_date = as.Date("2003-06-05"),
  ratio = 0.5,
  ddd = 14
)

test_that("Pre2dup adds 30 days when hospitalization exceeds global_hosp_max 30",
          {
            # Hospitalization exceeds global hosp max 30
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = as.IDate("2003-06-10"),
              hosp_end = as.IDate("2003-07-15")
            )

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = one_purchase_data,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-07-19")), tolerance = 0.1)
            expect_equal(dup$dup_days, 44)
            expect_equal(dup$dup_hospital_days, 34)
          })

test_that("Pre2dup adds 10 days when hospitalization exceeds global_hosp_max 10",
          {
            # Hospitalization exceeds global hosp max 10
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = as.IDate("2003-06-10"),
              hosp_end = as.IDate("2003-07-15")
            )

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = one_purchase_data,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 10,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-29")), tolerance = 0.1)
            expect_equal(dup$dup_days, 24)
            expect_equal(dup$dup_hospital_days, 19)
          })

test_that("Pre2dup combines hospitalizations and ignores ending date", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-08", "2003-06-10")),
                                 hosp_end = as.IDate(c("2003-06-11", "2003-06-15")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = one_purchase_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-06-25")), tolerance = 0.1)
  expect_equal(dup$dup_days, 20)
  expect_equal(dup$dup_hospital_days, 6)
})

test_that("Pre2dup handles partially overlapping hospitalizations", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-08", "2003-06-10")),
                                 hosp_end = as.IDate(c("2003-06-10", "2003-06-15")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = one_purchase_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-06-25")), tolerance = 0.1)
  expect_equal(dup$dup_days, 20)
  expect_equal(dup$dup_hospital_days, 6)
})

test_that("Pre2dup handles totally overlapping hospitalizations", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-10", "2003-06-11")),
                                 hosp_end = as.IDate(c("2003-06-14", "2003-06-15")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = one_purchase_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-06-23")), tolerance = 0.1)
  expect_equal(dup$dup_days, 18)
  expect_equal(dup$dup_hospital_days, 4)
})

test_that("Pre2dup handles partially overlapping hospitalizations at start of exposure",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-01", "2003-06-03")),
                                           hosp_end = as.IDate(c("2003-06-05", "2003-06-08")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = one_purchase_data,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-22")), tolerance = 0.1)
            expect_equal(dup$dup_days, 16)
            expect_equal(dup$dup_hospital_days, 2)
          })

test_that("Pre2dup handles partially overlapping hospitalizations at end of exposure",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-16", "2003-06-18")),
                                           hosp_end = as.IDate(c("2003-06-20", "2003-06-23")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = one_purchase_data,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-25")), tolerance = 0.1)
            expect_equal(dup$dup_days, 20)
            expect_equal(dup$dup_hospital_days, 6)
          })

test_that("Pre2dup handles hospitalizations longer than exposure", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-03")),
                                 hosp_end = as.IDate(c("2003-07-08")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = one_purchase_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-19")), tolerance = 0.1)
  expect_equal(dup$dup_days, 44)
  expect_equal(dup$dup_hospital_days, 32)
})

test_that("Pre2dup handles consecutive hospitalizations", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c(
                                   "2003-06-08", "2003-06-11", "2003-06-18"
                                 )),
                                 hosp_end = as.IDate(c(
                                   "2003-06-11", "2003-06-18", "2003-06-22"
                                 )))
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = one_purchase_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-02")), tolerance = 0.1)
  expect_equal(dup$dup_days, 27)
  expect_equal(dup$dup_hospital_days, 13)
})

two_separate_purchases <- data.frame(
  ID = c(100001, 100001),
  ATC = c("N06DX01", "N06DX01"),
  VNR = c(194091, 194091),
  purc_date = as.Date(c("2003-06-05", "2003-06-30")),
  ratio = c(0.5, 0.5),
  ddd = c(14, 14)
)

test_that("Pre2dup does not combine with 5 days flexibility with longer pause",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-16")),
                                           hosp_end = as.IDate(c("2003-06-22")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = two_separate_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-24", "2003-07-14")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(19, 14))
            expect_equal(dup$dup_hospital_days, c(5, 0))
          })

test_that("Pre2dup combines with 10 days flexibility", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-16")),
                                 hosp_end = as.IDate(c("2003-06-22")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 10,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-14")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(39))
  expect_equal(dup$dup_hospital_days, c(5))
})

test_that("Pre2dup combines with a long hospitalization", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-16")),
                                 hosp_end = as.IDate(c("2003-07-03")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-16")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(41))
  expect_equal(dup$dup_hospital_days, c(16))
})

test_that("Pre2dup does not combine if hospitalization started after first exposure",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-23")),
                                           hosp_end = as.IDate(c("2003-07-03")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = two_separate_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-19", "2003-07-16")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(14, 16))
            expect_equal(dup$dup_hospital_days, c(0, 2))
          })

test_that("Pre2dup does not combine if hospitalization was between exposures",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-21")),
                                           hosp_end = as.IDate(c("2003-06-26")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = two_separate_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-06-19", "2003-07-14")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(14, 14))
            expect_equal(dup$dup_hospital_days, c(0, 0))
          })

test_that("Pre2dup combines and calculates hospitalizations correctly", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-23", "2003-07-11")),
                                 hosp_end = as.IDate(c("2003-07-03", "2003-07-16")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-06-19", "2003-07-20")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(14, 20))
  expect_equal(dup$dup_hospital_days, c(0, 6))
})

test_that("Pre2dup calculates scattered hospitalizations (1)", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c(
                                   "2003-06-01", "2003-06-11", "2003-06-25", "2003-07-15"
                                 )),
                                 hosp_end = as.IDate(c(
                                   "2003-06-04", "2003-06-18", "2003-07-03", "2003-07-17"
                                 )))
  two_purchases <- data.frame(
    ID = c(100001, 100001),
    ATC = c("N06DX01", "N06DX01"),
    VNR = c(194091, 194091),
    purc_date = as.Date(c("2003-06-05", "2003-06-30")),
    ratio = c(0.5, 0.5),
    ddd = c(14, 14)
  )
  packpar <- data.table(
    vnr = 194091,
    ATC = "N06DX01",
    lower_ddd = 0.5,
    usual_ddd = 1,
    minimum_dur = 14,
    usual_dur = 28,
    maximum_dur = 56
  )
  # make atc parameters
  atcpar <- data.table(
    partial_atc = "N06D",
    lower_ddd_atc = 0.2,
    usual_ddd_atc = 0.8,
    minimum_dur_atc = 30,
    maximum_dur_atc = 300
  )

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-16")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(41))
  expect_equal(dup$dup_hospital_days, c(14))
})

test_that("Pre2dup calculates scattered hospitalizations (2)", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c(
                                   "2003-06-01", "2003-06-21", "2003-07-14"
                                 )),
                                 hosp_end = as.IDate(c(
                                   "2003-06-04", "2003-06-26", "2003-07-17"
                                 )))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-19", "2003-07-14")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(14, 14))
  expect_equal(dup$dup_hospital_days, c(0, 0))
})

overlapping_purchases <- data.frame(
  ID = c(100001, 100001),
  ATC = c("N06DX01", "N06DX01"),
  VNR = c(194091, 194091),
  purc_date = as.Date(c("2003-06-05", "2003-06-15")),
  ratio = c(0.5, 0.5),
  ddd = c(14, 14)
)

test_that("Pre2dup calculates scattered hospitalizations (3)", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c(
                                   "2003-06-01", "2003-06-21", "2003-07-14"
                                 )),
                                 hosp_end = as.IDate(c(
                                   "2003-06-04", "2003-06-26", "2003-07-17"
                                 )))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-07-19", "2003-07-14")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(14, 14))
  expect_equal(dup$dup_hospital_days, c(0, 0))
})

overlapping_purchases <- data.frame(
  ID = c(100001, 100001),
  ATC = c("N06DX01", "N06DX01"),
  VNR = c(194091, 194091),
  purc_date = as.Date(c("2003-06-05", "2003-06-15")),
  ratio = c(0.5, 0.5),
  ddd = c(14, 14)
)

test_that("Pre2dup handles overlapping purchases", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-10")),
                                 hosp_end = as.IDate(c("2003-06-28")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = overlapping_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date("2003-07-11"), tolerance = 0.1)
  expect_equal(dup$dup_days, 36)
  expect_equal(dup$dup_hospital_days, 17)
})

test_that("Pre2dup handles overlapping purchases, hospitalization on later exposure period",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-20")),
                                           hosp_end = as.IDate(c("2003-06-27")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = overlapping_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date("2003-07-05"), tolerance = 0.1)
            expect_equal(dup$dup_days, 30)
            expect_equal(dup$dup_hospital_days, 6)
          })

test_that("Pre2dup handles overlapping purchases, hospitalization on first exposure period",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-07")),
                                           hosp_end = as.IDate(c("2003-06-14")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = overlapping_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date("2003-06-29"), tolerance = 0.1)
            expect_equal(dup$dup_days, 24)
            expect_equal(dup$dup_hospital_days, 6)
          })

test_that(
  "Pre2dup handles overlapping purchases, hospitalization longer than both exposure periods",
  {
    # Make hospitalization data
    hospitalizations <- data.table(id = 100001,
                                   hosp_start = as.IDate(c("2003-06-11", "2003-06-25")),
                                   hosp_end = as.IDate(c("2003-06-18", "2003-07-03")))

    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = overlapping_purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end",
        date_range = c("2003-01-01", "2005-12-31"),
        global_gap_max = 300,
        global_min = 5,
        global_max = 300,
        global_max_single = 150,
        global_ddd_high = 10,
        global_hosp_max = 30,
        days_covered = 5,
        weight_past = 1,
        weight_current = 4,
        weight_next = 1,
        weight_first_last = 5,
        data_to_return = "periods",
        drop_atcs = FALSE,
        post_process_perc = 1
      )
    ))

    expect_equal(dup$dup_end, as.Date("2003-07-08"), tolerance = 0.1)
    expect_equal(dup$dup_days, 33)
    expect_equal(dup$dup_hospital_days, 13)
  }
)

test_that(
  "Pre2dup handles overlapping purchases, hospitalization on both exposure periods, global max 15",
  {
    # Make hospitalization data
    hospitalizations <- data.table(id = 100001,
                                   hosp_start = as.IDate(c("2003-06-04")),
                                   hosp_end = as.IDate(c("2003-07-03")))

    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = overlapping_purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end",
        date_range = c("2003-01-01", "2005-12-31"),
        global_gap_max = 300,
        global_min = 5,
        global_max = 300,
        global_max_single = 150,
        global_ddd_high = 10,
        global_hosp_max = 15,
        days_covered = 5,
        weight_past = 1,
        weight_current = 4,
        weight_next = 1,
        weight_first_last = 5,
        data_to_return = "periods",
        drop_atcs = FALSE,
        post_process_perc = 1
      )
    ))

    expect_equal(dup$dup_end, as.Date("2003-07-14"), tolerance = 0.1)
    expect_equal(dup$dup_days, 39)
    expect_equal(dup$dup_hospital_days, 27)
  }
)

test_that(
  "Pre2dup handles overlapping purchases, hospitalization on both exposure periods, global max 5",
  {
    # Make hospitalization data
    hospitalizations <- data.table(id = 100001,
                                   hosp_start = as.IDate(c("2003-06-04")),
                                   hosp_end = as.IDate(c("2003-07-03")))

    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = overlapping_purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end",
        date_range = c("2003-01-01", "2005-12-31"),
        global_gap_max = 300,
        global_min = 5,
        global_max = 300,
        global_max_single = 150,
        global_ddd_high = 10,
        global_hosp_max = 5,
        days_covered = 5,
        weight_past = 1,
        weight_current = 4,
        weight_next = 1,
        weight_first_last = 5,
        data_to_return = "periods",
        drop_atcs = FALSE,
        post_process_perc = 1
      )
    ))

    expect_equal(dup$dup_end, as.Date("2003-07-04"), tolerance = 0.1)
    expect_equal(dup$dup_days, 29)
    expect_equal(dup$dup_hospital_days, 27)
  }
)

test_that("Pre2dup combines separate purchases, global max 30", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-16")),
                                 hosp_end = as.IDate(c("2003-06-27")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date("2003-07-14"), tolerance = 0.1)
  expect_equal(dup$dup_days, 39)
  expect_equal(dup$dup_hospital_days, 10)
})

test_that("Pre2dup does not combine separate purchases, global max 2", {
  # Make hospitalization data
  hospitalizations <- data.table(id = 100001,
                                 hosp_start = as.IDate(c("2003-06-16")),
                                 hosp_end = as.IDate(c("2003-06-27")))

  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = two_separate_purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      hosp_data = hospitalizations,
      hosp_person_id = "id",
      hosp_admission = "hosp_start",
      hosp_discharge = "hosp_end",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 2,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-06-21", "2003-07-14")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(16, 14))
  expect_equal(dup$dup_hospital_days, c(5, 0))
})

test_that("Pre2dup combines separate purchases, global max 2 but days_covered 10",
          {
            # Make hospitalization data
            hospitalizations <- data.table(id = 100001,
                                           hosp_start = as.IDate(c("2003-06-16")),
                                           hosp_end = as.IDate(c("2003-06-28")))

            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = two_separate_purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 2,
                days_covered = 10,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date("2003-07-14"), tolerance = 0.1)
            expect_equal(dup$dup_days, 39)
            expect_equal(dup$dup_hospital_days, 11)
          })


test_that("Pre2dup stops if global maximum is not greater than global minimum value provided",
          {
            expect_snapshot(
              pre2dup(
                pre_data = one_purchase_data,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                # hosp_data = hospitalizations,
                # hosp_person_id = "id",
                # hosp_admission = "hosp_start",
                # hosp_discharge = "hosp_end",
                date_range = c("2004-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 3,
                global_max_single = 150,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 5,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              ),
              error = TRUE
            )
          })

# Post processing tests
# Data has two days between purchases
# post_process_perc = 1 -> gap less than or equal to 2 days -> combine
# post_process_perc = 0.5 -> gap more than 1 days -> do not combine

long_purchase_short_gap <- data.frame(
  ID = rep(1000001, 2),
  ATC = "A",
  VNR = rep(1111, 2),
  purc_date = as.Date(c("2003-06-05", "2003-12-24")),
  ddd = rep(200, 2),
  ratio = rep(1, 2)
)
pack_par <- data.frame(
  ATC = "A",
  vnr = 1111,
  lower_ddd = 1,
  usual_ddd = 1,
  minimum_dur = 100,
  usual_dur = 200,
  maximum_dur = 300
)

atc_par <- data.frame(
  partial_atc = "A",
  lower_ddd_atc = 1,
  usual_ddd_atc = 1,
  minimum_dur_atc = 100,
  maximum_dur_atc = 300
)

test_that("Pre2dup postprocessing combines purchases if post_processing_perc is 1",
          {
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = long_purchase_short_gap,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = pack_par,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atc_par,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 250,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 0,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 1
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2004-07-11")), tolerance = 0.1)
            expect_equal(dup$dup_days, 402)
          })

test_that("Pre2dup postprocessing does not combine purchases if post_processing_perc is 0.5",
          {
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = long_purchase_short_gap,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = pack_par,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atc_par,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                date_range = c("2003-01-01", "2005-12-31"),
                global_gap_max = 300,
                global_min = 5,
                global_max = 300,
                global_max_single = 250,
                global_ddd_high = 10,
                global_hosp_max = 30,
                days_covered = 0,
                weight_past = 1,
                weight_current = 4,
                weight_next = 1,
                weight_first_last = 5,
                data_to_return = "periods",
                drop_atcs = FALSE,
                post_process_perc = 0.5
              )
            ))

            expect_equal(dup$dup_end, as.Date(c("2003-12-22", "2004-07-11")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(200, 200))
          })

long_purchase_short_gap$ATC <- as.factor(long_purchase_short_gap$ATC)
test_that("Pre2dup handles factor ATC", {
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = long_purchase_short_gap,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = pack_par,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atc_par,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 250,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 0,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 0.5
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2003-12-22", "2004-07-11")), tolerance = 0.1)
  expect_equal(dup$dup_days, c(200, 200))
})

test_that("Pre2dup stockpiling connects purchases", {
  packparN06 <- data.table(
    vnr = 194091,
    ATC = "N06DX01",
    lower_ddd = 0.5,
    usual_ddd = 1,
    minimum_dur = 14,
    usual_dur = 28,
    maximum_dur = 56
  )
  atcparN06 <- data.table(
    partial_atc = "N06D",
    lower_ddd_atc = 0.2,
    usual_ddd_atc = 0.8,
    minimum_dur_atc = 30,
    maximum_dur_atc = 300
  )
  ID <- rep(100001, 7)
  ATC <- rep("N06DX01", 7)
  VNR <- rep(194091, 7)
  purc_date <- as.IDate("2004-06-05") + 50 * 1:7
  ratio <- rep(1, 7)
  ddd <- rep(28, 7)
  purc_date[4] <- purc_date[4] - 15 # Without stockpiling exposure period would end
  stocp_data <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = stocp_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packparN06,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcparN06,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      date_range = c("2003-01-01", "2005-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2005-07-16")), tolerance = 0.1)
  expect_equal(dup$dup_days, 356)
  expect_equal(dup$dup_hospital_days, 0)
})

test_that("Pre2dup needs only puchase data and parameter files to run", {
  packparN06 <- data.table(
    vnr = 194091,
    ATC = "N06DX01",
    lower_ddd = 0.5,
    usual_ddd = 1,
    minimum_dur = 14,
    usual_dur = 28,
    maximum_dur = 56
  )
  atcparN06 <- data.table(
    partial_atc = "N06D",
    lower_ddd_atc = 0.2,
    usual_ddd_atc = 0.8,
    minimum_dur_atc = 30,
    maximum_dur_atc = 300
  )
  ID <- rep(100001, 7)
  ATC <- rep("N06DX01", 7)
  VNR <- rep(194091, 7)
  purc_date <- as.IDate("2004-06-05") + 50 * 1:7
  ratio <- rep(1, 7)
  ddd <- rep(28, 7)
  stocp_data <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = stocp_data,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packparN06,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcparN06,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      data_to_return = "periods",
      drop_atcs = FALSE
    )
  ))

  expect_equal(dup$dup_end, as.Date(c("2005-07-16")), tolerance = 0.1)
  expect_equal(dup$dup_days, 356)
  expect_equal(dup$dup_hospital_days, 0)
})

test_that("Pre2dup returns parameter file with durations 40 and 120", {
  id <- sort(rep(1:5, each = 20))
  vnr <- rep(c(rep(30627, 10), rep(41738, 10)), 5)
  atc <- rep(c(rep("N05AH02", 10), rep("N05AH04", 10)), 5)
  d40 <- as.Date("2020-01-01")  + 40 * 1:10
  d120 <- as.Date("2022-01-01")  + 120 * 1:10
  dates <- rep(c(d40, d120), 5)
  ddds <- rep(c(rep(33, 10), rep(80, 10)), 5)
  ratio <- rep(1, 100)
  purchases_data <- data.frame(id, vnr, atc, dates, ddds, ratio)
  pack_params <- data.frame(
    vnr = c(30627, 41738),
    ATC = c("N05AH02", "N05AH04"),
    product_name = c("LEPONEX", "KETIPINOR"),
    strength = c(100, 300),
    package_size = c(100, 100),
    DDD_pack = c(33.33333, 75.00000),
    minimum_dur = c(25, 50),
    maximum_dur = c(100, 200),
    lower_ddd = c(0.3333, 0.3750),
    usual_dur = c(33.33, 100.00),
    usual_ddd = c(1.00, 0.75)
  )
  outdata <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = purchases_data,
      pre_person_id = "id",
      pre_atc = "atc",
      pre_package_id = "vnr",
      pre_date = "dates",
      pre_ratio = "ratio",
      pre_ddd = "ddds",
      package_parameters = pack_params,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = ATC_parameters,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      data_to_return = "both"
    )
  ))
  dup <- outdata$periods
  updated_params <- outdata$package_parameters

  expect_equal(dup$dup_days, rep(c(408, 1224), 5))
  expect_equal(updated_params$common_duration, c(40, 120))
})


test_that("Test that pre2dup calculates purchased DDD per ATC DDD", {
  ID <- rep(100001, 10)
  ATC <- rep("N06DX01", 10)
  VNR <- c(rep(194091, 9), 111111)
  purc_date <- as.IDate(
    c(
      "2004-06-05",
      "2004-07-03",
      "2004-08-21",
      "2004-10-24",
      "2004-12-01",
      "2005-04-09",
      "2005-06-04",
      "2005-06-30",
      "2005-08-01",
      "2006-09-01"
    )
  )
  ratio <- rep(1, 10)
  ddd <- c(rep(28, 9), 50)
  tdata_vnr_diff <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)
  periods <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = tdata_vnr_diff,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      date_range = c("2004-01-01", "2007-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(periods$dup_end[4], as.Date("2006-09-01") + 50 / 0.8)
})

test_that("Pre2dup uses ATC min if other options are missing", {
  ID <- rep(100001, 10)
  ATC <- rep("N06DX01", 10)
  VNR <- c(rep(194091, 9), 111111)
  purc_date <- as.IDate(
    c(
      "2004-06-05",
      "2004-07-03",
      "2004-08-21",
      "2004-10-24",
      "2004-12-01",
      "2005-04-09",
      "2005-06-04",
      "2005-06-30",
      "2005-08-01",
      "2006-09-01"
    )
  )
  ratio <- rep(1, 10)
  ddd <- c(rep(28, 9), NA)
  tdata_vnr_diff <- data.table(ID, ATC, VNR, purc_date, ratio, ddd)
  periods <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = tdata_vnr_diff,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      date_range = c("2004-01-01", "2007-12-31"),
      global_gap_max = 300,
      global_min = 5,
      global_max = 300,
      global_max_single = 150,
      global_ddd_high = 10,
      global_hosp_max = 30,
      days_covered = 5,
      weight_past = 1,
      weight_current = 4,
      weight_next = 1,
      weight_first_last = 5,
      data_to_return = "periods",
      drop_atcs = FALSE,
      post_process_perc = 1
    )
  ))

  expect_equal(periods$dup_end[4], as.Date("2006-09-01") + 30)
})

test_that("Pre2dup renames package params common duration", {
  id <- sort(rep(1:5, each = 20))
  vnr <- rep(c(rep(30627, 10), rep(41738, 10)), 5)
  atc <- rep(c(rep("N05AH02", 10), rep("N05AH04", 10)), 5)
  d40 <- as.Date("2020-01-01")  + 40 * 1:10
  d120 <- as.Date("2022-01-01")  + 120 * 1:10
  dates <- rep(c(d40, d120), 5)
  ddds <- rep(c(rep(33, 10), rep(80, 10)), 5)
  ratio <- rep(1, 100)
  purchases_data <- data.frame(id, vnr, atc, dates, ddds, ratio)
  pack_params <- data.frame(
    vnr = c(30627, 41738),
    ATC = c("N05AH02", "N05AH04"),
    product_name = c("LEPONEX", "KETIPINOR"),
    strength = c(100, 300),
    package_size = c(100, 100),
    DDD_pack = c(33.33333, 75.00000),
    minimum_dur = c(25, 50),
    maximum_dur = c(100, 200),
    lower_ddd = c(0.3333, 0.3750),
    usual_dur = c(33.33, 100.00),
    usual_ddd = c(1.00, 0.75),
    common_duration = c(40, 120)
  )
  updated_params <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = purchases_data,
      pre_person_id = "id",
      pre_atc = "atc",
      pre_package_id = "vnr",
      pre_date = "dates",
      pre_ratio = "ratio",
      pre_ddd = "ddds",
      package_parameters = pack_params,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = ATC_parameters,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      data_to_return = "parameters"
    )
  ))

  expect_true("common_duration.x" %in% names(updated_params))
})

test_that("Pre2dup reports common durations of packs missing in parameters",
          {
            id <- c(sort(rep(1:5, each = 20)))
            vnr <- rep(c(rep(11111, 2), rep(30627, 18)), 5)
            atc <- rep("N05AH02", 100)
            dates <- rep((as.Date("2020-01-01")  + 40 * 1:20), 5)
            ddds <- rep(40, 100)
            ratio <- rep(1, 100)
            purchases_data2 <- data.frame(id, vnr, atc, dates, ddds, ratio)
            pack_params <- data.frame(
              vnr = c(30627, 41738),
              ATC = c("N05AH02", "N05AH04"),
              product_name = c("LEPONEX", "KETIPINOR"),
              strength = c(100, 300),
              package_size = c(100, 100),
              DDD_pack = c(33.33333, 75.00000),
              minimum_dur = c(25, 50),
              maximum_dur = c(100, 200),
              lower_ddd = c(0.3333, 0.3750),
              usual_dur = c(33.33, 100.00),
              usual_ddd = c(1.00, 0.75)
            )
            expect_snapshot(
              pre2dup(
                pre_data = purchases_data2,
                pre_person_id = "id",
                pre_atc = "atc",
                pre_package_id = "vnr",
                pre_date = "dates",
                pre_ratio = "ratio",
                pre_ddd = "ddds",
                package_parameters = pack_params,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = ATC_parameters,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                data_to_return = "parameters"
              )
            )
          })

test_that("Pre2dup calculates hospitalizations correctly and uses periods as default",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c("2000-01-30", "2000-03-05", "2000-03-18"),
              hosp_end = c("2000-02-14", "2000-03-16", "2000-04-29")
            )
            # Make purchases data
            purchases <- data.table(
              ID = rep(100001, 1),
              purc_date = c("2000-01-04", "2000-02-11"),
              ATC = rep("N06D", 2),
              VNR = rep(111, 2),
              ratio = rep(1, 2),
              ddd = rep(28, 2)
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 1)
            expect_equal(dup$dup_start, as.Date("2000-01-04"))
            expect_equal(dup$dup_end, as.Date("2000-03-22"))
            expect_equal(dup$dup_days, 78)
            expect_equal(dup$dup_hospital_days, 28)
            expect_equal(dup$dup_n_purchases, 2)
          })

test_that("Pre2dup calculates hospitalizations correctly: multiple short hospitalizations",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c(
                "2000-01-01",
                "2000-01-21",
                "2000-02-02",
                "2000-02-21",
                "2000-03-10"
              ),
              hosp_end = c(
                "2000-01-08",
                "2000-01-24",
                "2000-02-10",
                "2000-03-03",
                "2000-03-30"
              )
            )
            purchases <- data.table(
              ID = rep(100001, 1),
              purc_date = c("2000-01-04", "2000-02-11"),
              ATC = rep("N06D", 2),
              VNR = rep(111, 2),
              ratio = rep(1, 2),
              ddd = rep(28, 2)
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 1)
            expect_equal(dup$dup_start, as.Date("2000-01-04"))
            expect_equal(dup$dup_end, as.Date("2000-03-20"))
            expect_equal(dup$dup_days, 76)
            expect_equal(dup$dup_hospital_days, 32)
            expect_equal(dup$dup_n_purchases, 2)
          })

test_that("Pre2dup calculates hospitalizations correctly: separate hospitalizations)",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c("2000-01-04", "2000-02-04"),
              hosp_end = c("2000-01-24", "2000-02-18")
            )
            purchases <- data.table(
              ID = 100001,
              purc_date = c("2000-01-04"),
              ATC = "N06D",
              VNR = 111,
              ratio = 2,
              ddd = 56
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 1)
            expect_equal(dup$dup_start, as.Date("2000-01-04"))
            expect_equal(dup$dup_end, as.Date("2000-03-30"))
            expect_equal(dup$dup_days, 86)
            expect_equal(dup$dup_hospital_days, 32)
            expect_equal(dup$dup_n_purchases, 1)
          })

test_that("Pre2dup calculates hospitalizations correctly: overlapping hospitalizations",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c("2000-01-04", "2000-01-20"),
              hosp_end = c("2000-01-24", "2000-01-30")
            )
            purchases <- data.table(
              ID = 100001,
              purc_date = c("2000-01-04"),
              ATC = "N06D",
              VNR = 111,
              ratio = 2,
              ddd = 56
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 1)
            expect_equal(dup$dup_start, as.Date("2000-01-04"))
            expect_equal(dup$dup_end, as.Date("2000-03-25"))
            expect_equal(dup$dup_days, 81)
            expect_equal(dup$dup_hospital_days, 25)
            expect_equal(dup$dup_n_purchases, 1)
          })

test_that("Pre2dup calculates hospitalizations correctly: consecutive hospitalizations",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c("2000-01-04", "2000-01-24", "2000-01-30"),
              hosp_end = c("2000-01-24", "2000-01-30", "2000-02-02")
            )
            purchases <- data.table(
              ID = 100001,
              purc_date = c("2000-01-04"),
              ATC = "N06D",
              VNR = 111,
              ratio = 2,
              ddd = 56
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 1)
            expect_equal(dup$dup_start, as.Date("2000-01-04"))
            expect_equal(dup$dup_end, as.Date("2000-03-28"))
            expect_equal(dup$dup_days, 84)
            expect_equal(dup$dup_hospital_days, 28)
            expect_equal(dup$dup_n_purchases, 1)
          })

test_that("Pre2dup calculates hospitalizations correctly, 5 hospitalizations",
          {
            hospitalizations <- data.table(
              id = 100001,
              hosp_start = c(
                "2000-01-01",
                "2000-01-21",
                "2000-02-02",
                "2000-02-21",
                "2000-04-10"
              ),
              hosp_end = c(
                "2000-01-08",
                "2000-01-24",
                "2000-02-10",
                "2000-03-03",
                "2000-04-15"
              )
            )
            purchases <- data.table(
              ID = rep(100001, 3),
              purc_date = c("2000-01-04", "2000-02-11", "2000-05-25"),
              ATC = rep("N06D", 3),
              VNR = rep(111, 3),
              ratio = rep(1, 3),
              ddd = rep(28, 3)
            )
            atcpar <- data.table(
              partial_atc = "N06D",
              lower_ddd_atc = 0.2,
              usual_ddd_atc = 0.8,
              minimum_dur_atc = 30,
              maximum_dur_atc = 300
            )
            packpar <- data.table(
              vnr = 111,
              ATC = "N06D",
              lower_ddd = 0.5,
              usual_ddd = 1,
              minimum_dur = 14,
              usual_dur = 28,
              maximum_dur = 56
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                hosp_data = hospitalizations,
                hosp_person_id = "id",
                hosp_admission = "hosp_start",
                hosp_discharge = "hosp_end"
              )
            ))

            expect_equal(nrow(dup), 2)
            expect_equal(dup$dup_start, as.Date(c("2000-01-04", "2000-05-25")), tolerance = 0.1)
            expect_equal(dup$dup_end, as.Date(c("2000-03-20", "2000-06-22")), tolerance = 0.1)
            expect_equal(dup$dup_days, c(76, 28))
            expect_equal(dup$dup_hospital_days, c(22, 0))
            expect_equal(dup$dup_n_purchases, c(2, 1))
          })

test_that(
  "Pre2dup calculates hospitalizations correctly: hospitalization extending exposure, but not connecting",
  {
    hospitalizations <- data.table(
      id = 100001,
      hosp_start = c("2000-01-26", "2000-02-09"),
      hosp_end = c("2000-02-08", "2000-02-14")
    )
    purchases <- data.table(
      ID = rep(100001, 2),
      purc_date = c("2000-01-04", "2000-02-20"),
      ATC = rep("N06D", 2),
      VNR = rep(111, 2),
      ratio = rep(1, 2),
      ddd = rep(28, 2)
    )
    atcpar <- data.table(
      partial_atc = "N06D",
      lower_ddd_atc = 0.2,
      usual_ddd_atc = 0.8,
      minimum_dur_atc = 30,
      maximum_dur_atc = 300
    )
    packpar <- data.table(
      vnr = 111,
      ATC = "N06D",
      lower_ddd = 0.5,
      usual_ddd = 1,
      minimum_dur = 14,
      usual_dur = 28,
      maximum_dur = 56
    )
    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end"
      )
    ))

    expect_equal(nrow(dup), 2)
    expect_equal(dup$dup_days, c(40, 28))
    expect_equal(dup$dup_hospital_days, c(16, 0))
  }
)

test_that(
  "Pre2dup calculates hospitalizations correctly: hospitalization extending exposure and connecting",
  {
    hospitalizations <- data.table(
      id = 100001,
      hosp_start = c("2000-01-26"),
      hosp_end = c("2000-02-08")
    )
    purchases <- data.table(
      ID = rep(100001, 2),
      purc_date = c("2000-01-04", "2000-02-10"),
      ATC = rep("N06D", 2),
      VNR = rep(111, 2),
      ratio = rep(1, 2),
      ddd = rep(28, 2)
    )
    atcpar <- data.table(
      partial_atc = "N06D",
      lower_ddd_atc = 0.2,
      usual_ddd_atc = 0.8,
      minimum_dur_atc = 30,
      maximum_dur_atc = 300
    )
    packpar <- data.table(
      vnr = 111,
      ATC = "N06D",
      lower_ddd = 0.5,
      usual_ddd = 1,
      minimum_dur = 14,
      usual_dur = 28,
      maximum_dur = 56
    )
    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end"
      )
    ))

    expect_equal(nrow(dup), 1)
    expect_equal(dup$dup_days, c(65))
    expect_equal(dup$dup_hospital_days, c(12))
  }
)

test_that(
  "Pre2dup calculates hospitalizations correctly: hospitalization after exposure ignored",
  {
    hospitalizations <- data.table(
      id = 100001,
      hosp_start = c("2000-02-12"),
      hosp_end = c("2000-02-24")
    )
    purchases <- data.table(
      ID = rep(100001, 1),
      purc_date = c("2000-01-04"),
      ATC = rep("N06D", 1),
      VNR = rep(111, 1),
      ratio = rep(1, 1),
      ddd = rep(28, 1)
    )
    atcpar <- data.table(
      partial_atc = "N06D",
      lower_ddd_atc = 0.2,
      usual_ddd_atc = 0.8,
      minimum_dur_atc = 30,
      maximum_dur_atc = 300
    )
    packpar <- data.table(
      vnr = 111,
      ATC = "N06D",
      lower_ddd = 0.5,
      usual_ddd = 1,
      minimum_dur = 14,
      usual_dur = 28,
      maximum_dur = 56
    )
    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end"
      )
    ))

    expect_equal(nrow(dup), 1)
    expect_equal(dup$dup_days, c(28))
    expect_equal(dup$dup_hospital_days, c(0))
  }
)

test_that(
  "Pre2dup calculates hospitalizations correctly: hospitalization between exposures ignored",
  {
    hospitalizations <- data.table(
      id = 100001,
      hosp_start = c("2000-02-12"),
      hosp_end = c("2000-02-24")
    )
    purchases <- data.table(
      ID = rep(100001, 2),
      purc_date = c("2000-01-04", "2000-03-01"),
      ATC = rep("N06D", 2),
      VNR = rep(111, 2),
      ratio = rep(1, 2),
      ddd = rep(28, 2)
    )
    atcpar <- data.table(
      partial_atc = "N06D",
      lower_ddd_atc = 0.2,
      usual_ddd_atc = 0.8,
      minimum_dur_atc = 30,
      maximum_dur_atc = 300
    )
    packpar <- data.table(
      vnr = 111,
      ATC = "N06D",
      lower_ddd = 0.5,
      usual_ddd = 1,
      minimum_dur = 14,
      usual_dur = 28,
      maximum_dur = 56
    )
    dup <- suppressWarnings(suppressMessages(
      pre2dup(
        pre_data = purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        hosp_data = hospitalizations,
        hosp_person_id = "id",
        hosp_admission = "hosp_start",
        hosp_discharge = "hosp_end"
      )
    ))

    expect_equal(nrow(dup), 2)
    expect_equal(dup$dup_days, c(28, 28))
    expect_equal(dup$dup_hospital_days, c(0, 0))
  }
)

test_that("Pre2dup warns if drop_atcs is not TRUE or FALSE", {
  purchases <- data.table(
    ID = rep(100001, 2),
    purc_date = c("2000-01-04", "2000-03-01"),
    ATC = rep("N06D", 2),
    VNR = rep(111, 2),
    ratio = rep(1, 2),
    ddd = rep(28, 2)
  )
  atcpar <- data.table(
    partial_atc = "N06D",
    lower_ddd_atc = 0.2,
    usual_ddd_atc = 0.8,
    minimum_dur_atc = 30,
    maximum_dur_atc = 300
  )
  packpar <- data.table(
    vnr = 111,
    ATC = "N06D",
    lower_ddd = 0.5,
    usual_ddd = 1,
    minimum_dur = 14,
    usual_dur = 28,
    maximum_dur = 56
  )
  expect_snapshot(
    pre2dup(
      pre_data = purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      drop_atcs = 0
    ),
    error = TRUE
  )
})

test_that(
  "Pre2dup stops if no data is left after ingoring ATCs without sufficient level of DDD records",
  {
    purchases <- data.table(
      ID = rep(100001, 2),
      purc_date = c("2000-01-04", "2000-03-01"),
      ATC = rep("N06D", 2),
      VNR = rep(111, 2),
      ratio = rep(1, 2),
      ddd = rep(NA, 2)
    )
    atcpar <- data.table(
      partial_atc = "N06D",
      lower_ddd_atc = 0.2,
      usual_ddd_atc = 0.8,
      minimum_dur_atc = 30,
      maximum_dur_atc = 300
    )
    packpar <- data.table(
      vnr = 111,
      ATC = "N06D",
      lower_ddd = 0.5,
      usual_ddd = 1,
      minimum_dur = 14,
      usual_dur = 28,
      maximum_dur = 56
    )
    expect_snapshot(
      pre2dup(
        pre_data = purchases,
        pre_person_id = "ID",
        pre_atc = "ATC",
        pre_package_id = "VNR",
        pre_date = "purc_date",
        pre_ratio = "ratio",
        pre_ddd = "ddd",
        package_parameters = packpar,
        pack_atc = "ATC",
        pack_id = "vnr",
        pack_ddd_low = "lower_ddd",
        pack_ddd_usual = "usual_ddd",
        pack_dur_min = "minimum_dur",
        pack_dur_usual = "usual_dur",
        pack_dur_max = "maximum_dur",
        atc_parameters = atcpar,
        atc_class = "partial_atc",
        atc_ddd_low = "lower_ddd_atc",
        atc_ddd_usual = "usual_ddd_atc",
        atc_dur_min = "minimum_dur_atc",
        atc_dur_max = "maximum_dur_atc",
        drop_atcs = TRUE
      ),
      error = TRUE
    )
  }
)

test_that("Pre2dup ignores the ATCs without sufficient level of DDD records",
          {
            purchases <- data.table(
              ID = rep(100001, 4),
              purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
              ATC = c(rep("N06D", 2), rep("N04A", 2)),
              VNR = c(rep(111, 2), rep(222, 2)),
              ratio = rep(1, 4),
              ddd = c(rep(NA, 2), rep(20, 2))
            )
            atcpar <- data.table(
              partial_atc = c("N06D", "N04A"),
              lower_ddd_atc = rep(0.2, 2),
              usual_ddd_atc = rep(0.8, 2),
              minimum_dur_atc = rep(30, 2),
              maximum_dur_atc = rep(300, 2)
            )
            packpar <- data.table(
              vnr = c(111, 222),
              ATC = c("N06D", "N04A"),
              lower_ddd = rep(0.5, 2),
              usual_ddd = rep(1, 2),
              minimum_dur = rep(14, 2),
              usual_dur = rep(28, 2),
              maximum_dur = rep(56, 2)
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                drop_atcs = TRUE
              )
            ))
            expect_equal(unique(dup$ATC), "N04A")
          })

test_that("Pre2dup stops if there are ATCs without sufficient level of DDD records",
          {
            purchases <- data.table(
              ID = rep(100001, 4),
              purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
              ATC = c(rep("N06D", 2), rep("N04A", 2)),
              VNR = c(rep(111, 2), rep(222, 2)),
              ratio = rep(1, 4),
              ddd = c(rep(NA, 2), rep(20, 2))
            )
            atcpar <- data.table(
              partial_atc = c("N06D", "N04A"),
              lower_ddd_atc = rep(0.2, 2),
              usual_ddd_atc = rep(0.8, 2),
              minimum_dur_atc = rep(30, 2),
              maximum_dur_atc = rep(300, 2)
            )
            packpar <- data.table(
              vnr = c(111, 222),
              ATC = c("N06D", "N04A"),
              lower_ddd = rep(0.5, 2),
              usual_ddd = rep(1, 2),
              minimum_dur = rep(14, 2),
              usual_dur = rep(28, 2),
              maximum_dur = rep(56, 2)
            )
            expect_snapshot(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                drop_atcs = FALSE
              ),
              error = TRUE
            )
          })

test_that("Pre2dup stops ATC parameters does not contain all ATCs in pruchases",
          {
            purchases <- data.table(
              ID = rep(100001, 4),
              purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
              ATC = c(rep("N06D", 2), rep("N04A", 2)),
              VNR = c(rep(111, 2), rep(222, 2)),
              ratio = rep(1, 4),
              ddd = c(rep(40, 2), rep(20, 2))
            )
            atcpar <- data.table(
              partial_atc = c("N05D", "N04A"),
              lower_ddd_atc = rep(0.2, 2),
              usual_ddd_atc = rep(0.8, 2),
              minimum_dur_atc = rep(30, 2),
              maximum_dur_atc = rep(300, 2)
            )
            packpar <- data.table(
              vnr = c(111, 222),
              ATC = c("N06D", "N04A"),
              lower_ddd = rep(0.5, 2),
              usual_ddd = rep(1, 2),
              minimum_dur = rep(14, 2),
              usual_dur = rep(28, 2),
              maximum_dur = rep(56, 2)
            )
            expect_snapshot(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                drop_atcs = FALSE
              ),
              error = TRUE
            )
          })

test_that("Pre2dup stops if insufficient level of package parameters", {
  purchases <- data.table(
    ID = rep(100001, 4),
    purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
    ATC = c(rep("N06D", 2), rep("N04A", 2)),
    VNR = c(rep(111, 2), rep(222, 2)),
    ratio = rep(1, 4),
    ddd = c(rep(40, 2), rep(20, 2))
  )
  atcpar <- data.table(
    partial_atc = c("N06D", "N04A"),
    lower_ddd_atc = rep(0.2, 2),
    usual_ddd_atc = rep(0.8, 2),
    minimum_dur_atc = rep(30, 2),
    maximum_dur_atc = rep(300, 2)
  )
  packpar <- data.table(
    vnr = c(111, 222),
    ATC = c("N06D", "N04A"),
    lower_ddd = rep(0.5, 2),
    usual_ddd = rep(1, 2),
    minimum_dur = rep(14, 2),
    usual_dur = rep(28, 2),
    maximum_dur = rep(56, 2)
  )
  expect_snapshot(
    pre2dup(
      pre_data = purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar[1, ],
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      drop_atcs = FALSE
    ),
    error = TRUE
  )
})

test_that("Pre2dup drops the ATC with insufficient level of package parameters",
          {
            purchases <- data.table(
              ID = rep(100001, 4),
              purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
              ATC = c(rep("N06D", 2), rep("N04A", 2)),
              VNR = c(rep(111, 2), rep(222, 2)),
              ratio = rep(1, 4),
              ddd = c(rep(40, 2), rep(20, 2))
            )
            atcpar <- data.table(
              partial_atc = c("N06D", "N04A"),
              lower_ddd_atc = rep(0.2, 2),
              usual_ddd_atc = rep(0.8, 2),
              minimum_dur_atc = rep(30, 2),
              maximum_dur_atc = rep(300, 2)
            )
            packpar <- data.table(
              vnr = c(111, 222),
              ATC = c("N06D", "N04A"),
              lower_ddd = rep(0.5, 2),
              usual_ddd = rep(1, 2),
              minimum_dur = rep(14, 2),
              usual_dur = rep(28, 2),
              maximum_dur = rep(56, 2)
            )
            dup <- suppressWarnings(suppressMessages(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar[1, ],
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                drop_atcs = TRUE
              )
            ))

            expect_equal(unique(dup$ATC), "N06D")
          })

test_that("Common durations cannot be calculated", {
  purchases <- data.table(
    ID = rep(100001, 4),
    purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
    ATC = c(rep("N06D", 2), rep("N04A", 2)),
    VNR = c(rep(111, 2), rep(222, 2)),
    ratio = rep(1, 4),
    ddd = c(rep(40, 2), rep(20, 2))
  )
  atcpar <- data.table(
    partial_atc = c("N06D", "N04A"),
    lower_ddd_atc = rep(0.2, 2),
    usual_ddd_atc = rep(0.8, 2),
    minimum_dur_atc = rep(30, 2),
    maximum_dur_atc = rep(300, 2)
  )
  packpar <- data.table(
    vnr = c(111, 222),
    ATC = c("N06D", "N04A"),
    lower_ddd = rep(0.5, 2),
    usual_ddd = rep(1, 2),
    minimum_dur = rep(14, 2),
    usual_dur = rep(28, 2),
    maximum_dur = rep(56, 2)
  )
  dup <- suppressWarnings(suppressMessages(
    pre2dup(
      pre_data = purchases,
      pre_person_id = "ID",
      pre_atc = "ATC",
      pre_package_id = "VNR",
      pre_date = "purc_date",
      pre_ratio = "ratio",
      pre_ddd = "ddd",
      package_parameters = packpar,
      pack_atc = "ATC",
      pack_id = "vnr",
      pack_ddd_low = "lower_ddd",
      pack_ddd_usual = "usual_ddd",
      pack_dur_min = "minimum_dur",
      pack_dur_usual = "usual_dur",
      pack_dur_max = "maximum_dur",
      atc_parameters = atcpar,
      atc_class = "partial_atc",
      atc_ddd_low = "lower_ddd_atc",
      atc_ddd_usual = "usual_ddd_atc",
      atc_dur_min = "minimum_dur_atc",
      atc_dur_max = "maximum_dur_atc",
      drop_atcs = TRUE,
      data_to_return = "both"
    )
  ))

  expect_equal(nrow(dup), 4)
})

test_that("Pre2dup stops all ATCs missed sufficient level of parameter info",
          {
            purchases <- data.table(
              ID = rep(100001, 4),
              purc_date = c("2000-01-04", "2000-03-01", "2000-06-14", "2000-08-21"),
              ATC = c(rep("N06D", 2), rep("N04A", 2)),
              VNR = c(rep(111, 2), rep(222, 2)),
              ratio = rep(1, 4),
              ddd = c(rep(40, 2), rep(20, 2))
            )
            atcpar <- data.table(
              partial_atc = c("N06D", "N04A"),
              lower_ddd_atc = rep(0.2, 2),
              usual_ddd_atc = rep(0.8, 2),
              minimum_dur_atc = rep(30, 2),
              maximum_dur_atc = rep(300, 2)
            )
            packpar <- data.table(
              vnr = c(1113, 2223),
              ATC = c("N06D", "N04A"),
              lower_ddd = rep(0.5, 2),
              usual_ddd = rep(1, 2),
              minimum_dur = rep(14, 2),
              usual_dur = rep(28, 2),
              maximum_dur = rep(56, 2)
            )
            expect_snapshot(
              pre2dup(
                pre_data = purchases,
                pre_person_id = "ID",
                pre_atc = "ATC",
                pre_package_id = "VNR",
                pre_date = "purc_date",
                pre_ratio = "ratio",
                pre_ddd = "ddd",
                package_parameters = packpar,
                pack_atc = "ATC",
                pack_id = "vnr",
                pack_ddd_low = "lower_ddd",
                pack_ddd_usual = "usual_ddd",
                pack_dur_min = "minimum_dur",
                pack_dur_usual = "usual_dur",
                pack_dur_max = "maximum_dur",
                atc_parameters = atcpar,
                atc_class = "partial_atc",
                atc_ddd_low = "lower_ddd_atc",
                atc_ddd_usual = "usual_ddd_atc",
                atc_dur_min = "minimum_dur_atc",
                atc_dur_max = "maximum_dur_atc",
                drop_atcs = TRUE
              ),
              error = TRUE
            )
          })
