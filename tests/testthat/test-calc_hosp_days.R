test_that("Hospitalization before purchase is excluded", {
  result <- calc_hosp_days(
    pre_id = 1,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-30")),
    next_start = NA,
    hosp_id = 1,
    hosp_in = as.integer(as.Date("2003-06-01")),
    hosp_out = as.integer(as.Date("2003-06-04"))
  )

  expect_equal(result$hosp_days_all, 0)
  expect_equal(result$hosp_days_exp, 0)
})

test_that("Hospitalizations last day at exposure start", {
  result <- calc_hosp_days(
    pre_id = 2,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 2,
    hosp_in = as.integer(as.Date("2003-06-01")),
    hosp_out = as.integer(as.Date("2003-06-06"))
  )

  expect_equal(result$hosp_days_all, 0)
  expect_equal(result$hosp_days_exp, 0)
})

test_that("Hospitalization starts before exposure start", {
  result <- calc_hosp_days(
    pre_id = 3,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 3,
    hosp_in = as.integer(as.Date("2003-06-01")),
    hosp_out = as.integer(as.Date("2003-06-07"))
  )

  expect_equal(result$hosp_days_all, 1)
  expect_equal(result$hosp_days_exp, 1)
})

test_that("Hospitalization overlaps with purchase window", {
  result <- calc_hosp_days(
    pre_id = 4,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-30")),
    next_start = NA,
    hosp_id = 4,
    hosp_in = as.integer(as.Date("2003-06-10")),
    hosp_out = as.integer(as.Date("2003-06-15"))
  )

  expect_equal(result$hosp_days_all, 4)
  expect_equal(result$hosp_days_exp, 4)
})

test_that("Multiple hospitalizations are summed", {
  result <- calc_hosp_days(
    pre_id = 5,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = c(5, 5),
    hosp_in = as.integer(as.Date(c("2003-06-05", "2003-06-10"))),
    hosp_out = as.integer(as.Date(c("2003-06-08", "2003-06-15")))
  )

  expect_equal(result$hosp_days_all, 6)
  expect_equal(result$hosp_days_exp, 6)
})

test_that("Hospitalization continues past exposure", {
  result <- calc_hosp_days(
    pre_id = 6,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 6,
    hosp_in = as.integer(as.Date("2003-06-16")),
    hosp_out = as.integer(as.Date("2003-06-22"))
  )

  expect_equal(result$hosp_days_all, 5)
  expect_equal(result$hosp_days_exp, 5)
})

test_that("Hospitalization starts at time of exposure end", {
  result <- calc_hosp_days(
    pre_id = 7,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 7,
    hosp_in = as.integer(as.Date("2003-06-19")),
    hosp_out = as.integer(as.Date("2003-06-24"))
  )

  expect_equal(result$hosp_days_all, 0)
  expect_equal(result$hosp_days_exp, 0)
})

test_that("Hospitalization starts after exposure end", {
  result <- calc_hosp_days(
    pre_id = 8,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 8,
    hosp_in = as.integer(as.Date("2003-06-20")),
    hosp_out = as.integer(as.Date("2003-06-25"))
  )

  expect_equal(result$hosp_days_all, 0)
  expect_equal(result$hosp_days_exp, 0)
})

test_that("Hospitalization is longer that exposure", {
  result <- calc_hosp_days(
    pre_id = 9,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date("2003-06-05")),
    exp_end = as.integer(as.Date("2003-06-19")),
    next_start = NA,
    hosp_id = 9,
    hosp_in = as.integer(as.Date("2003-06-03")),
    hosp_out = as.integer(as.Date("2003-06-21"))
  )

  expect_equal(result$hosp_days_all, 15)
  expect_equal(result$hosp_days_exp, 15)
})


test_that("No hospitalization results in 0 days", {
  result <- calc_hosp_days(
    pre_id = 10,
    pre_atc = "A",
    curr_purc_start = as.integer(as.Date(c("2003-06-05", "2003-06-30"))),
    exp_end = as.integer(as.Date(c("2003-06-19", "2003-07-14"))),
    next_start = as.integer(as.Date("2003-06-30"), NA),
    hosp_id = integer(),
    hosp_in = as.integer(as.Date(character())),
    hosp_out = as.integer(as.Date(character()))
  )

  expect_equal(result$hosp_days_all, c(0, 0))
  expect_equal(result$hosp_days_exp, c(0, 0))
})
