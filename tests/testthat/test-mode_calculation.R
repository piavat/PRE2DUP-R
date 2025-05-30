
test_that("Most common duration is chosen in mode calculation", {

  # duration 30; 10 purchases & 10 packages
  # duration 60; 12 purchases & 12 packages <- chosen
  vnr <- rep(1111, 22)
  time_btw <- c(rep(30, 10), rep(60, 12))
  ratio <- rep(1, 22)
  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 60)
  expect_equal(result$n_purchases, 12)
})

test_that("Number of purchases is preferred instead number of packages in mode calculation", {

  # duration 30; 13 purchases & 13 packages <- chosen
  # duration 60; 12 purchases & 21 packages
  vnr <- rep(1111, 25)
  time_btw <- c(rep(30, 13), 600, rep(60, 11))
  ratio <- c(rep(1, 13), 10, rep(1, 11))

  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 30)
  expect_equal(result$n_purchases, 13)
})

test_that("mode_calculaton chooses shortest duration if multiple modes with equal distance to median and mean", {

  # duration 30; 10 purchases & 10 packages <- chosen
  # duration 60; 10 purchases & 10 packages
  vnr <- rep(1111, 20)
  time_btw <- c(rep(60, 10), rep(30, 10))
  ratio <- rep(1, 20)

  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 30)
})


test_that("Mode calculation handles multiple package types", {

  vnr <- c(rep(1111, 22), rep(2222, 22), rep(3333, 22), rep(4444, 22), rep(5555, 22))
  time_btw <- rep(c(rep(30, 10), rep(60, 12)), 5)
  ratio <- rep(rep(1, 22), 5)
  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$pack_id, c(1111, 2222, 3333, 4444, 5555))
  expect_equal(result$common_duration, c(60, 60, 60, 60, 60))
  expect_equal(result$n_purchases, c(12, 12, 12, 12, 12))
})

test_that("mode_calculaton chooses the closest to median if several modes", {

  # Duration 20: 5 purchases & 5 packages
  # duration 30; 10 purchases & 10 packages <- median, chosen
  # duration 60; 10 purchases & 10 packages
  vnr <- rep(1111, 25)
  time_btw <- c(rep(20, 5), rep(30, 10), rep(60, 10))
  ratio <- rep(1, 25)

  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 30)
  expect_equal(result$n_purchases, 10)
})

test_that("mode_calculaton chooses the closest to mean if several modes and same distance to median", {

  # duration 27: 1 purchase & 1 package <- median goes to 45, but does not affect much mean
  # duration 30; 10 purchases & 10 packages
  # duration 60; 10 purchases & 10 packages
  # Duration 70: 1 purchase & 1 package <- makes mean closer to 60
  vnr <- rep(1111, 22)
  time_btw <- c(27, rep(30, 10), rep(60, 10), 70)
  ratio <- rep(1, 22)

  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 60)
  expect_equal(result$n_purchases, 10)
})

test_that("mode_calculation combines close durations", {

  # duration 27: 1 purchase & 1 package <- not combined with 30
  # duration 28: 1 purchase & 1 package <- will be combined with 30
  # duration 29: 1 purchase & 1 package <- will be combined with 30
  # duration 30: 10 purchases & 10 packages
  # duration 31: 1 purchase & 1 package <- will be combined with 30
  # duration 32: 1 purchase & 1 package <- will be combined with 30
  # duration 33: 1 purchase & 1 package <- not combined with 30
  # duration 60; 10 purchases & 10 packages
  # Duration 70: 1 purchase & 1 package
  vnr <- rep(1111, 26)
  time_btw <- c(27,28, 29, rep(30, 10), 31, 32,rep(60, 10), 70)
  ratio <- rep(1, 26)

  result <- mode_calculation(pack_id = vnr, pre_ratio = ratio, dur = time_btw)

  expect_equal(result$common_duration, 30)
  expect_equal(result$n_purchases, 14)
})
test_that("mode_calculation performs iterations correctly", {

  test_data <- data.table(
    pack_id = rep(1, 7),
    pre_ratio = c(5, 3, 8, 2, 6, 4, 1),
    dur = c(10, 11, 12, 13, 14, 15, 16)  # Sequential durations with 1-day differences
  )
  expect_snapshot(
    result <- mode_calculation(test_data$pack_id,
                               test_data$pre_ratio,
                               test_data$dur,
                               report_iters = 1)
  )
  # This small dataset does not give results
  expect_true(nrow(result) == 0)
})
