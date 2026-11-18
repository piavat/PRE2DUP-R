# Periods must not depend on how person ids are labelled. Ids of different
# digit widths (1..10) sort differently as text and as numbers.
test_that("pre2dup gives the same periods for ids 1001..1010 and 1..10", {
  packpar <- data.table(vnr = 194091, ATC = "N06DX01", lower_ddd = 0.5, usual_ddd = 1,
                        minimum_dur = 14, usual_dur = 28, maximum_dur = 56)
  atcpar <- data.table(partial_atc = "N06D", lower_ddd_atc = 0.2, usual_ddd_atc = 0.8,
                       minimum_dur_atc = 30, maximum_dur_atc = 300)
  # Person 2 stockpiles; the other nine persons buy once.
  d <- data.table(person = c(1, 2, 2, 2, 2, 3:10), day = c(0, 0, 27, 93, 97, rep(0, 8)))
  d[, `:=`(ATC = "N06DX01", VNR = 194091, ratio = 1, ddd = 28,
           purc_date = as.IDate("2004-01-01") + day)]
  run <- function(ids) {
    x <- copy(d)[, ID := ids[person]]
    p <- suppressWarnings(suppressMessages(pre2dup(
      x, "ID", "ATC", "VNR", "purc_date", "ratio", "ddd",
      packpar, "ATC", "vnr", "lower_ddd", "usual_ddd", "minimum_dur", "usual_dur", "maximum_dur",
      atcpar, "partial_atc", "lower_ddd_atc", "usual_ddd_atc", "minimum_dur_atc", "maximum_dur_atc",
      date_range = c("2004-01-01", "2004-12-31"))))
    p <- as.data.table(p)
    p[, person := match(ID, ids)]
    setorder(p, person, dup_start)
    p[, .(person, dup_start, dup_end)]
  }
  equal_width <- run(1000 + 1:10)
  mixed_width <- run(1:10)
  expect_equal(nrow(equal_width), 10)
  expect_equal(mixed_width, equal_width)
})
