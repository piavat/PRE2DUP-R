## code to prepare `purchases_example` dataset
library(data.table)

make_dates <- function(start_date, interval_days, n) {
  as.Date(start_date) + cumsum(c(0, rep(interval_days, n - 1)))
}

# Drug A (vnr 30627, ATC N05AH02), duration approx 33 days, amount 33.33
a_dates_1 <- make_dates("2025-01-01", 33, 3)  # person 1
a_dates_2 <- make_dates("2025-01-15", 33, 3)  # person 2
a_dates_3 <- make_dates("2025-02-01", 33, 3)  # person 3
a_dates_4 <- make_dates("2025-01-10", 33, 3)  # person 4

# Drug B (vnr 41738, ATC N05AH04), duration approx 100 days, amount 100
b_dates_3 <- make_dates("2025-01-05", 100, 2)  # person 3
b_dates_4 <- make_dates("2025-01-20", 100, 2)  # person 4
b_dates_5 <- make_dates("2025-01-01", 100, 2)  # person 5

# Build the data
purchases_example <- rbindlist(list(
  data.table(id = 1, vnr = "30627", ATC = "N05AH02", purchase_date = a_dates_1, n_packages = 1, amount = 33.33),
  data.table(id = 2, vnr = "30627", ATC = "N05AH02", purchase_date = a_dates_2, n_packages = 1, amount = 33.33),
  data.table(id = 3, vnr = "30627", ATC = "N05AH02", purchase_date = a_dates_3, n_packages = 1, amount = 33.33),
  data.table(id = 3, vnr = "41738", ATC = "N05AH04", purchase_date = b_dates_3, n_packages = 1, amount = 100),
  data.table(id = 4, vnr = "30627", ATC = "N05AH02", purchase_date = a_dates_4, n_packages = 1, amount = 33.33),
  data.table(id = 4, vnr = "41738", ATC = "N05AH04", purchase_date = b_dates_4, n_packages = 1, amount = 100),
  data.table(id = 5, vnr = "41738", ATC = "N05AH04", purchase_date = b_dates_5, n_packages = 1, amount = 100)
))

# Order by id and date
setorder(purchases_example, id, purchase_date)


usethis::use_data(purchases_example, overwrite = TRUE)
