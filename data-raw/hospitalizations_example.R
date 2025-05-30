## code to prepare `hospitalizations_example` dataset goes here

hospitalizations_example <- data.frame(
  id = c(2, 5),
  hospital_start = as.Date(c("2025-02-15", "2025-01-20")),
  hospital_end   = as.Date(c("2025-02-21", "2025-02-28"))
)

usethis::use_data(hospitalizations_example, overwrite = TRUE)
