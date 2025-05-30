

ATC_parameters <- readRDS("data-raw/ATC_parameters.rds")

usethis::use_data(ATC_parameters, overwrite = TRUE, compress = "xz")
