## code to prepare `package_parameters_example` dataset goes here
package_parameters_example <- readRDS("data-raw/package_parameters_example.rds")
usethis::use_data(package_parameters_example, overwrite = TRUE)
