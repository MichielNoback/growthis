## code to prepare `DATASET` dataset goes here

xlsx_file <- paste0(here::here(), "/data-raw/K_pneumoniae_2maart2020.xls")
varioscan <- read_varioscan(xlsx_file)

usethis::use_data(varioscan, overwrite = TRUE)
