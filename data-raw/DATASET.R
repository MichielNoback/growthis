## code to prepare available datasets

files <- c("aureus_2020_01_30" = "S_aureus_30jan2020.xlsx",
           "pneumoniae_2020_03_02" = "K_pneumoniae_2maart2020.xls",
           "aureus_2021_11_12" = "S_aureus_stampersruw_12nov2021.xls",
           "coli_2021_12_02" = "E_coli_stampersruw_02dec2021.xlsx")

varioscan_data <- list()
for (experiment in names(files)) {
    #print(experiment)
    xlsx_file <- paste0(here::here(), "/data-raw/", files[experiment])
    data <- read_varioscan(xlsx_file)
    varioscan_data[[experiment]] <- data
}

varioscan_data <- do.call(rbind, varioscan_data)

usethis::use_data(varioscan_data, overwrite = TRUE)


#
# xlsx_file <- paste0(here::here(), "/data-raw/S_aureus_30jan2020.xlsx")
# aureus_2020_01_30 <- read_varioscan(xlsx_file)
# usethis::use_data(aureus_2020_01_30, overwrite = TRUE)
#
# xlsx_file <- paste0(here::here(), "/data-raw/K_pneumoniae_2maart2020.xls")
# pneumoniae_2020_03_02 <- read_varioscan(xlsx_file)
# usethis::use_data(pneumoniae_2020_03_02, overwrite = TRUE)
#
# xlsx_file <- paste0(here::here(), "/data-raw/S_aureus_stampersruw_12nov2021.xls")
# aureus_2021_11_12 <- read_varioscan(xlsx_file)
# usethis::use_data(aureus_2021_11_12, overwrite = TRUE)
#
# xlsx_file <- paste0(here::here(), "/data-raw/E_coli_stampersruw_02dec2021.xlsx")
# coli_2021_12_02 <- read_varioscan(xlsx_file)
# usethis::use_data(coli_2021_12_02, overwrite = TRUE)
