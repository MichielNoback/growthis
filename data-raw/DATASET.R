## code to prepare `DATASET` dataset goes here

files <- c("aureus-2020-01-30" = "S_aureus_30jan2020.xlsx",
           "pneumoniae-2020-03-02" = "K_pneumoniae_2maart2020.xls",
           "aureus-2021-11-12" = "S_aureus_stampersruw_12nov2021.xls",
           "coli=2021-12-02" = "E_coli_stampersruw_02dec2021.xlsx")

varioscan <- list()
for (experiment in names(files)) {
    xlsx_file <- paste0(here::here(), "/data-raw/", files[experiment])
    data <- read_varioscan(xlsx_file)
    varioscan[[experiment]] <- data
}

varioscan <- do.call(rbind, varioscan)

usethis::use_data(varioscan, overwrite = TRUE)

