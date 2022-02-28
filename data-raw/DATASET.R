## code to prepare available datasets

experiment_data <- read_experiment_data(paste0(here::here(), "/data-raw/experiments.xlsx"))
usethis::use_data(experiment_data, overwrite = TRUE)

#writes each single excel file to a separate rdata file,
#with exp_name as file name
for (i in seq_len(nrow(experiment_data))) {
    file_name <- as.character(experiment_data[i, "file_name"])
    exp_name <- as.character(experiment_data[i, "experiment_date"])

    print(paste(file_name, exp_name, sep = ": "))

    xlsx_file <- paste0(here::here(), "/data-raw/", file_name)
    data <- read_varioscan(xlsx_file)
    assign(exp_name, data)
    do.call(eval(parse(text="usethis::use_data")),
            list(as.name(exp_name), overwrite = TRUE))
    #print(data)
}




# varioscan_data <- list()
# for (experiment in names(files)) {
#     #print(experiment)
#     xlsx_file <- paste0(here::here(), "/data-raw/", files[experiment])
#     data <- read_varioscan(xlsx_file)
#     varioscan_data[[experiment]] <- data
# }
#
# varioscan_data <- do.call(rbind, varioscan_data)
#
# usethis::use_data(varioscan_data, overwrite = TRUE)


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
