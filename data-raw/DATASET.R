## code to prepare available datasets

experiment_data <- read_experiment_data(paste0(here::here(), "/data-raw/experiments.xlsx"))
usethis::use_data(experiment_data, overwrite = TRUE)
print(experiment_data)
#writes each single excel file to a separate rdata file,
#with exp_name as file name
for (i in seq_len(nrow(experiment_data))) {
    #if(i < 10) next #no need to parse old files again

    file_name <- as.character(experiment_data[i, "file_name"])
    exp_name <- as.character(experiment_data[i, "experiment_date"])

    print(paste(file_name, exp_name, sep = ": "))

    xlsx_file <- paste0(here::here(), "/data-raw/", file_name)

    data <- read_varioscan(xlsx_file)

    package_data_file <- paste0(here::here(), "/data/", exp_name, ".rda")
    print(package_data_file)

    saveRDS(object = data, file = package_data_file)

    assign(exp_name, data)
    do.call(eval(parse(text="usethis::use_data")),
            list(as.name(exp_name), overwrite = TRUE))
    #print(data)
    #break
}
