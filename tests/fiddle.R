data_wide <- coli %>%
        dplyr::filter(replicate %in% c("1C", "2C", "3C", "Avg")) %>%
        tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
        dplyr::mutate(minimum = pmin(`1C`, `2C`, `3C`),
                      maximum = pmax(`1C`, `2C`, `3C`))




xlsx_file <- paste0(here::here(), "/data-raw/", "S_aureus_30jan2020.xlsx")
data <- read_varioscan(xlsx_file)


wide_format <- coli_2021_12_02 %>%
    dplyr::filter(replicate %in% c("1C", "2C", "3C")) %>%
    dplyr::mutate(ID = paste(series, dilution, replicate, sep = "_"),
                  time = as.numeric(duration)) %>%
    dplyr::select(time, ID, OD) %>%
    tidyr::pivot_wider(names_from = ID, values_from = OD)


exclude_single <- list(
    start_date = "2-12-2021",
    series = "Exp1",
    dilution = 0.02,
    replicate = "1" #will remove both 1 and 1C
)


exclude_multiple <- list(
    list(
        start_date = "2-12-2021",
        series = "Exp1",
        dilution = 0.02,
        replicate = "1" #will remove both 1 and 1C
    ),
    list(
        start_date = "2-12-2021",
        series = "Exp2",
        dilution = 0.01,
        replicate = "2" #will remove both 1 and 1C
    )
)



data <- coli_2021_12_02
exclude <- exclude_single
exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))

print(data %>% dplyr::filter(
    !(start_date == exclude$start_date &
          series == exclude$series &
          dilution == exclude$dilution &
          replicate %in% exclude$replicates)), n = 30)


#    (replicate %in% exclude$replicates))

data %>%
    dplyr::filter(!(start_date == exclude$start_date) #&&
                   #series == exclude$series)# &&
                  # dilution != exclude$diluton &&
                  # !(replicate %in% exclude$replicates)
    )


