data_wide <- coli %>%
        dplyr::filter(replicate %in% c("1C", "2C", "3C", "Avg")) %>%
        tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
        dplyr::mutate(minimum = pmin(`1C`, `2C`, `3C`),
                      maximum = pmax(`1C`, `2C`, `3C`))




xlsx_file <- paste0(here::here(), "/data-raw/", "S_aureus_30jan2020.xlsx")
data <- read_varioscan(xlsx_file)
