growth_params_multi <- tidyr::unite(data = growth_params_multi,
                                    col = "date_series",
                                    date_started, series,
                                    sep = " ",
                                    remove = FALSE)
growth_params_multi


data_key <- "series"
current_key <- "Exp1"


growth_params_20220209 %>%
    filter( .data[[data_key]] == current_key)

growth_params_20220209 %>%
    filter( get(data_key) == current_key)
