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

exclude_control <- list(
    start_date = "2-12-2021",
    series = "Exp1",
    dilution = 0.02,
    replicate = "C" #need to recalculate the corrected values for all three replicates
)




data <- coli_2021_12_02
exclude <- exclude_multiple
#exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))

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




(coli_excl <- filter_data(coli, exclude_wells = exclude_multiple))
print(coli_excl, n=30)


replicates <- c("1", "1C", "2", "2C", "3", "3C", "C", "Avg")
dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0)
combos <- expand.grid(dils = dilutions, repls = replicates)
(combos <- paste(combos$repls, combos$dils, sep = "_"))

coli_excl %>%
    tidyr::pivot_wider(names_from = c(replicate, dilution), values_from = OD)


#correct for removed (and thus missing) wells
(data_corrected <- coli_excl %>%
    tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Avg = if(any(is.na(c(`1C`, `2C`, `3C`)))) mean(c(`1C`, `2C`, `3C`), na.rm = T) else Avg) %>%
    tidyr::pivot_longer(cols = 9:16, names_to = "replicate", values_to = "OD") %>%
    dplyr::select(dilution, series, replicate, OD, duration, start_date, strain, extract, date_extracted, medium)
)




(exp_plot <- ggplot2::ggplot(data = data_wide,
                            mapping = ggplot2::aes(x = duration, y = Avg)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = minimum,
                                      ymax = maximum,
                                      fill = as.factor(dilution)),
                         alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(color = as.factor(dilution))) +
    ggplot2::scale_x_time() +
    ggplot2::scale_color_manual(values = get_series_palette()) +
    ggplot2::scale_fill_manual(values = get_series_palette()) +
    ggplot2::labs(fill = "dilution", color = "dilution") +
    #ggplot2::xlab("Duration (h)") +
    ggplot2::ylab("OD") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
    ) +
    ggplot2::facet_wrap(ggplot2::vars(start_date, series, extract)))


