do.scale <- T
var <- "Sepal.Length"
irs <- dplyr::as_tibble(iris)
irs %>% dplyr::mutate({{var}} := if(do.scale) scale(.data[[var]]) else .data[[var]])



# Library
library(ggplot2)
library(plotly)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# new column: text for tooltip:
data <- data %>%
    mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))

# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) +
    geom_tile() +
    scale_fill_gradient2(low = scales::muted("blue"),
                         mid = "white",
                         high = scales::muted("red"))

ggplotly(p, tooltip="text")



variable <- "r"
ggplot2::ggplot(data = selection,
                mapping = aes_string(x = "series_repl",
                                     y = "dilution",
                                     fill = variable,
                                     text = "text")) +
    geom_tile() +
    scale_fill_gradient(low = scales::muted("blue"),
                        #mid = "white",
                        high = scales::muted("red"))




data_wide <- coli %>%
        dplyr::filter(replicate %in% c("1C", "2C", "3C", "Avg")) %>%
        tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
        dplyr::mutate(minimum = pmin(`1C`, `2C`, `3C`),
                      maximum = pmax(`1C`, `2C`, `3C`))




xlsx_file <- paste0(here::here(), "/data-raw/", "S_aureus_30jan2020.xlsx")
data <- read_varioscan(xlsx_file)

#get plate layout
current_dataset %>%
    dplyr::select(dilution, series, replicate, OD, duration) %>%
    dplyr::filter(replicate %in% c("1", "2", "3", "C")) %>%
    dplyr::group_by(dilution, series, replicate) %>%
    dplyr::summarize(excluded = any(is.na(OD))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(series_repl = paste0(series, "_", replicate)) %>%
    dplyr::select(dilution, series_repl, excluded) %>%
    dplyr::arrange(dplyr::desc(dilution)) %>%
    tidyr::pivot_wider(names_from = series_repl, values_from = excluded)
#print(n = 100)


#return selection of clicked cells
selection <- clicked_wells %>%
    tidyr::pivot_longer(cols = -1, names_to = "exp", values_to = "glyph") %>%
    dplyr::filter(grepl(pattern = "remove-sign", glyph)) %>%
    tidyr::separate(col = exp, into = c("series", "replicate")) %>%
    dplyr::select(-glyph)

list_sel <- apply(X = selection,
      MARGIN = 1,
      FUN = function(x) list(dilution = x["dilution"], series = x["series"], replicate = x["replicate"]))

list_sel


apply(X = tbbl_selection,
      MARGIN = 1,
      FUN = function(x) list(
          start_date = start_date_of_exp,
          dilution = as.numeric(x["dilution"]),
          series = x["series"],
          replicate = x["replicate"]))


#get cells that have (only) NAs
current_dataset %>%
    dplyr::filter(is.na(OD)) %>%
    dplyr::group_by(dilution, series, replicate) %>%
    dplyr::group_keys()



metadata <- list(
    experiment_name = "SMMK Growth Curve K pneumoniae 2maart2020",
    date_started = "2020-03-02",
    experiment_id = "Kpneu_20200302",
    dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0),
    series = tibble::tibble(
        exp_id = c("Exp1", "Exp2", "Exp3"),
        strain = c("K. pneumoniae", "K. pneumoniae", "K. pneumoniae"),
        extract = c("Red Naomi_unkown", "White rose_unkown", "Elution control"),
        date_extracted = c("00-00-0000", "00-00-0000", "00-00-0000"),
        medium = c("NB1x_pH7.4", "NB1x_pH7.4", "NB1x_pH7.4")
    )
)




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
        start_date = "2-3-2020",
        series = "Exp1",
        dilution = 0.02,
        replicate = "1" #will remove both 1 and 1C
    ),
    list(
        start_date = "2-3-2020",
        series = "Exp2",
        dilution = 0.01,
        replicate = "2" #will remove both 1 and 1C
    ),
    list(
        start_date = "2-3-2020",
        series = "Exp1",
        dilution = 0.02,
        replicate = "C" #need to recalculate the corrected values for all three replicates
    )
)

exclude_multiple_df <- as.data.frame(t(sapply(exclude_multiple, FUN = function(x) {c(start_date = x$start_date,
                                              dilution = x$dilution,
                                              series = x$series,
                                              replicate = x$replicate)})))

exclude_control <- list(
    start_date = "2-3-2020",
    series = "Exp1",
    dilution = 0.02,
    replicate = "C" #need to recalculate the corrected values for all three replicates
)



exclude_single <- list(
    start_date = "2-3-2020",
    series = "Exp1",
    dilution = 0.02,
    replicate = "1" #will remove both 1 and 1C
)




data <- coli_2021_12_02
exclude <- exclude_multiple
#exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))




start_date_of_exp <- "2-3-2020"
data <- load_selected_experiment("2-3-2020")
nrow(data)
exclude <- selected_wells[[1]]

# exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))
# data <- data %>%
#     dplyr::filter(
#         !(start_date == exclude$start_date)# &
#               series == exclude$series &
#               dilution == exclude$dilution &
#               replicate %in% exclude$replicates
# nrow(data)

# data are excluded so averages should be recalculated
tmp <- data %>%
    tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Avg = if(any(is.na(c(`1C`, `2C`, `3C`)))) mean(c(`1C`, `2C`, `3C`), na.rm = T) else Avg) %>%
    tidyr::pivot_longer(cols = 10:17, names_to = "replicate", values_to = "OD") %>%
    dplyr::select(dilution, series, replicate, OD, duration, start_date, strain, extract, extract_id, buffer_strength, pH_buffer)









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


