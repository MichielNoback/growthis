# not exported shiny helper functions

#' prepares the growth parameters dataframe, as
prepare_stats_data_download <- function(growth_params) {

    growth_params %>% tidyr::separate()
}


#' serves available experiment start dates of the dataset
#'
#' @param data the varioscan data tibble
#'
available_experiment_dates <- function(data) {
    unique(data$experiment_date)
}

#' serves available extracts of the dataset for the given experiments
#'
#' @param data the varioscan data tibble
#' @param the experiments
#'
available_extracts <- function(data, experiment_dates) {
    data %>% dplyr::filter(start_date %in% experiment_dates) %>%
        dplyr::pull(extract) %>%
        unique()
}


#' serves available strains of the dataset
#'
#' @param data the varioscan data tibble
#'
available_strains <- function(data, experiment_dates) {
    data %>% dplyr::filter(start_date %in% experiment_dates) %>%
    dplyr::pull(strain) %>%
    unique()
}


# get_faceting_vars <- function() {
# #    c("dilution", "series", "replicate", "start_date", "experiment_name", "strain", "extract", "date_extracted", "medium")
#     c("series", "experiment_name", "strain", "extract", "date_extracted", "medium")
# }

#' helper for creating messages
#' @param message_text the prepended massege text
#' @param vector the vector of elements to message, with separating symbols
message_helper <- function(message_text, vector) {
    message(paste0(message_text, ": ", paste(vector, collapse = " -+- ")))
}

#' serves bar
#'
#' @export
bar <- function() {
    print("bar")
}
