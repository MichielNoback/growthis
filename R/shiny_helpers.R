# not exported shiny helper functions


#' serves available experiment names of the dataset
#'
#' @param data the varioscan data tibble
#'
available_experiment_names <- function(data) {
    unique(data$experiment_name)
}

#' serves available extracts of the dataset for the given experiments
#'
#' @param data the varioscan data tibble
#' @param the experiments
#'
available_extracts <- function(data, experiment_names) {
    data %>% dplyr::filter(experiment_name %in% experiment_names) %>%
        dplyr::pull(extract) %>%
        unique()
}


#' serves available strains of the dataset
#'
#' @param data the varioscan data tibble
#'
available_strains <- function(data, experiment_names) {
    data %>% dplyr::filter(experiment_name %in% experiment_names) %>%
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
