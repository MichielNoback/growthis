#' Constructor for varioscan data object
#'
#' @param
varioscan <- function(growth_data, metadata) {
    if (! "tbl_df" %in% class(growth_data)) {
        stop("Growth_data should be a tibble")
    }

    value <- list(
        "growth_data" = growth_data,
        "metadata" = metadata
    )
}


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
