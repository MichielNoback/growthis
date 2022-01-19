#' Filter Varioscan data
#'
#' General-purpose function to filter Varioscan data in long format on different column values.
#' Assumes these columns exist:
#' \code{
#'   [1] "dilution"       "time"           "series"         "replicate"      "OD"
#'   [6] "duration"       "start_date"     "strain"         "extract"        "date_extracted"
#'   [11] "medium"
#' }
#' @param data the dataframe holding all data
#' @param extracts which extracts to include. Defaults to all extracts.
#' @param strains the bacterial strains to include. Defaults to all strains.
#' @param replicates the replicates to include. Defaults to all replicates.
#' @param experiment_dates the start date values to include
#' @return filtered data with same columns
#' @export
#'
#' @examples
#' #filter replicates
#' filter_data(all_data, replicates = c("1C", "2C"))
#'
#' # filter replicates and extract
#' filter_data(all_data, replicates = c("1C", "2C"), extracts = "Tulip stem")
#'
#' filter replicates and lower date
#' filter_data(all_data, replicates = c("1C", "2C"))
#'
filter_data <- function(data,
                        extracts = "all",
                        strains = "all",
                        replicates = "all",
                        experiment_dates = "all") {
    data <- data %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(strains[1] == "all") TRUE else strain %in% strains) %>%
        dplyr::filter(if(replicates[1] == "all") TRUE else replicate %in% replicates) %>%
        dplyr::filter(if(experiment_dates[1] == "all") TRUE else start_date %in% experiment_dates)
    return(data)
}

foo <- function() {
    print("foo")
}
