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
#' @param session_names the session_name values to include
#' @param lower_date Lubridate Date object: the lower date (start_date of experiment) to include, and later. Defaults to a week ago from the current day.
#' @param upper Lubridate Date object: the upper date (inclusive) to include, and before. Defaults to the current day.
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
#' filter_data(all_data, replicates = c("1C", "2C"), lower_date = lubridate::dmy("2-3-2020"))
#'
filter_data <- function(data,
                        extracts = "all",
                        strains = "all",
                        replicates = "all",
                        experiment_names = "all",
                        lower_date = lubridate::dmy("1-1-2000"), #today() - 7,
                        upper_date = lubridate::today()) {
    data <- data %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(strains[1] == "all") TRUE else strain %in% strains) %>%
        dplyr::filter(if(replicates[1] == "all") TRUE else replicate %in% replicates) %>%
        dplyr::filter(if(experiment_names[1] == "all") TRUE else experiment_name %in% experiment_names) %>%
        dplyr::filter(start_date >= lower_date & start_date <= upper_date)

    return(data)
}

foo <- function() {
    print("foo")
}
