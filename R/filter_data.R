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
#' @param exclude a list indicating which series/dilution/replicate should be excluded. Defaults to "none".
#' See details.
#' @return filtered data with same columns
#'
#' @details
#' \code{exclude} The `exclude` parameter can be used to specifically exclude a
#' single failed well (i.e. a single growth curve).
#' If multiple should be excluded, a list should be provided with
#' an element for each well.
#'
#' It should be provided as a list in this form:
#' ```
#' exclude_single <- list(
#'     start_date = "2-12-2021",
#'     series = "Exp1",
#'     dilution = 0.02,
#'     replicate = "1" #will remove both 1 and 1C
#' )
#' ```
#'
#' Optionally, a list-of lists can be provided with repeated definitions of items to exclude:
#'
#' ```
#' exclude_multiple <- list(
#'     list(
#'         start_date = "2-12-2021",
#'         series = "Exp1",
#'         dilution = 0.02,
#'         replicate = "1" #will remove both 1 and 1C
#'     ),
#'     list(
#'         start_date = "2-12-2021",
#'         series = "Exp2",
#'         dilution = 0.01,
#'         replicate = "2" #will remove both 2 and 2C
#'     )
#' )
#' ```
#'
#'
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
                        experiment_dates = "all",
                        exclude_wells = "none") {
    print(exclude_wells)
    if (length(extracts) == 0) stop("argument extracts is empty")
    if (length(strains) == 0) stop("argument strains is empty")
    if (length(replicates) == 0) stop("argument replicates is empty")
    if (length(experiment_dates) == 0) stop("argument experiment_dates is empty")
    if (length(exclude_wells) == 0 ||
        (class(exclude_wells) == "character" && exclude_wells != "none")){
        #||        class(exclude_wells) != "list"
        stop("argument exclude_wells is empty or illegal")
    }

    data <- data %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(strains[1] == "all") TRUE else strain %in% strains) %>%
        dplyr::filter(if(replicates[1] == "all") TRUE else replicate %in% replicates) %>%
        dplyr::filter(if(experiment_dates[1] == "all") TRUE else start_date %in% experiment_dates)

    message_helper("data dimensions:", dim(data))

    if (class(exclude_wells) != "character") {
        message("filtering excludes")

        if (class(exclude_wells[[1]]) == "list") { #multiple
            for (single_exclude in exclude_wells) {
                data <- exclude_data(data, single_exclude)
            }
        } else { #single
            data <- exclude_data(data, exclude_wells)
        }
    }

    return(data)
}

#' not exported helper function to exclude specific wells or columns
exclude_data <- function(data, exclude) {

    if (is.null(exclude$start_date)) stop("no start_date on exclude")
    if (length(exclude$start_date) != 1) stop("only one start_date allowed")

    if (is.null(exclude$series)) stop("no series on exclude")
    if (length(exclude$series) != 1) stop("only one series allowed")

    if (is.null(exclude$dilution)) stop("no dilution on exclude")
    if (length(exclude$dilution) != 1) stop("only one dilution allowed")

    if (is.null(exclude$replicate)) stop("no replicate on exclude")
    if (length(exclude$replicate) != 1) stop("only one replicate allowed")

    #expand to include controls
    exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))

    message_helper("single exclude", exclude)

    data <- data %>%
        dplyr::filter(
            !(start_date == exclude$start_date &
                  series == exclude$series &
                  dilution == exclude$dilution &
                  replicate %in% exclude$replicates))

    return(data)
}


foo <- function() {
    print("foo")
}

