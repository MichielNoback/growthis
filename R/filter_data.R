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
#' A special case is the removal of a control well (usually when infection has started to grow in it).
#' When a control well is removed, all its values are replaced by its 2 hour value. Corrected values
#' for the replicates will be recalculated.
#'
#' ```
#' exclude_control <- list(
#'     start_date = "2-12-2021",
#'     series = "Exp1",
#'     dilution = 0.02,
#'     replicate = "C" #need to recalculate the corrected values for all three replicates
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
    if (length(extracts) == 0) stop("argument extracts is empty")
    if (length(strains) == 0) stop("argument strains is empty")
    if (length(replicates) == 0) stop("argument replicates is empty")
    if (length(experiment_dates) == 0) stop("argument experiment_dates is empty")
    if (length(exclude_wells) == 0 ||
        (class(exclude_wells) == "character" && exclude_wells != "none")){
        stop("argument exclude_wells is empty or illegal")
    }

    data <- data %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(extracts[1] == "all") TRUE else extract %in% extracts) %>%
        dplyr::filter(if(strains[1] == "all") TRUE else strain %in% strains) %>%
        dplyr::filter(if(replicates[1] == "all") TRUE else replicate %in% replicates) %>%
        dplyr::filter(if(experiment_dates[1] == "all") TRUE else start_date %in% experiment_dates)

    if (class(exclude_wells) != "character") {
        if (class(exclude_wells) != "list") stop("only list data type can exclude wells")
        #message("filtering excludes")
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

    if (exclude$replicate == "C") {
        message("excluding control")

        ## determine new value for this well based on mean value of
        ## 6000 <= t <= 8400
        ODs_for_new_control <- data %>%
            # tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
            dplyr::filter(start_date == exclude$start_date &
                          series == exclude$series &
                          dilution == exclude$dilution &
                          replicate == "C" &
                          duration >=6000 & duration <= 8400) %>%
            dplyr::pull(OD)
        new_control_value <- mean(ODs_for_new_control)
        #print(new_control_value)

        ## now mutate to use the new control value
        data <- data %>% dplyr::mutate(
            OD = ifelse((start_date == exclude$start_date &
                            series == exclude$series &
                            dilution == exclude$dilution &
                            replicate == "C"), new_control_value, OD))

        ## and recalculate the corrected replicates and average
        data <- data %>%
            tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                `1C` = `1` - C,
                `2C` = `2` - C,
                `3C` = `3` - C,
                Avg = mean(c(`1C`, `2C`, `3C`), na.rm = T)
            ) %>%
            tidyr::pivot_longer(cols = 10:17, names_to = "replicate", values_to = "OD") %>%
            dplyr::select(dilution, series, replicate, OD, duration, start_date, strain, extract, extract_id, buffer_strength, pH_buffer)
    } else {
        message("excluding replicate")
        #expand to include corrected values
        exclude$replicates <- c(exclude$replicate, paste0(exclude$replicate, "C"))
        data <- data %>%
            dplyr::filter(
                !(start_date == exclude$start_date &
                      series == exclude$series &
                      dilution == exclude$dilution &
                      replicate %in% exclude$replicates))

        # data are excluded so averages should be recalculated
        data <- data %>%
            tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(Avg = if(any(is.na(c(`1C`, `2C`, `3C`)))) mean(c(`1C`, `2C`, `3C`), na.rm = T) else Avg) %>%
            tidyr::pivot_longer(cols = 10:17, names_to = "replicate", values_to = "OD") %>%
            dplyr::select(dilution, series, replicate, OD, duration, start_date, strain, extract, extract_id, buffer_strength, pH_buffer)
    }

    return(data)
}


foo <- function() {
    print("foo")
}

