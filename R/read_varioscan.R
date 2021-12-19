#library(readxl)
#library(dplyr)
# replaced by usethis::use_package("readxl") etc

#' Reads Varioscan data from Excel
#'
#' Reads varioscan data from a given excel file. Assumes data is arranged in 96-well format (8 rows and 12 columns). It also assumes a triplicate experimental setup: 3 curves in triplo, each with a control at the fourth column. If the "General_info" tab does not contain metadata at the correct location (see below), the \code{experiments} argument will be used for the labeling.
#' For increased ease in downstream analysis, the "General_info" tab should contain a block of metadata information starting at cell A28 and ending at cell E31. It should have three rows corresponding to the three growth series. The columns should contain "medium addition" (or title, e.g. anthocyanin X), "Strain" (e.g. K. pneumoniae), Date_extracted (in format day/month/year), and "Medium" (e.g. NB 5x Buffercap pH 6)
#'
#' @param xlsx_file A varioscan data file
#' @param dilutions the dilutions used in the subsequent rows for dose-response studies
#' @param experiments the 3 experiments (will end up as labels in the long format). Only used if metadata is not present in excel file
#' @param time_interval the time interval, in minutes, between each reading
#' @return all data in long format
#'
#' @export
#'
read_varioscan <- function(xlsx_file,
                    dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0),
                    experiments = c("Exp1", "Exp2", "Exp3"),
                    time_interval = 10) {
    #local variables
    data_sheet = "Photometric1"
    metadata_sheet <- "General_Info"

    plate_format = c(8, 12)
    data_series = paste0(rep(experiments, each = 4), "_", c("1", "2", "3", "C"))
    column_names = c("dilution", data_series)

    # process metadata
    metadata <- read_metadata(xlsx_file, metadata_sheet)
    return(metadata)

    data <- readxl::read_excel(xlsx_file, sheet = data_sheet)
    #fetch sample layout
    #sample_layout <- data[5:13, 1:13]

    #iterate blocks with timepoints
    rows <- nrow(data)
    starts <- seq(from = 16, to = rows, by = 22)
    #empty tibble to hold everything
    all_data <- list()

    for (n in seq_along(starts)) {
        start <- starts[n]
        #print(start)
        time_point = (n - 1) * time_interval
        row_range <- start:(start + (plate_format[1]-1))
        column_range <- 1:(plate_format[2] + 1)
        #select block with measurements
        time_point_block <- data[row_range, column_range]
        #print(time_point_block)
        #give correct names
        names(time_point_block) <- column_names
        time_point_block$dilution = dilutions
        time_point_block$time = rep(time_point, nrow(time_point_block))
        time_point_block <- time_point_block %>%
            dplyr::mutate(across(where(is.character), as.numeric))


        time_point_block <- time_point_block %>%
            tidyr::pivot_longer(cols = -c(dilution, time),
                                names_to = c("series", "replicate"),
                                names_pattern = "(.+)_(.+)",
                                values_to = "OD")

        #print(time_point_block)

        all_data[[n]] <- time_point_block
    }
    return(do.call(rbind, all_data))
}

read_metadata <- function(xlsx_file, metadata_sheet) {
    metadata <- readxl::read_excel(xlsx_file,
                                   range = paste0(metadata_sheet, "!A28:E31"))
    return(metadata)
}
