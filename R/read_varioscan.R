#library(readxl)
#library(dplyr)
#library(lubridate)
# replaced by usethis::use_package("readxl") etc

#' Reads Varioscan data from Excel
#'
#' Reads varioscan data from a given excel file. Assumes data is arranged in 96-well format (8 rows and 12 columns). It also assumes a triplicate experimental setup: 3 curves in triplo, each with a control at the fourth column. If the "General_info" tab does not contain metadata at the correct location (see below), the \code{experiments} argument will be used for the labeling.
#' For increased ease in downstream analysis, the "General_info" tab should contain a block of metadata information starting at cell A28 and ending at cell E31. It should have three rows corresponding to the three growth series. The columns should contain "medium addition" (or title, e.g. anthocyanin X), "Strain" (e.g. K. pneumoniae), Date_extracted (in format day/month/year), and "Medium" (e.g. NB 5x Buffercap pH 6)
#'
#' @param xlsx_file A varioscan data file
#' @param dilutions the dilutions used in the subsequent rows for dose-response studies
#' @param time_interval the time interval, in minutes, between each reading
#' @return all data in long format
#'
#' @export
#'
read_varioscan <- function(xlsx_file,
                    dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0),
                    time_interval = 10) {
    #local variables
    data_sheet = "Photometric1"
    metadata_sheet <- "General_Info"
    experiments = c("Exp1", "Exp2", "Exp3")
    plate_format = c(8, 12)
    data_series = paste0(rep(experiments, each = 4), "_", c("1", "2", "3", "C"))
    column_names = c("dilution", data_series)

    #read growth date
    start_date_cell <- readxl::read_excel(xlsx_file,
                                          range = paste0(metadata_sheet, "!F9"),
                                          col_names = FALSE)
    start_date <- lubridate::dmy(unlist(strsplit(start_date_cell$...1,
                                                 split = " "))[1])

    # process metadata
    metadata <- read_metadata(xlsx_file, metadata_sheet)
    #return(metadata)

    data <- readxl::read_excel(xlsx_file, sheet = data_sheet)

    #iterate blocks with timepoints
    rows <- nrow(data)
    starts <- seq(from = 16, to = rows, by = 22)
    #empty tibble to hold everything
    all_data <- list()

    for (n in seq_along(starts)) {
        start <- starts[n]
        time_point = (n - 1) * time_interval
        row_range <- start:(start + (plate_format[1]-1))
        column_range <- 1:(plate_format[2] + 1)

        #select block with measurements
        time_point_block <- data[row_range, column_range]
        #print(time_point_block)

        #give correct names
        names(time_point_block) <- column_names
        time_point_block$dilution = dilutions

        #convert to numeric
        time_point_block <- time_point_block %>%
            dplyr::mutate(across(where(is.character), as.numeric))

        #background correction and averaging
        #return(time_point_block)
        time_point_block <- do_bc_correction_and_averaging(time_point_block)

        #add time column
        time_point_block$time = rep(time_point, nrow(time_point_block))

        #return(time_point_block)
        #pivot to long format
        time_point_block <- time_point_block %>%
            tidyr::pivot_longer(cols = -c(dilution, time),
                                names_to = c("series", "replicate"),
                                names_pattern = "(.+)_(.+)",
                                values_to = "OD")
        #converts integer interval to duration and adds date of experiment
        time_point_block <- time_point_block %>%
            dplyr::mutate(duration = lubridate::dhours(time / 60),
                          start_date = start_date) %>%
            dplyr::select(-time)

        if(! is.null(metadata)) {
            time_point_block <- add_metadata(time_point_block, metadata)
        }
        #return(time_point_block)
        #print(time_point_block)
        all_data[[n]] <- time_point_block
    }
    return(do.call(rbind, all_data))
}


#not exported helper function
do_bc_correction_and_averaging <- function(time_point_block) {
    #dilution Exp1_1 Exp1_2 Exp1_3 Exp1_C Exp2_1 Exp2_2 Exp2_3 Exp2_C Exp3_1 Exp3_2 Exp3_3 Exp3_C
    setup <- matrix(2:13, nrow=3, byrow = TRUE)

    time_point_block <- time_point_block %>%
        dplyr::mutate(
            Exp1_1C = Exp1_1 - Exp1_C,
            Exp1_2C = Exp1_2 - Exp1_C,
            Exp1_3C = Exp1_3 - Exp1_C,
            Exp2_1C = Exp2_1 - Exp2_C,
            Exp2_2C = Exp2_2 - Exp2_C,
            Exp2_3C = Exp2_3 - Exp2_C,
            Exp3_1C = Exp3_1 - Exp3_C,
            Exp3_2C = Exp3_2 - Exp3_C,
            Exp3_3C = Exp3_3 - Exp3_C,
            Exp1_Avg = (Exp1_1C + Exp1_2C + Exp1_3C) / 3,
            Exp2_Avg = (Exp2_1C + Exp2_2C + Exp2_3C) / 3,
            Exp3_Avg = (Exp3_1C + Exp3_2C + Exp3_3C) / 3
        ) %>%
        dplyr::select(c(dilution, Exp1_1, Exp1_1C, Exp1_2, Exp1_2C,
                        Exp1_3, Exp1_3C, Exp1_C, Exp1_Avg,
                        Exp2_1, Exp2_1C, Exp2_2, Exp2_2C,
                        Exp2_3, Exp2_3C, Exp2_C, Exp2_Avg,
                        Exp3_1, Exp3_1C, Exp3_2, Exp3_2C,
                        Exp3_3, Exp3_3C, Exp3_C, Exp3_Avg))

    return(time_point_block)
}

#not exported helper function
add_metadata <- function(time_point_block, metadata) {
    time_point_block <- time_point_block %>% dplyr::mutate(
        # additive = character(nrow(time_point_block)),
        # additive = ifelse(series == "Exp1", metadata$Additive[1], additive),
        # additive = ifelse(series == "Exp2", metadata$Additive[2], additive),
        # additive = ifelse(series == "Exp3", metadata$Additive[3], additive),
        strain = character(nrow(time_point_block)),
        strain = ifelse(series == "Exp1", metadata$Strain[1], strain),
        strain = ifelse(series == "Exp2", metadata$Strain[2], strain),
        strain = ifelse(series == "Exp3", metadata$Strain[3], strain),
        extract = character(nrow(time_point_block)),
        extract = ifelse(series == "Exp1", metadata$Extract[1], extract),
        extract = ifelse(series == "Exp2", metadata$Extract[2], extract),
        extract = ifelse(series == "Exp3", metadata$Extract[3], extract),
        date_extracted = character(nrow(time_point_block)),
        date_extracted = ifelse(series == "Exp1", metadata$Date_extracted[1], date_extracted),
        date_extracted = ifelse(series == "Exp2", metadata$Date_extracted[2], date_extracted),
        date_extracted = ifelse(series == "Exp3", metadata$Date_extracted[3], date_extracted),
        medium = character(nrow(time_point_block)),
        medium = ifelse(series == "Exp1", metadata$Medium[1], medium),
        medium = ifelse(series == "Exp2", metadata$Medium[2], medium),
        medium = ifelse(series == "Exp3", metadata$Medium[3], medium))
    return(time_point_block)
}

#not exported helper function
read_metadata <- function(xlsx_file, metadata_sheet) {
    metadata <- readxl::read_excel(xlsx_file,
                                   range = paste0(metadata_sheet, "!A28:E31"))
    if(! "Extract" %in% names(metadata)) {
        warning("metadata not found (at correct position A28)")
        return(NULL)
    }
    return(metadata)
}



