#' Reads remote data store and compares to the data collection already present
#'
#' Assumes that the directory has a .htaccss file with this content:
#' DirectoryIndex index
#' IndexOptions Type=text/plain SuppressSize
#'
#'
check_remote_for_new_datasets <- function(all_experiment_dates) {
    message_helper("checking for new datasets. Already present:", all_experiment_dates)
    remote_data_url <- "https://bioinf.nl/~michiel/growthis_data/"
    ## download listing
    filenames <- RCurl::getURL(remote_data_url, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
    ## remove all html tags
    tmp <- gsub("<.*?>","",filenames)
    ## proces into character vector of filenames
    tmp <- unlist(strsplit(tmp, "\n+"))
    tmp <- trimws(tmp[grepl(".xlsx?", tmp)])

    ## read and compare with already registered
    for (i in seq_along(tmp)) {
        filename <- paste0(remote_data_url, tmp[i])

        print(filename)
        tempfile <- tempfile(fileext = ".xlsx")
        download.file(url = filename, destfile = tempfile, mode = "wb", quiet = TRUE)
        # httr::GET(url = filename, httr::write_disk(tempfile))
        #df <- read_excel(tf, 2L)

        start_date <- extract_start_date(xlsx_file = tempfile, metadata_sheet = "General_Info")
        print(start_date)
        break
    }

}


#' Loads a single experiment from Rda file (from package package data)
#'
load_selected_experiment <- function(experiment_date_single) {
    #message_helper("loading", experiment_date_single)
    return(get(experiment_date_single))
}


#' Reads experiment names to be used in the app
#'
#' @param xlsx_file An excel file with two columns: `file_name` and `experiment_name`
#' @return a tibble with 3 columns
#'
#' @export
#'
read_experiment_data <- function(xlsx_file) {
    experiment_names <- readxl::read_excel(xlsx_file)
    return(experiment_names)
}


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
    data_sheet <- 2 #"Photometric"
    metadata_sheet <- "General_Info"
    experiments = c("Exp1", "Exp2", "Exp3")
    plate_format = c(8, 12)
    data_series = paste0(rep(experiments, each = 4), "_", c("1", "2", "3", "C"))
    column_names = c("dilution", data_series)
    start_date <- extract_start_date(xlsx_file, metadata_sheet)

    # process metadata
    metadata <- read_metadata(xlsx_file, metadata_sheet)

    data <- suppressMessages(readxl::read_excel(xlsx_file, sheet = data_sheet))

    # iterate blocks with timepoints
    rows <- nrow(data)
    starts <- seq(from = 16, to = rows, by = 22)
    # empty tibble to hold everything
    all_data <- list()

    for (n in seq_along(starts)) {
        start <- starts[n]
        time_point <- (n - 1) * time_interval
        row_range <- start:(start + (plate_format[1]-1))
        column_range <- 1:(plate_format[2] + 1)

        # select block with measurements
        time_point_block <- data[row_range, column_range]

        # give correct names
        names(time_point_block) <- column_names
        time_point_block$dilution = dilutions

        time_point_block <- process_raw_time_point_block(time_point_block,
                                                         time_point,
                                                         start_date)

        if(! is.null(metadata)) {
            time_point_block <- add_metadata(time_point_block, metadata)
        }

        all_data[[n]] <- time_point_block
    }
    # bind them all
    all_data <- data.table::rbindlist(all_data)
    #all_data <- do.call(rbind, all_data)

    # finally remove all "discard" data
    all_data <- all_data %>% dplyr::filter(strain != "discard")

    return(all_data)
}


#' Processes a single time-point block
#' Not exported helper function
process_raw_time_point_block <- function(time_point_block, time_point, start_date) {
    # convert to numeric
    time_point_block <- time_point_block %>%
        dplyr::mutate(across(where(is.character), as.numeric))

    # background correction and averaging
    time_point_block <- do_bc_correction_and_averaging(time_point_block)

    # add time column
    time_point_block$time = rep(time_point, nrow(time_point_block))

    # pivot to long format
    time_point_block <- time_point_block %>%
        tidyr::pivot_longer(cols = -c(dilution, time),
                            names_to = c("series", "replicate"),
                            names_pattern = "(.+)_(.+)",
                            values_to = "OD")

    # converts integer interval to duration and adds date of experiment
    time_point_block <- time_point_block %>%
        dplyr::mutate(duration = lubridate::dhours(time / 60),
                      start_date = start_date,
                      #experiment_name = experiment_name
        ) %>%
        dplyr::select(-time)

    return(time_point_block)
}


#not exported helper function
extract_start_date <- function(xlsx_file, metadata_sheet) {
    start_date_cell <- suppressMessages(readxl::read_excel(xlsx_file,
                                          range = paste0(metadata_sheet, "!F9"),
                                          col_names = FALSE))
    start_date <- as.character(unlist(strsplit(start_date_cell$...1, split = " "))[1])
    return(start_date)
}

#not exported helper function
extract_experiment_name <- function(xlsx_file, metadata_sheet) {
    experiment_name <- readxl::read_excel(xlsx_file,
                                          range = paste0(metadata_sheet, "!F5"),
                                          col_names = FALSE)
    return(experiment_name$...1)
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
        strain = character(nrow(time_point_block)),
        strain = ifelse(series == "Exp1", metadata$Strain[1], strain),
        strain = ifelse(series == "Exp2", metadata$Strain[2], strain),
        strain = ifelse(series == "Exp3", metadata$Strain[3], strain),
        extract = character(nrow(time_point_block)),
        extract = ifelse(series == "Exp1", metadata$Extract[1], extract),
        extract = ifelse(series == "Exp2", metadata$Extract[2], extract),
        extract = ifelse(series == "Exp3", metadata$Extract[3], extract),
        extract_id = character(nrow(time_point_block)),
        extract_id = ifelse(series == "Exp1", metadata$Extract_ID[1], extract_id),
        extract_id = ifelse(series == "Exp2", metadata$Extract_ID[2], extract_id),
        extract_id = ifelse(series == "Exp3", metadata$Extract_ID[3], extract_id),
        buffer_strength = character(nrow(time_point_block)),
        buffer_strength = ifelse(series == "Exp1", metadata$Buffer_strength[1], buffer_strength),
        buffer_strength = ifelse(series == "Exp2", metadata$Buffer_strength[2], buffer_strength),
        buffer_strength = ifelse(series == "Exp3", metadata$Buffer_strength[3], buffer_strength),
        pH_buffer = character(nrow(time_point_block)),
        pH_buffer = ifelse(series == "Exp1", metadata$pH_buffer[1], pH_buffer),
        pH_buffer = ifelse(series == "Exp2", metadata$pH_buffer[2], pH_buffer),
        pH_buffer = ifelse(series == "Exp3", metadata$pH_buffer[3], pH_buffer))
    return(time_point_block)
}

#not exported helper function
read_metadata <- function(xlsx_file, metadata_sheet) {
    metadata <- suppressMessages(readxl::read_excel(xlsx_file,
                                   range = paste0(metadata_sheet, "!A28:F31")))
    if(! "Extract" %in% names(metadata)) {
        warning("metadata not found (at correct position A28)")
        return(NULL)
    }
    return(metadata)
}



