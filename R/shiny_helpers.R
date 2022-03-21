# not exported shiny helper functions


#' serves UI glyphs of the well selection editor box
get_ui_glyphs <- function() {
    #The glyphs to represent the wells
    ok_glyph <- as.character(icon("ok-circle", lib = "glyphicon", style="font-size:1.4em"))
    remove_glyph <- as.character(icon("remove-sign", lib = "glyphicon", style="color:darkred;font-size:1.4em"))
    excluded_glyph <- as.character(icon("ban-circle", lib = "glyphicon", style="color:darkgrey;font-size:1.4em"))

    list("ok_glyph" = ok_glyph,
         "remove_glyph" = remove_glyph,
         "excluded_glyph" = excluded_glyph)
}

#' returns the plate layout belonging to the given experiment
get_plate_layout <- function(selected_experiment) {
    plate_layout <- selected_experiment %>%
        dplyr::select(dilution, series, replicate, OD, duration) %>%
        dplyr::filter(replicate %in% c("1", "2", "3", "C")) %>%
        dplyr::group_by(dilution, series, replicate) %>%
        dplyr::summarize(excluded = any(is.na(OD)), .groups = "drop") %>%
        #dplyr::ungroup() %>%
        dplyr::mutate(series_repl = paste0(series, "_", replicate)) %>%
        dplyr::select(dilution, series_repl, excluded) %>%
        dplyr::arrange(dplyr::desc(dilution)) %>%
        tidyr::pivot_wider(names_from = series_repl, values_from = excluded)

    plate_layout
}

#' Adds glyphicons to a given plate layout
get_clickable_plate_layout <- function(ui_glyphs, plate_layout) {
    plate_layout <- plate_layout %>%
        dplyr::mutate(dplyr::across(dplyr::matches("Exp"), function(x) ifelse(x, ui_glyphs$excluded_glyph, ui_glyphs$ok_glyph)))
    return(plate_layout)
}


#' returns new glyph depending on the glyph that was clicked
get_new_glyph <- function(clicked_glyph, ui_glyphs) {
    if(clicked_glyph == ui_glyphs$excluded_glyph) return(ui_glyphs$excluded_glyph)
    else if (clicked_glyph == ui_glyphs$ok_glyph) return(ui_glyphs$remove_glyph)
    return(ui_glyphs$ok_glyph)
}


#' serves a list of selected wells, to be used for downstram filtering using the filter_data function
get_selected_wells <- function(wells, start_date_of_exp) {
    tbbl_selection <- wells %>%
        tidyr::pivot_longer(cols = -1, names_to = "exp", values_to = "glyph") %>%
        dplyr::filter(grepl(pattern = "remove-sign", glyph)) %>%
        tidyr::separate(col = exp, into = c("series", "replicate")) %>%
        dplyr::select(-glyph)

    list_selection <- apply(X = tbbl_selection,
                            MARGIN = 1,
                            FUN = function(x) list(
                                start_date = start_date_of_exp,
                                dilution = as.numeric(x["dilution"]),
                                series = x["series"],
                                replicate = x["replicate"]))

    list_selection
}


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
