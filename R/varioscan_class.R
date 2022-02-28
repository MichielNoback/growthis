#' Constructor for varioscan data object
#'
#'
varioscan <- function(growth_data, metadata) {
    if (! "tbl_df" %in% class(growth_data)) {
        stop("Growth_data should be a tibble")
    }

    value <- list(
        "growth_data" = growth_data,
        "metadata" = metadata
    )
}


#' Constructor for the metadata object that will be part of the varioscan data
metadata <- function(date_started,
                     dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0),
                     experiment_name = NA) {

    self.series <- tibble::tibble(
        series_name = c("Exp1", "Exp2", "Exp3"),
        strain = character(3),
        extract = character(3),
        extract_id = character(3),
        buffer_strength = numeric(3),
        pH_buffer = character(3)
    )

    structure(class = "varioscan_metadata", list(
        experiment_name = experiment_name,
        date_started = date_started,
        dilutions = dilutions,

        strains = function(strains = NULL) {
            #if null simply return as getter
            if (is.null(strains)) return(self.series$strain)

            #else function as setter
            if (! (class(strains) == "character" && length(strains == 3))) {
                stop("strains should be a 3-element character vector")
            }
            self.series$strain <<- strains;
            return(self.series$strain)
        },

        get_series = function() self.series

    ))
}

print.variascan_metadata <- function(x) {
    message(paste0("varioscan_metadata object: "), x$date_started)
    print(x$get_series())
}


test_meta <- function() {
    m_data <<- metadata(date_started = "2020-03-02", experiment_name = "Growth curve of S.aureus with tulip extracts")
    m_data$strains(LETTERS[1:3])
    print.variascan_metadata(m_data)
}


# metadata <- list(
#     experiment_description = "SMMK Growth Curve K pneumoniae 2maart2020",
#     date_started = "2020-03-02",
#     dilutions = c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0),
#     series = tibble::tibble(
#         series_name = c("Exp1", "Exp2", "Exp3"),
#         strain = c("K. pneumoniae", "K. pneumoniae", "K. pneumoniae"),
#         extract = c("Red Naomi_unkown", "White rose_unkown", "Elution control"),
#         extract_id = c("00-00-0000", "00-00-0000", "00-00-0000"),
#         buffer_strength = c("NB1x_pH7.4", "NB1x_pH7.4", "NB1x_pH7.4"),
#         pH_buffer = C()
#     )
# )

