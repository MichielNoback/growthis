# not exported shiny helper functions

#' serves available extracts of the dataset
#'
#' @param data the varioscan data tibble
#'
available_extracts <- function(data) {
    unique(data$extract)
}


#' serves available strains of the dataset
#'
#' @param data the varioscan data tibble
#'
available_strains <- function(data) {
    unique(data$strain)
}

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
