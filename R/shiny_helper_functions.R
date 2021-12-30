
#not exported shiny helper function
available_extracts <- function(data) {
    unique(data$extract)
}


#not exported shiny helper function
available_strains <- function(data) {
    unique(data$strain)
}
