#' The experiments, with their dates and descriptions
#'
#' There are multiple indivdual datasets available that are not documented individually.
#' They are identified by their run date, e.g. `growthis::\\`12-11-2021\\``. Simply type growthis:: in the console to discover them, or use the Shiny app.
#'
#' @format A tibble frame with 20,928 rows and 10 variables, in long format
#' \describe{
#'   \item{file_name}{the name of the excel file with raw varioscan data and experiment metadata}
#'   \item{experiment_date}{the date on which the experimen was run. This identifies the dataset uniquely}
#'   \item{description}{A short description of the experiment}
#' }
#'
#' All individual datasets have this format:
#'
#' The plate has a 8 rows * 12 columns layout.
#' The experiments were set up in three blocks of 4 columns each, see picture below.
#'  \figure{varioscan_layout.png}{options: width=90\%}
#'
#' @format A tibble frame with 20,928 rows and 10 variables, in long format
#' \describe{
#'   \item{dilution}{the dilution of the extract that was added}
#'   \item{series}{the series; one of three (each 4 columns)}
#'   \item{replicate}{the replicate; can be 1/2/3 for raw measurements, C for control (column 4 of each series), 1C/2C/3C for corrected measurements or Avg for the average of 1C/2C/3C}
#'   \item{OD}{the OD; either raw or corrected}
#'   \item{duration}{the duration; a lubridate Duration object. Increases in 10m intervals}
#'   \item{start_date}{a lubridate Date  object.the start date of the experiment}
#'   \item{strain}{the bacterial strain used in the growth experiment}
#'   \item{extract}{the extract used for the dilution series}
#'   \item{extract_id}{the the ID of the extract}
#'   \item{buffer_strength}{the strength of the medium buffer}
#'   \item{pH_buffer}{the initial pH of the medium}
#' }
#'
#'
"experiment_data"
