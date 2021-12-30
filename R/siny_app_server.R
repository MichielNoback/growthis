#' starts the shiny app
#' @export
#'
shiny_app <- function() {
    shiny::shinyApp(ui = shiny_app_ui, shiny_app_server)
}

load(paste0(here::here(), "/data/varioscan.rda"))

all_extracts <<- available_extracts(varioscan)
all_strains <<- available_strains(varioscan)

#not exported server function
shiny_app_server <- function(input, output, session) {
    #STORE FILTER SELECTIONS
    user_data <- shiny::reactiveValues(
        date_range = list(start = lubridate::today(),
                           end = lubridate::today()),
        extracts = character(0),
        strains = character(0),
        graph_type = character(0)
    )

    #only runs at start of session
    shiny::observe({
        message("updating extracts and strains")
        shiny::updateCheckboxGroupInput(session,
                                        inputId = "extracts",
                                        choices = all_extracts,
                                        selected = all_extracts)
        shiny::updateCheckboxGroupInput(session,
                                        inputId = "strains",
                                        choices = all_strains,
                                        selected = all_strains)
    })


    ##UI OBSERVERS ONLY STORE IN user_data

    shiny::observeEvent(input$date_range, {
        message(paste0("date range changed: ", paste0(input$date_range, collapse = "-")))
        user_data$date_range <- input$date_range
    })

    shiny::observeEvent(input$extracts, {
        user_data$extracts <- input$extracts
        message(paste0("extracts changed: ", paste(user_data$extracts, collapse = "-")))
    })

    shiny::observeEvent(input$strains, {
        user_data$strains <- input$strains
        message(paste0("strains changed: ", paste(user_data$strains, collapse = "-")))
    })

    shiny::observeEvent(input$graph_type, {
        user_data$graph_type <- input$graph_type
        message(paste0("graph_type changed: ", paste(user_data$graph_type, collapse = "-")))
    })



}
