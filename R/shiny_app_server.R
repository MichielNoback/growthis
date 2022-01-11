#' starts the shiny app
#' @export
#'
shiny_app <- function() {
    load(paste0(here::here(), "/data/varioscan.rda"))
    shiny::shinyApp(ui = shiny_app_ui, server = shiny_app_server)
}

#foo() # in filter_data.R - runs
#bar() # in shiny_helpers.R -  gives error

#not exported server function
shiny_app_server <- function(input, output, session) {
    # STORE FILTER SELECTIONS
    user_data <- shiny::reactiveValues(
        filtered_data = dplyr::tibble()
    )

    #only runs at start of session
    shiny::observe({
        message("updating extracts and strains")
        all_extracts <- available_extracts(varioscan)
        all_strains <- available_strains(varioscan)
        shiny::updateCheckboxGroupInput(session,
                                        inputId = "extracts",
                                        choices = all_extracts,
                                        selected = all_extracts)
        shiny::updateCheckboxGroupInput(session,
                                        inputId = "strains",
                                        choices = all_strains,
                                        selected = all_strains)
    })

    #only take action when the button is clicked
    shiny::observeEvent(input$show_graph, {
        shiny::req(input$strains, input$extracts)

        #user_data$graph_type <- input$graph_type
        message("====================")
        message_helper("graph requested with date_range", input$date_range)
        message_helper("extracts", input$extracts)
        message_helper("strains", input$strains)
        message_helper("graph_type", input$graph_type)
        message("====================")

        user_data$filtered_data <- filter_data(data = varioscan,
                            extracts = input$extracts,
                            strains = input$strains,
                            lower_date = lubridate::ymd(input$date_range[1]),
                            upper_date = lubridate::ymd(input$date_range[2]))
        #print(user_data$filtered_data)

        output$growthcurve_plot <- renderPlot({
            plot_growthcurves(varioscan_data = user_data$filtered_data,
                              type = input$graph_type)
        })
    })



    ##UI OBSERVERS ONLY STORE IN user_data

    # shiny::observeEvent(input$date_range, {
    #     message_helper("date range changed", user_data$date_range)
    #     user_data$date_range <- input$date_range
    # })
    #
    # shiny::observeEvent(input$extracts, {
    #     user_data$extracts <- input$extracts
    #     message_helper("extracts changed", user_data$extracts)
    # })
    #
    # shiny::observeEvent(input$strains, {
    #     user_data$strains <- input$strains
    #     message_helper("strains changed", user_data$strains)
    # })
    #
    # shiny::observeEvent(input$graph_type, {
    #     user_data$graph_type <- input$graph_type
    #     message_helper("graph_type changed", user_data$graph_type)
    # })

}





