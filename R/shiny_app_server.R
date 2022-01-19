#' starts the shiny app
#' @export
#'
shiny_app <- function() {
    load(paste0(here::here(), "/data/varioscan_data.rda"))
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
        message("updating experiment dates")
        all_experiment_dates <- available_experiment_dates(varioscan_data)
        shinyWidgets::updatePickerInput(session,
                                 inputId = "experiment_dates",
                                 choices = all_experiment_dates)
    })

    shiny::observeEvent(input$experiment_dates, {
        message_helper("experiment selection changed", input$experiment_dates)
        all_extracts <- available_extracts(varioscan_data, input$experiment_dates)
        all_strains <- available_strains(varioscan_data, input$experiment_dates)

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

        # extracts <<- input$extracts
        # strains <<- input$strains
        # experiment_dates <<- input$experiment_dates

        user_data$filtered_data <- filter_data(data = varioscan_data,
                            extracts = input$extracts,
                            strains = input$strains,
                            experiment_dates = input$experiment_dates)

        message_helper("data filtered", unique(user_data$filtered_data$start_date))
        #filtered_data <<- user_data$filtered_data

        output$growthcurve_plot <- renderPlot({
            plot_growthcurves(varioscan_data = user_data$filtered_data,
                              plot_type = input$graph_type)
        })

        user_data$growth_params <- do_growth_analysis(user_data$filtered_data)

        output$growth_params <- DT::renderDataTable({
            DT::datatable(user_data$growth_params
                          , options = list(dom = 'tp')) %>%
                DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
                DT::formatSignif(columns = c(8,9,11,14), digits = 2)
        })
    })

    # Downloadable csv of selected dataset
    output$data_download <- shiny::downloadHandler(
        filename = function() {
            paste0("growthis_data_download_",
                   lubridate::today(), ".csv")
        },
        content = function(file) {
            filtered_data <- filter_data(data = varioscan_data,
                            extracts = input$extracts,
                            strains = input$strains,
                            experiment_dates = input$experiment_dates)
            write.csv(filtered_data, file, row.names = FALSE)
        }
    )

    output$growth_statistics_download <- shiny::downloadHandler(
        filename = function() {
            paste0("growthis_statistics_",
                   lubridate::today(), ".csv")
        },
        content = function(file) {
            write.csv(user_data$growth_params,
                      file,
                      row.names = FALSE)
        }
    )
}





