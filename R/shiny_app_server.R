#' starts the shiny app
#' @export
#'
shiny_app <- function() {
    #load(paste0(here::here(), "/data/varioscan_data.rda"))
    #shiny::shinyApp(ui = shiny_app_ui, server = shiny_app_server)
    shiny::runApp(list(ui = shiny_app_ui, server = shiny_app_server), launch.browser = TRUE)
}

#foo() # in filter_data.R - runs
#bar() # in shiny_helpers.R -  gives error

#not exported server function
shiny_app_server <- function(input, output, session) {
    # STORE USER SELECTIONS
    user_data <- shiny::reactiveValues(
        filtered_data = dplyr::tibble(),
        selected_experiment = dplyr::tibble(),
        growth_params_single = dplyr::tibble()
    )

    #only runs at start of session
    shiny::observe({
        message("updating experiment dates")
        all_experiment_dates <- available_experiment_dates(growthis::experiment_data)
        shinyWidgets::updatePickerInput(session,
                                 inputId = "experiment_date_single",
                                 choices = all_experiment_dates)
    })

    #only take action when the button is clicked
    shiny::observeEvent(input$show_graph_single, {
        shiny::req(input$experiment_date_single)

        message_helper("displaying experiment date", input$experiment_date_single)

        user_data$selected_experiment <- load_selected_experiment(input$experiment_date_single)
        #print(user_data$selected_experiment)

        output$growthcurve_plot_single <- renderPlot({
            plot_growthcurves(varioscan_data = user_data$selected_experiment,
                              plot_type = input$graph_type_single)
        })

        user_data$growth_params_single <- do_growth_analysis(user_data$selected_experiment)

        output$growth_params_single <- DT::renderDataTable({
            DT::datatable(user_data$growth_params_single,
                          options = list(dom = 'tp', pageLength = 20)) %>%
                DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
                DT::formatSignif(columns = c(8,9,11,14), digits = 2)
        })
    })
    # Downloadable csv of selected dataset
    output$data_download_single <- shiny::downloadHandler(
        filename = function() {
            paste0("growthis_dataset_",
                   input$experiment_date_single, ".csv")
        },
        content = function(file) {
            #to accommodate when there is a download without a graph
            selected_experiment <- load_selected_experiment(input$experiment_date_single)
            write.csv(selected_experiment, file, row.names = FALSE)
        }
    )

    # Downloadable csv of statistics of selected dataset
    output$growth_statistics_single_download <- shiny::downloadHandler(
        filename = function() {
            paste0("growthis_statistics_",
                   input$experiment_date_single, ".csv")
        },
        content = function(file) {
            write.csv(user_data$growth_params_single, file, row.names = FALSE)
        }
    )

    # shiny::observeEvent(input$experiment_dates, {
    #     message_helper("experiment selection changed", input$experiment_dates)
    #     all_extracts <- available_extracts(varioscan_data, input$experiment_dates)
    #     all_strains <- available_strains(varioscan_data, input$experiment_dates)
    #
    #     shiny::updateCheckboxGroupInput(session,
    #                             inputId = "extracts",
    #                             choices = all_extracts,
    #                             selected = all_extracts)
    #
    #     shiny::updateCheckboxGroupInput(session,
    #                             inputId = "strains",
    #                             choices = all_strains,
    #                             selected = all_strains)
    # })

    #only take action when the button is clicked
    # shiny::observeEvent(input$show_graph, {
    #     shiny::req(input$strains, input$extracts)
    #
    #     message("====================")
    #     message_helper("experiment dates", input$experiment_dates)
    #     message_helper("extracts", input$extracts)
    #     message_helper("strains", input$strains)
    #     message_helper("graph_type", input$graph_type)
    #     message("====================")
    #
    #
    #     experiment_dates <<- input$experiment_dates
    #
    #     user_data$filtered_data <- filter_data(data = varioscan_data,
    #                         extracts = input$extracts,
    #                         strains = input$strains,
    #                         experiment_dates = input$experiment_dates)
    #
    #     message_helper("data filtered", unique(user_data$filtered_data$start_date))
    #     #filtered_data <<- user_data$filtered_data
    #
    #     output$growthcurve_plot <- renderPlot({
    #         plot_growthcurves(varioscan_data = user_data$filtered_data,
    #                           plot_type = input$graph_type)
    #     })
    #
    #     user_data$growth_params <- do_growth_analysis(user_data$filtered_data)
    #
    #     output$growth_params <- DT::renderDataTable({
    #         DT::datatable(user_data$growth_params,
    #                       options = list(dom = 'tp', pageLength = 20)) %>%
    #             DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
    #             DT::formatSignif(columns = c(8,9,11,14), digits = 2)
    #     })
    # })

    # Downloadable csv of selected dataset
    # output$data_download <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("growthis_data_download_",
    #                lubridate::today(), ".csv")
    #     },
    #     content = function(file) {
    #         filtered_data <- filter_data(data = varioscan_data,
    #                         extracts = input$extracts,
    #                         strains = input$strains,
    #                         experiment_dates = input$experiment_dates)
    #         write.csv(filtered_data, file, row.names = FALSE)
    #     }
    # )

    # output$growth_statistics_download <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("growthis_statistics_",
    #                lubridate::today(), ".csv")
    #     },
    #     content = function(file) {
    #         write.csv(user_data$growth_params,
    #                   file,
    #                   row.names = FALSE)
    #     }
    # )
}





