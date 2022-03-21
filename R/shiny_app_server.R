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
    ui_glyphs <- get_ui_glyphs()

#    current_dataset <- load_test_dataset()

    # plate_layout <- get_plate_layout(current_dataset)
    # clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, plate_layout)


    #The reactive version of the data
    plate_layout_reactive = shiny::reactiveValues(
        wells = dplyr::tibble() #clickable_plate_layout
    )

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


        plate_layout <- get_plate_layout(user_data$selected_experiment)
        clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, plate_layout)
        plate_layout_reactive$wells <- clickable_plate_layout

        #show well selection box
        #shinyjs::toggle("well_selection_box_wrapper")


        # user_data$growth_params_single <- do_growth_analysis(user_data$selected_experiment)
        #
        # output$growth_params_single <- DT::renderDataTable({
        #     DT::datatable(user_data$growth_params_single,
        #                   options = list(dom = 'tp', pageLength = 20)) %>%
        #         DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
        #         DT::formatSignif(columns = c(8,9,11,14), digits = 2)
        # })
    })

    #The proxy to update the DT
    proxy <- DT::dataTableProxy(outputId = 'well_selection')


    #Update the well selection box table when clicked
    shiny::observeEvent(input$well_selection_cells_selected, {
        #message("icon clicked")
        shiny::req(input$well_selection_cells_selected)

        # WEIRD THIS DOES SUDDENLY NOT WORK ANYMORE:
        # plate_layout_reactive$wells[input$well_selection_cells_selected]
        # (IT IS A 2-ELEMENT VECTOR), WITH ERROR
        # Warning: Error in : Subscript `input$well_selection_cells_selected` is a matrix, it must be of type logical.
        s_row <- input$well_selection_cells_selected[1]
        s_col <- input$well_selection_cells_selected[2]
        clicked_glyph <- plate_layout_reactive$wells[s_row, s_col]
        plate_layout_reactive$wells[s_row, s_col] <- get_new_glyph(clicked_glyph, ui_glyphs)

        #Send proxy (no need to refresh whole table)
        DT::replaceData(proxy, plate_layout_reactive$wells)
    })

    shiny::observeEvent(input$remove_wells_button, {
        message("removing indices")

        selected_wells <- get_selected_wells(plate_layout_reactive$wells, dplyr::pull(user_data$selected_experiment, start_date)[1])
        #print(selected_wells)
        current_dataset <- user_data$selected_experiment
        #print(user_data$selected_experiment)
        modified_dataset <- filter_data(user_data$selected_experiment, exclude_wells = selected_wells)
        #print(modified_dataset)
        user_data$selected_experiment <- modified_dataset

        new_plate_layout <- get_plate_layout(modified_dataset)
        #print(new_plate_layout)
        new_clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, new_plate_layout)
        #print(new_clickable_plate_layout)
        plate_layout_reactive$wells <- new_clickable_plate_layout
        DT::replaceData(proxy, plate_layout_reactive$wells)
    })


    #The "checkbox" table
    output$well_selection = DT::renderDataTable(
        plate_layout_reactive$wells,
        #clickable_plate_layout,
        selection = list(mode = "single", target = 'cell'),
        options = list(
            columnDefs = list(list(width = '10px', targets ="_all")), #list(className = 'dt-center', targets = 1:(ncol(clickable_plate_layout)-1)),
            dom = "t",
            ordering = F
        ),
        #rownames= FALSE,  #this gives an empty table after a click!
        class = "plate_layout",
        escape = F,
        width = "100",
        fillContainer = FALSE
    )

    # Downloadable csv of selected dataset
    output$data_download_single <- shiny::downloadHandler(
        filename = function() {
            paste0("growthis_dataset_",
                   input$experiment_date_single, ".csv")
        },
        content = function(file) {
            #to accommodate when there is a download without a graph
            selected_experiment <- user_data$selected_experiment #load_selected_experiment(input$experiment_date_single)
            write.csv(selected_experiment, file, row.names = FALSE)
        }
    )

    # button id = show_statistics_single
    # table id = growth_params_single
    #only take action when the button is clicked
    shiny::observeEvent(input$show_statistics_single, {
        shiny::req(input$show_statistics_single)

        message_helper("displaying statistics of", input$experiment_date_single)

        user_data$growth_params_single <- do_growth_analysis(user_data$selected_experiment)

        output$growth_params_single <- DT::renderDataTable({
            DT::datatable(user_data$growth_params_single,
                          options = list(dom = 'tp', pageLength = 20)) %>%
                DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
                DT::formatSignif(columns = c(8,9,11,14), digits = 2)
        })
    })


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
}


#below removed but saved for now

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
#}





