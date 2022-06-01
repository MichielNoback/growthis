#' starts the shiny app
#'
#'
shiny_app <- function(launch_browser = FALSE, port = 3838) {
    shiny::runApp(list(ui = shiny_app_ui,
                       server = shiny_app_server),
                  launch.browser = launch_browser,
                  port = port)
}

#shiny::shinyApp(ui = shiny_app_ui, server = shiny_app_server)


#not exported server function
shiny_app_server <- function(input, output, session) {
    ui_glyphs <- get_ui_glyphs()

    #The reactive version of the plate layout
    plate_layout_reactive = shiny::reactiveValues(
        wells = dplyr::tibble() #clickable_plate_layout
    )

    # STORE USER SELECTIONS
    user_data <- shiny::reactiveValues(
        filtered_data_multi = dplyr::tibble(),
        selected_experiment = dplyr::tibble(),
        selected_experiments_multi = dplyr::tibble(),
        growth_params_single = dplyr::tibble(),
        growth_params_multi = dplyr::tibble(),
    )

    ## only runs at start of session
    shiny::observe({
        message("updating experiment dates")
        all_experiment_dates <- available_experiment_dates(growthis::experiment_data)

        #message("scanning remote data store")
        #new_experiment_dates <- check_remote_for_new_datasets(all_experiment_dates)
        #message("New experiments found; adding these to package data")

        shinyWidgets::updatePickerInput(session,
                                 inputId = "experiment_date_single",
                                 choices = all_experiment_dates)
        shinyWidgets::updatePickerInput(session,
                                        inputId = "experiment_date_multiple",
                                        choices = all_experiment_dates)
    })

    ## an excel file is uploaded with varioscan data
    shiny::observe({
        shiny::req(input$local_varioscan_excel)
        message_helper("reading uploaded file", input$local_varioscan_excel$datapath)
        user_data$selected_experiment <- read_varioscan(input$local_varioscan_excel$datapath)
    })

    ## A built-in dataset was selected
    shiny::observe({
        shiny::req(input$experiment_date_single)
        message_helper("displaying experiment data", input$experiment_date_single)
        user_data$selected_experiment <- load_selected_experiment(input$experiment_date_single)
    })


    ## listens to changes in experiment selections of multi experiment viewer
    shiny::observeEvent(input$experiment_date_multiple, {
        message_helper("multi experiment selection changed", input$experiment_date_multiple)

        ## Load requested datasets
        all_data <- list()

        for (exp_date in input$experiment_date_multiple) {
            message_helper("loading", exp_date)
            exp <- load_selected_experiment(exp_date)
            all_data[[exp_date]] <- exp
        }
        user_data$selected_experiments_multi <- data.table::rbindlist(all_data) #do.call(rbind, all_data)
    })

    ## When one or more local files were uploaded they have to be added to the current selection
    shiny::observeEvent(input$local_varioscan_excel_multi$datapath, {
        shiny::req(input$local_varioscan_excel_multi$datapath)

        all_data <- list()
        for (uploaded_file in input$local_varioscan_excel_multi$datapath) {
            message_helper("loading", uploaded_file)
            exp <- read_varioscan(uploaded_file)
            exp_date <- exp$start_date[1]
            all_data[[exp_date]] <- exp
        }
        all_data <- data.table::rbindlist(all_data)
        ## append if already with data from built-in selection
        if(nrow(user_data$selected_experiments_multi) > 1) {
            user_data$selected_experiments_multi <- rbind(user_data$selected_experiments_multi, all_data)
        } else {
            user_data$selected_experiments_multi <- all_data
        }
    })

    ## listens to changes in the user_data$selected_experiments_multi dataset
    shiny::observe({
        shiny::req(user_data$selected_experiments_multi)
        all_extracts <- available_extracts(user_data$selected_experiments_multi)
        all_strains <- available_strains(user_data$selected_experiments_multi)

        shiny::updateCheckboxGroupInput(session,
                                        inputId = "extracts_multiple",
                                        choices = all_extracts,
                                        selected = all_extracts)

        shiny::updateCheckboxGroupInput(session,
                                        inputId = "strains_multiple",
                                        choices = all_strains,
                                        selected = all_strains)
    })


    ## Only take action when the button is clicked in the multi experiment viewer
    ## to show the graph and prepare combined data
    shiny::observeEvent(input$show_graph_multiple, {
        shiny::req(input$strains_multiple, input$extracts_multiple)

        message("====================")
        message_helper("experiment dates", input$experiment_date_multiple)
        message_helper("extracts", input$extracts_multiple)
        message_helper("strains", input$strains_multiple)
        message_helper("graph_type", input$graph_type_multiple)
        message("====================")


        user_data$filtered_data_multi <- filter_data(data = user_data$selected_experiments_multi,
                            extracts = input$extracts_multiple,
                            strains = input$strains_multiple)

        output$growthcurve_plot_multiple <- renderPlot({
            plot_growthcurves(varioscan_data = user_data$filtered_data_multi,
                              plot_type = input$graph_type_multiple)
        })
        user_data$growth_params_multi <- do_growth_analysis(user_data$filtered_data_multi)

        output$growth_params_multiple <- DT::renderDataTable({
            DT::datatable(user_data$growth_params_multi,
                          options = list(dom = 'tp', pageLength = 20)) %>%
                DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
                DT::formatSignif(columns = c(8,9,11,14), digits = 2)
        })
    })

    ## listens to experiment selection changes as reactive of user_data$selected experiment
    shiny::observe({
        shiny::req(user_data$selected_experiment)
        if (nrow(user_data$selected_experiment > 1)) {
            output$growthcurve_plot_single <- renderPlot({
                plot_growthcurves(varioscan_data = user_data$selected_experiment,
                                  plot_type = input$graph_type_single)
            })

            plate_layout <- get_plate_layout(user_data$selected_experiment)
            clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, plate_layout)
            plate_layout_reactive$wells <- clickable_plate_layout

            ## Button with explicit action was removed
            message_helper("displaying statistics of", input$experiment_date_single)
            user_data$growth_params_single <- do_growth_analysis(user_data$selected_experiment)
            growth_params_single <- user_data$growth_params_single
            output$growth_params_single <- DT::renderDataTable({
                DT::datatable(user_data$growth_params_single,
                              options = list(dom = 'tp', pageLength = 20)) %>%
                    DT::formatRound(columns = c(5,6,7,10,12,13), digits = 2, interval = 10) %>%
                    DT::formatSignif(columns = c(8,9,11,14,16), digits = 2)
            })

            shiny::updateSelectInput(inputId = "growth_params_plot_variable_single",
                                     choices = names(growth_params_single)[5:14])

            growth_params_tibble <- mutate(user_data$growth_params_single, dilution = as.numeric(dilution))
            if(input$model_plot_exp_selection_single != "all") {
                growth_params_tibble <- growth_params_tibble %>%
                    filter(series == input$model_plot_exp_selection_single)
            }

            all_model_data <- model_dose_response(growth_params = growth_params_tibble,
                                                  dependent_var = input$model_plot_dependent_var_selection_single,
                                                  nls_trace = FALSE)

            output$yield_over_concentration_plot_single <- shiny::renderPlot({ # plotly::renderPlotly({
                plot_dependent_var_over_concentration(growth_params_tibble,
                                                      all_model_data = all_model_data,
                                                      dependent_var = input$model_plot_dependent_var_selection_single,
                                                      exp = input$model_plot_exp_selection_single)
            })

            output$model_info_single <- shiny::renderPrint(all_model_data$models)
        }
    })

    ## The proxy to update the DT
    proxy <- DT::dataTableProxy(outputId = 'well_selection')

    ## Update the well selection box table (glyplicon) when clicked
    shiny::observeEvent(input$well_selection_cells_selected, {
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

    ## Wells were selected for removal in the well selection grid
    shiny::observeEvent(input$remove_wells_button, {
        message("removing indices")

        selected_wells <- get_selected_wells(plate_layout_reactive$wells,
                                             dplyr::pull(user_data$selected_experiment, start_date)[1])
        current_dataset <- user_data$selected_experiment
        modified_dataset <- filter_data(user_data$selected_experiment, exclude_wells = selected_wells)
        user_data$selected_experiment <- modified_dataset
        new_plate_layout <- get_plate_layout(modified_dataset)
        new_clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, new_plate_layout)
        plate_layout_reactive$wells <- new_clickable_plate_layout
        DT::replaceData(proxy, plate_layout_reactive$wells)
    })


    ## The well selection grid (table)
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

    ## Downloadable csv of selected dataset
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

    ## generate heatmap of a selected variable from single-experiment growth params
    shiny::observeEvent(input$growth_params_plot_variable_single, {
        req(input$growth_params_plot_variable_single)
        message_helper("displaying growth param heatmap for", input$growth_params_plot_variable_single)
        isolate(output$growth_params_plot_single <- plotly::renderPlotly(
            plot_growth_statistics(growth_params_tibble = user_data$growth_params_single,
                                   variable = input$growth_params_plot_variable_single,
                                   do_scale = if (input$growth_params_plot_scaled_single == "yes") TRUE else FALSE)
        ))
    })

    ## Downloadable csv of statistics of selected dataset
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


