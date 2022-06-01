
#not exported ui variables and functions

single_statistics_tab <- shiny::fluidPage(
    # shiny::actionButton(inputId = "show_statistics_single",
    #                     label = "Calculate statistics",
    #                     icon = shiny::icon("chart-area"),
    #                     width = 150),
    br(),
    shiny::downloadButton(outputId = "growth_statistics_single_download",
                                      label = "Download as csv"),
    DT::dataTableOutput(outputId = "growth_params_single"),
    br(),
    shiny::h4("Visualize"),
    shiny::radioButtons(inputId = "growth_params_plot_scaled_single",
                                   label =  "Scale data",
                                   choices = c("yes", "no"),
                                   selected = "no",
                                   inline = TRUE),
    shiny::selectInput(inputId = "growth_params_plot_variable_single",
                       label = "Select a variable",
                       choices = NULL),
    plotly::plotlyOutput("growth_params_plot_single")
)


single_statistics_analysis_tab <- shiny::fluidPage(
    shiny::h4("Visualize"),
    shiny::div(style="display: inline-block;vertical-align:top; width: 200px;",
               shiny::selectInput(inputId = "model_plot_dependent_var_selection_single",
                                  label = "Variable to model on",
                                  choices = c("AUC_l", "AUC_e", "yield"),
                                  selected = "AUC_l")),

    shiny::div(style="display: inline-block;vertical-align:top; width: 200px;",
               shiny::selectInput(inputId = "model_plot_exp_selection_single",
                       label = "Select dataseries",
                       choices = c("all", "Exp1", "Exp2", "Exp3"),
                       selected = "all")),
    shiny::br(),

    shiny::div(style="display: inline-block;vertical-align:top;width:900px;!important",
               shiny::plotOutput("yield_over_concentration_plot_single", width = "900px", height="600px")),
#    plotly::plotlyOutput("yield_over_concentration_plot_single", width = "1000px", height="700px")

    shiny::div(style="display: inline-block;vertical-align:top; width: 500px;",
           shiny::verbatimTextOutput(outputId = "model_info_single"))
)


multi_statistics_tab <- shiny::fluidPage(
    # shiny::actionButton(inputId = "show_statistics_multiple",
    #                     label = "Calculate statistics",
    #                     icon = shiny::icon("chart-area"),
    #                     width = 150),
    shiny::downloadButton(outputId = "growth_statistics_multiple_download",
                          label = "Download as csv"),
    DT::dataTableOutput(outputId = "growth_params_multiple")
)

multi_statistics_analysis_tab <- shiny::fluidPage(
    shiny::h4("Visualize"),
    shiny::div(style="display: inline-block;vertical-align:top; width: 200px;",
               shiny::selectInput(inputId = "model_plot_dependent_var_selection_multi",
                                  label = "Variable to model on",
                                  choices = c("AUC_l", "AUC_e", "yield"),
                                  selected = "AUC_l")),
    shiny::br(),

    shiny::div(style="display: inline-block;vertical-align:top;width:900px;!important",
               shiny::plotOutput("yield_over_concentration_plot_multi", width = "900px", height="600px")),
    #    plotly::plotlyOutput("yield_over_concentration_plot_single", width = "1000px", height="700px")

    shiny::div(style="display: inline-block;vertical-align:top; width: 500px;",
               shiny::verbatimTextOutput(outputId = "model_info_multi"))
)

#clickable grid for selection of wells in the 96 wells layout
well_selection_box <- shinydashboard::box(
    title = "Well selection",
    id = "well_selection_box",
            #style='width:600px;overflow-x: scroll;overflow-y: scroll;',
            DT::dataTableOutput("well_selection"),
            br(),
            shinyWidgets::actionBttn(inputId = "remove_wells_button",
                                     label = "Remove selection",
                                     style = "fill",
                                     icon = icon("warning-sign", lib = "glyphicon"),
                                     color = "danger")

)


single_growthcurve_box <- shinydashboard::box(
    style='padding:20px;width:1100px;overflow-x: scroll;', #height:800px;overflow-y: scroll;
    shiny::radioButtons(inputId = "graph_type_single",
                      label =  "Graph type",
                      choices = c("ribbon", "average", "replicates"),
                      selected = "ribbon",
                      inline = TRUE),
    shiny::plotOutput(outputId = "growthcurve_plot_single",
                    width = "1100px", height="700px"),
#    shinyjs::hidden(div(id = "well_selection_box_wrapper",
                        tags$style(HTML('.plate_layout th, .plate_layout td {padding: 4px !important; background-color: floralwhite !important;}')),
                        well_selection_box,
#)),
    shiny::tags$div(id = "well_selector_div")
)

multi_growthcurve_box <- shinydashboard::box(
    style='padding:20px;width:1000px;overflow-x: scroll;height:750px;overflow-y:scroll;',
    shiny::radioButtons(inputId = "graph_type_multiple",
                        label =  "Graph type",
                        choices = c("ribbon", "average", "replicates"),
                        selected = "ribbon",
                        inline = TRUE),
    shiny::plotOutput(outputId = "growthcurve_plot_multiple",
                      width = "1000px", height="1200px"),
    br()
)



#viewer and editor of single experiment
single_exp_tab <- shiny::fluidPage(
    shiny::h4("View and edit a single experiment"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 3, #total = 12
            shinyWidgets::pickerInput(inputId = "experiment_date_single",
                                      label = "Use from available",
                                      width = 150,
                                      multiple = FALSE,
                                      choices = NULL,
                                      options = list(title = "Choose experiment")),

            shiny::fileInput("local_varioscan_excel", "Or upload from your hard drive",
                      multiple = FALSE,
                      accept = c("xls", "xlsx")),

            shiny::downloadButton(outputId = "data_download_single",
                                  label = "Download as csv",
                                  width = 150)
        ),
        shiny::mainPanel(
            #shinyjs::useShinyjs(), #used for hiding the well selection box
            #single_growthcurve_box
            shiny::tabsetPanel(type = "tabs",
                               shiny::tabPanel("Growth curves", single_growthcurve_box),
                               shiny::tabPanel("Growth statistics", single_statistics_tab),
                               shiny::tabPanel("Growth statistics analysis", single_statistics_analysis_tab),
            )
        )
    )
)




## Viewer for multiple experiments
multi_exp_tab <- shiny::fluidPage(
    shiny::h4("View and compare experiments"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 3, #total = 12
            shinyWidgets::pickerInput(
                inputId = "experiment_date_multiple",
                label = "Use from available (first)",
                width = 200,
                multiple = TRUE,
                choices = NULL,
                options = list(title = "Choose experiments")),

            shiny::fileInput(inputId = "local_varioscan_excel_multi",
                             label = "and/or upload from your hard drive",
                             multiple = TRUE,
                             accept = c("xls", "xlsx")),

            shinyWidgets::awesomeCheckboxGroup(
                inputId = "extracts_multiple",
                label = "Extracts",
                inline = TRUE,
                status = "info",
                choices = NULL
            ),

            shinyWidgets::awesomeCheckboxGroup(
                inputId = "strains_multiple",
                label = "Strains",
                inline = TRUE,
                status = "info",
                choices = NULL
            ),

            shiny::actionButton(inputId = "show_graph_multiple",
                                label = "Show graph",
                                icon = shiny::icon("chart-area"),
                                width = 150),
            shiny::downloadButton(outputId = "data_download_multiple",
                                  label = "Download as csv",
                                  width = 150)
        ),
        shiny::mainPanel(
            shiny::tabsetPanel(type = "tabs",
                               shiny::tabPanel("Growth curves", multi_growthcurve_box),
                               shiny::tabPanel("Growth statistics", multi_statistics_tab),
            )
        )
    )
)


#' The main UI function
#' @export
shiny_app_ui <- function() {
    ui <- shiny::mainPanel(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("View & edit single", single_exp_tab),
            shiny::tabPanel("Analyse and compare", multi_exp_tab),
        )
    )
    return(ui)
}
