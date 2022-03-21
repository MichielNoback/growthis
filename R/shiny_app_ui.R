
#not exported ui functions


# growthcurve_tab <- shiny::fluidPage(
#     shinydashboard::box(style='padding:20px;width:1000px;overflow-x: scroll;height:800px;overflow-y: scroll;',
#         shiny::radioButtons(inputId = "graph_type",
#                     label =  "Graph type",
#                     choices = c("ribbon", "average", "replicates"),
#                     selected = "ribbon",
#                     inline = TRUE),
#         shiny::plotOutput(outputId = "growthcurve_plot",
#                           width = "1000px", height="600px"),
#         shiny::tags$div(id = "well_selector_div")
#     )
# )

statistics_tab <- shiny::fluidPage(
    shiny::actionButton(inputId = "show_statistics_single",
                        label = "Calculate statistics",
                        icon = shiny::icon("chart-area"),
                        width = 150),
    #shiny::h4("Growth statistics"),
    shiny::downloadButton(outputId = "growth_statistics_single_download",
                                      label = "Download as csv"),
    DT::dataTableOutput(outputId = "growth_params_single")
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
    style='padding:20px;width:1000px;overflow-x: scroll;', #height:800px;overflow-y: scroll;
    shiny::radioButtons(inputId = "graph_type_single",
                      label =  "Graph type",
                      choices = c("ribbon", "average", "replicates"),
                      selected = "ribbon",
                      inline = TRUE),
    shiny::plotOutput(outputId = "growthcurve_plot_single",
                    width = "1000px", height="500px"),
#    shinyjs::hidden(div(id = "well_selection_box_wrapper",
                        tags$style(HTML('.plate_layout th, .plate_layout td {padding: 4px !important; background-color: floralwhite !important;}')),
                        well_selection_box,
#)),
    shiny::tags$div(id = "well_selector_div")
)


#viewer and editor of single experiment
single_exp_tab <- shiny::fluidPage(
    shiny::h4("View and edit a single experiment"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 3, #total = 12
            #shiny::h5("Make selection of available experiments"),
            shinyWidgets::pickerInput(inputId = "experiment_date_single",
                                      label = "Use from available",
                                      width = 150,
                                      multiple = FALSE,
                                      choices = NULL,
                                      options = list(title = "Choose experiment")),

            #shiny::h5("Or upload from your hard drive"),
            # Input: Select a file ----
            shiny::fileInput("local_varioscan_excel", "Or upload from your hard drive",
                      multiple = FALSE,
                      accept = c("xls", "xlsx")),

            # shiny::actionButton(inputId = "show_graph_single",
            #                     label = "Show graph",
            #                     icon = shiny::icon("chart-area"),
            #                     width = 150),
            # shiny::actionButton(inputId = "overwrite_single_experiment",
            #                     label = "Overwrite",
            #                     icon = shiny::icon("save"),
            #                     width = 150),
            shiny::downloadButton(outputId = "data_download_single",
                                  label = "Download as csv",
                                  width = 150)

        ),
        shiny::mainPanel(
            shinyjs::useShinyjs(), #used for hinding the well selection box
            #single_growthcurve_box
            shiny::tabsetPanel(type = "tabs",
                               shiny::tabPanel("Growth curves", single_growthcurve_box),
                               shiny::tabPanel("Growth statistics", statistics_tab),
            )
        )
    )
)


shiny_app_ui <- function() {
    ui <- shiny::mainPanel(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("View & edit single", single_exp_tab),
            shiny::tabPanel("Analyse and compare", "Compare across experiments")#multiple_exp_tab)
        )
    )
    return(ui)
}


# single_exp_tab <- shiny::fluidPage(
#     sidebarLayout(
#         shiny::titlePanel("Single experiment")
        # shiny::sidebarPanel(
        #     width = 3, #total = 12
        #     shiny::h4("Make selections"),
        #     shinyWidgets::pickerInput(inputId = "experiment_dates",
        #                               label = "Experiment names",
        #                               multiple = TRUE,
        #                               choices = NULL,
        #                               options = list(title = "Choose experiments")),
        #     shiny::checkboxGroupInput(inputId = "extracts",
        #                               label = "Extracts",
        #                               choices = NULL),
        #     shiny::checkboxGroupInput(inputId = "strains",
        #                               label = "Strains",
        #                               choices = NULL),
        #     shiny::actionButton(inputId = "show_graph",
        #                         label = "Show graph",
        #                         icon = shiny::icon("chart-area")),
        #     shiny::downloadButton(outputId = "data_download",
        #                           label = "Download as csv")
        # ),
        # shinydashboard::box(style='padding:20px;width:1000px;overflow-x: scroll;height:800px;overflow-y: scroll;',
        #                     shiny::radioButtons(inputId = "graph_type",
        #                                         label =  "Graph type",
        #                                         choices = c("ribbon", "average", "replicates"),
        #                                         selected = "ribbon",
        #                                         inline = TRUE),
        #                     shiny::plotOutput(outputId = "growthcurve_plot",
        #                                       width = "1000px", height="600px"),
        #                     shiny::tags$div(id = "well_selector_div")
        # )
#     )
# )

#
# multiple_exp_tab <- function() {
#     shiny::sidebarLayout(
#         shiny::titlePanel("Compare across experiments"),
#         # shiny::sidebarPanel(
#         #     width = 3, #total = 12
#         #     shiny::h4("Make selections"),
#         #     shinyWidgets::pickerInput(inputId = "experiment_dates",
#         #                               label = "Experiment names",
#         #                               multiple = TRUE,
#         #                               choices = NULL,
#         #                               options = list(title = "Choose experiments")),
#         #     shiny::checkboxGroupInput(inputId = "extracts",
#         #                               label = "Extracts",
#         #                               choices = NULL),
#         #     shiny::checkboxGroupInput(inputId = "strains",
#         #                               label = "Strains",
#         #                               choices = NULL),
#         #     shiny::actionButton(inputId = "show_graph",
#         #                         label = "Show graph",
#         #                         icon = shiny::icon("chart-area")),
#         #     shiny::downloadButton(outputId = "data_download",
#         #                           label = "Download as csv")
#         # ),
#         # shinydashboard::box(style='padding:20px;width:1000px;overflow-x: scroll;height:800px;overflow-y: scroll;',
#         #                     shiny::radioButtons(inputId = "graph_type",
#         #                                         label =  "Graph type",
#         #                                         choices = c("ribbon", "average", "replicates"),
#         #                                         selected = "ribbon",
#         #                                         inline = TRUE),
#         #                     shiny::plotOutput(outputId = "growthcurve_plot",
#         #                                       width = "1000px", height="600px"),
#         #                     shiny::tags$div(id = "well_selector_div")
#         # )
#     )
#
# }
#
