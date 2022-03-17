
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
    shiny::h4("Growth statistics"),
    shiny::downloadButton(outputId = "growth_statistics_single_download",
                                      label = "Download as csv"),
    DT::dataTableOutput(outputId = "growth_params_single")
)

single_growthcurve_box <- shinydashboard::box(
    style='padding:20px;width:1000px;overflow-x: scroll;height:800px;overflow-y: scroll;',
    shiny::radioButtons(inputId = "graph_type_single",
                      label =  "Graph type",
                      choices = c("ribbon", "average", "replicates"),
                      selected = "ribbon",
                      inline = TRUE),
    shiny::plotOutput(outputId = "growthcurve_plot_single",
                    width = "1000px", height="600px"),
    shiny::tags$div(id = "well_selector_div")
)

single_exp_tab <- shiny::fluidPage(
    shiny::h3("View and edit a single experiment"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 3, #total = 12
            shiny::h4("Make selections"),
                shinyWidgets::pickerInput(inputId = "experiment_date_single",
                                          label = "Experiment name",
                                          multiple = FALSE,
                                          choices = NULL,
                                          options = list(title = "Choose experiment")),
                shiny::actionButton(inputId = "show_graph_single",
                                    label = "Show graph",
                                    icon = shiny::icon("chart-area")),
                shiny::downloadButton(outputId = "data_download_single",
                                      label = "Download as csv")

        ),
        shiny::mainPanel(
            shiny::tabsetPanel(type = "tabs",
                               shiny::tabPanel("Growth curves", single_growthcurve_box),
                               shiny::tabPanel("Growth statistics", statistics_tab),
            )
        )
    )
)

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

shiny_app_ui <- function() {
    ui <- shiny::mainPanel(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Single experiment", single_exp_tab),
            shiny::tabPanel("Across experiments", "Compare across experiments")#multiple_exp_tab)
        )
    )
    return(ui)
}

