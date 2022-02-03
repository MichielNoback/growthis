
#not exported ui functions


growthcurve_tab <- shiny::fluidPage(
    shinydashboard::box(style='padding:20px;width:1000px;overflow-x: scroll;height:800px;overflow-y: scroll;',
#        shiny::fluidRow(
            shiny::radioButtons(inputId = "graph_type",
                        label =  "Graph type",
                        choices = c("ribbon", "average", "replicates"),
                        selected = "ribbon",
                        inline = TRUE),
            shiny::plotOutput(outputId = "growthcurve_plot",
                              width = "1000px", height="600px"),
            shiny::tags$div(id = "well_selector_div")
#        )
    )
)

statistics_tab <- shiny::fluidPage(
    shiny::h4("Growth statistics"),
    shiny::downloadButton(outputId = "growth_statistics_download",
                                      label = "Download as csv"),
    DT::dataTableOutput(outputId = "growth_params")
)

shiny_app_ui <- function() {
    ui <- shiny::fluidPage(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 3, #total = 12
                shiny::h4("Make selections"),
                shinyWidgets::pickerInput(inputId = "experiment_dates",
                                   label = "Experiment names",
                                   multiple = TRUE,
                                   choices = NULL,
                                   options = list(title = "Choose experiments")),
                shiny::checkboxGroupInput(inputId = "extracts",
                                          label = "Extracts",
                                          choices = NULL),
                shiny::checkboxGroupInput(inputId = "strains",
                                          label = "Strains",
                                          choices = NULL),
                shiny::actionButton(inputId = "show_graph",
                        label = "Show graph",
                        icon = shiny::icon("chart-area")),
                shiny::downloadButton(outputId = "data_download",
                                      label = "Download as csv")
            ),

            shiny::mainPanel(
                shiny::tabsetPanel(type = "tabs",
                                   shiny::tabPanel("Growth curves", growthcurve_tab),
                                   shiny::tabPanel("Growth statistics", statistics_tab)
                )
            )
        )
    )
    return(ui)
}

