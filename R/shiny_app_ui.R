
#not exported ui functions


growthcurve_tab <- shiny::fluidPage(
    shiny::fluidRow(
        shiny::radioButtons(inputId = "graph_type",
                    label =  "Graph type",
                    choices = c("ribbon", "average", "replicates"),
                    selected = "ribbon",
                    inline = TRUE),

        shiny::plotOutput(outputId = "growthcurve_plot")
    )
)

shiny_app_ui <- function() {
    ui <- shiny::fluidPage(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 3, #total = 12
                shiny::h4("Make selections"),
                shiny::dateRangeInput(inputId = "date_range",
                                      label = "Date range",
                                      start = "2020-01-01",
                                      startview = "year"),
                shiny::checkboxGroupInput(inputId = "extracts",
                                          label = "Extracts",
                                          choices = NULL),
                shiny::checkboxGroupInput(inputId = "strains",
                                          label = "Strains",
                                          choices = NULL),
                shiny::actionButton(inputId = "show_graph",
                                    label = "Show graph"),
            ),

            shiny::mainPanel(
                shiny::tabsetPanel(type = "tabs",
                            shiny::tabPanel("Growth curves", growthcurve_tab),
                            # shiny::tabPanel("Filter", shiny::verbatimTextOutput("Filter")),
                            # shiny::tabPanel("View & Analyse", shiny::verbatimTextOutput("View"))
                )
            )
        )
    )
    return(ui)
}

