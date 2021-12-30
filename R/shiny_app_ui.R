#not exported ui function
shiny_app_ui <- function() {
    ui <- shiny::fluidPage(
        shiny::titlePanel("Varioscan growth analysis"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 2, #total = 12
                shiny::dateRangeInput(inputId = "date_range",
                                      label = "Date range"),
                shiny::checkboxGroupInput(inputId = "extracts",
                                          label = "Extracts",
                                          choices = NULL),
                shiny::checkboxGroupInput(inputId = "strains",
                                          label = "Strains",
                                          choices = NULL),
                shiny::radioButtons(inputId = "graph_type",
                                    label =  "Graph type",
                                    choices = c("ribbon", "average", "replicates"),
                                    selected = "ribbon"),
                shiny::actionButton(inputId = "show_graph",
                                    label = "Show graph"),
            ),

            shiny::mainPanel(
                shiny::tabsetPanel(type = "tabs",
                            shiny::tabPanel("Data", shiny::verbatimTextOutput("Data")),
                            shiny::tabPanel("Filter", shiny::verbatimTextOutput("Filter")),
                            shiny::tabPanel("View & Analyse", shiny::verbatimTextOutput("View"))
                )
            )
        )
    )
    return(ui)
}
