library(shinyWidgets)
library(shiny)
library(DT)

ui <- shiny::fluidPage(
    shinydashboard::box(id = "well_selection_div",
               style='width:900px;overflow-x: scroll;overflow-y: scroll;',
    DT::dataTableOutput("well_selection"),
    shinyWidgets::actionBttn(inputId = "remove_wells_button",
                             label = "Remove selection",
                             style = "pill",
                             icon = icon("warning-sign", lib = "glyphicon"),
                             color = "danger")
        #    shiny::actionButton("remove_wells_button", "Remove selection", icon = icon("warning-sign", lib = "glyphicon"))

    )
)

server <- function(input, output, session) {
    #shiny:: remove_wells_button

    #The proxy to update the DT
    proxy <- DT::dataTableProxy('well_selection')

    #The glyphs to represent the wells
    ok_glyph <- as.character(icon("ok-circle", lib = "glyphicon"))
    remove_glyph <- as.character(icon("remove-sign", lib = "glyphicon"))

    #The initial data for the wells_df, and
    dilutions <- c(0.02, 0.01, 0.005, 0.0025, 0.0013, 0.0006, 0.0003, 0.0)
    wells_df = data.frame(row.names = paste0(LETTERS[1:8], " (", dilutions, ")"))
    #refs_df <-
    replicates <- c(1:3, "C")

    for (plate_col in 1:12) {
        series <- ceiling(plate_col / 4)
        repl <- replicates[((plate_col-1) %% 4) + 1]
        #colname <- paste0(plate_col, "_", "Exp", series, ".", repl)
        colname <- paste0(series, ".", repl)
        wells_df[, colname] <- rep(ok_glyph, 8)
    }

    #The reactive version of the data
    wells_reactive = shiny::reactiveValues(wells = wells_df)

    #Update the table when clicked
    shiny::observeEvent(shiny::req(input$well_selection_cells_selected), {
        wells_reactive$wells[input$well_selection_cells_selected] =
            ifelse(wells_reactive$wells[input$well_selection_cells_selected] == ok_glyph,
                    remove_glyph, ok_glyph)

        #Send proxy (no need to refresh whole table)
        DT::replaceData(proxy, wells_reactive$wells)
    })

    #The "checkbox" table
    output$well_selection = DT::renderDataTable(
        wells_df,
        selection = list(mode = "single", target = 'cell'),
        options = list(
            columnDefs = list(list(className = 'dt-center', width = '10px', targets = "_all")),
            dom = "t",
            ordering = F
        ),
       class = "compact",
       escape = F,
       width = "100",
       fillContainer = FALSE
    )

}

shinyApp(ui, server)




