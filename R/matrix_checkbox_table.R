library(shinyWidgets)
library(shiny)
library(DT)

load_test_dataset <- function() {
    start_date_of_exp <<- "2-3-2020"
    # current_dataset <<- load_selected_experiment(start_date_of_exp)

    #or, with removed wells:
    current_dataset <<- get(load(paste0(here::here(), "/data-raw/dataset_with_excluded.rda")))
}

well_selection_box <- shinydashboard::box(id = "well_selection_div",
                        #style='width:600px;overflow-x: scroll;overflow-y: scroll;',
                        DT::dataTableOutput("well_selection"),
                        br(),
                        shinyWidgets::actionBttn(inputId = "remove_wells_button",
                                                 label = "Remove selection",
                                                 style = "fill",
                                                 icon = icon("warning-sign", lib = "glyphicon"),
                                                 color = "danger")
)

get_ui_glyphs <- function() {
    #The glyphs to represent the wells
    ok_glyph <- as.character(icon("ok-circle", lib = "glyphicon", style="font-size:1.4em"))
    remove_glyph <- as.character(icon("remove-sign", lib = "glyphicon", style="color:darkred;font-size:1.4em"))
    excluded_glyph <- as.character(icon("ban-circle", lib = "glyphicon", style="color:darkgrey;font-size:1.4em"))

    list("ok_glyph" = ok_glyph,
         "remove_glyph" = remove_glyph,
         "excluded_glyph" = excluded_glyph)
}



#' returns the plate layout belonging to the given experiment
get_plate_layout <- function(selected_experiment) {
    plate_layout <- selected_experiment %>%
        dplyr::select(dilution, series, replicate, OD, duration) %>%
        dplyr::filter(replicate %in% c("1", "2", "3", "C")) %>%
        dplyr::group_by(dilution, series, replicate) %>%
        dplyr::summarize(excluded = any(is.na(OD)), .groups = "drop") %>%
        #dplyr::ungroup() %>%
        dplyr::mutate(series_repl = paste0(series, "_", replicate)) %>%
        dplyr::select(dilution, series_repl, excluded) %>%
        dplyr::arrange(dplyr::desc(dilution)) %>%
        tidyr::pivot_wider(names_from = series_repl, values_from = excluded)

    plate_layout
}

#' Adds glyphicons to a given plate layout
get_clickable_plate_layout <- function(ui_glyphs, plate_layout) {
    plate_layout <- plate_layout %>%
        dplyr::mutate(dplyr::across(dplyr::matches("Exp"), function(x) ifelse(x, ui_glyphs$excluded_glyph, ui_glyphs$ok_glyph)))
    return(plate_layout)
}


get_selected_wells <- function(wells, start_date_of_exp) {
    tbbl_selection <- wells %>%
        tidyr::pivot_longer(cols = -1, names_to = "exp", values_to = "glyph") %>%
        dplyr::filter(grepl(pattern = "remove-sign", glyph)) %>%
        tidyr::separate(col = exp, into = c("series", "replicate")) %>%
        dplyr::select(-glyph)

    list_selection <- apply(X = tbbl_selection,
                      MARGIN = 1,
                      FUN = function(x) list(
                          start_date = start_date_of_exp,
                          dilution = as.numeric(x["dilution"]),
                          series = x["series"],
                          replicate = x["replicate"]))

    list_selection
}

#returns new glyph depending on what was clicked
get_new_glyph <- function(clicked_glyph, ui_glyphs) {
    if(clicked_glyph == ui_glyphs$excluded_glyph) return(ui_glyphs$excluded_glyph)
    else if (clicked_glyph == ui_glyphs$ok_glyph) return(ui_glyphs$remove_glyph)
    return(ui_glyphs$ok_glyph)
}

ui <- shiny::fluidPage(
    #theme = "style.css",
    tags$style(HTML('.plate_layout th, .plate_layout td {padding: 4px !important; background-color: floralwhite !important;}')),
    well_selection_box
)

server <- function(input, output, session) {
    load_test_dataset()
    ui_glyphs <- get_ui_glyphs()
    plate_layout <- get_plate_layout(current_dataset)
    clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, plate_layout)
    #print(clickable_plate_layout)

    #The reactive version of the data
    plate_layout_reactive = shiny::reactiveValues(
        wells = clickable_plate_layout
    )

    #The proxy to update the DT
    proxy <- DT::dataTableProxy(outputId = 'well_selection')


    #Update the table when clicked
    shiny::observeEvent(shiny::req(input$well_selection_cells_selected), {


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

        #first locate selected wells
        selected_wells <- get_selected_wells(plate_layout_reactive$wells, start_date_of_exp)
        modified_dataset <- filter_data(current_dataset, exclude_wells = selected_wells)
        new_plate_layout <- get_plate_layout(modified_dataset)
        new_clickable_plate_layout <- get_clickable_plate_layout(ui_glyphs, new_plate_layout)

        plate_layout_reactive$wells <- new_clickable_plate_layout
        DT::replaceData(proxy, plate_layout_reactive$wells)
    })


    #The "checkbox" table
    output$well_selection = DT::renderDataTable(
        clickable_plate_layout,
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
}

shinyApp(ui, server)




