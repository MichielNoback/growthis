#' Create a heatmap of growth statistics
#'
#' Creates a heatmap based on a selected variable of a growth statistics dataset
#'
#' @param growth_params_tibble a tibble containing results of growth statistics analysis.
#' @param variable the variable to create a heatmap for
#' @param do_scale a logical indicating wheter the data should be scaled
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
plot_growth_statistics <- function(growth_params_tibble, variable_name, do_scale = FALSE) {
    # print(growth_params_tibble)
    # print("--->>>")
    # print(variable_name)

    selection <- growth_params_tibble %>%
        mutate(series_repl = paste0(series, "_", replicate),
                      dilution = as.numeric(dilution),
                      {{ variable_name }} := if(do_scale) scale(.data[[variable_name]])[,1] else .data[[variable_name]],
                      #tmp = .data[[variable_name]], #{{variable_name}},
                      text = paste0(variable_name, "=", round(.data[[variable_name]], 6),
                                    "\nseries =", series,
                                    "\nreplicate = ", replicate)) %>%
        arrange(dplyr::desc(dilution)) %>%
        mutate(dilution = factor(dilution)) %>%
        select(series_repl, dilution, {{variable_name}}, text) #%>%

    p <- ggplot(data = selection,
                 mapping = aes_string(x = "series_repl",
                                      y = "dilution",
                                      fill = variable_name,
                                      text = "text")) +
        geom_tile() +
        scale_fill_gradient2(low = "blue",
                            mid = "white",
                            high = "red") +
        xlab("Series / replicate") +
        theme_minimal()
    plotly::ggplotly(p, tooltip="text")
}

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyHeatmap.html"))
