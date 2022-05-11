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
    if(! variable_name %in%names(growth_params_tibble)) {
        stop(paste0("no such variable in the given tibble: ", variable_name));
    }

    ## when this print is removed, I get the error
    ## Warning: Error in local_error_context: promise already under evaluation: recursive default argument reference or earlier problems?
    ## 123: local_error_context
    ## 122: <Anonymous>
    ##
    #print(variable_name)
    ##
    ## For that reason I included the above if() clause which will never rstop in the context of this app

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
