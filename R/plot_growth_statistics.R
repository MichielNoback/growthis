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

    ## when this print is removed, I get the following error
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


#' Scatter plot of yield over concentration
#'
#' Creates a scatter plot of yield over concentration (dilution) for all replicates.
#'
#' @param growth_params_tibble a tibble containing results of growth statistics analysis.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
plot_yield_over_concentration <- function(growth_params_tibble, exp = "all") {
    #Plot van yield (y) over Concentration (X), met daarin (optioneel) de IC50/IC90.

    growth_params_tibble <- growth_params_tibble %>%
        mutate(dilution = as.numeric(dilution),
               yield2 = K - N0)
    if(exp != "all") {
        growth_params_tibble <- growth_params_tibble %>%
            filter(series %in% exp)
    }

    growth_params_tibble <- remove_extreme_yield_outliers(growth_params_tibble)

    print(growth_params_tibble)

    p <- ggplot(data = growth_params_tibble,
                mapping = aes(x = dilution, y = yield, color = series)) +
        geom_smooth(method = "loess", se = F) + #, span = 0.5
        geom_point(alpha = 0.5) +
        theme_minimal()
    #print(p)
    return(p)
}


#' removes extreme outliers with respect to yield
#' because of plot corruption
remove_extreme_yield_outliers <- function(growth_params_tibble, z_score_cutoff = 3) {
    growth_params_tibble %>%
        mutate(z_score = abs(yield2 - mean(yield2)) / sd(yield2)) %>%
        filter(z_score < z_score_cutoff)

#    as.data.frame(sapply(data, function(data) (abs(data-mean(data))/sd(data))))
}
