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



# plot_yield_over_concentration <- function(growth_params_tibble,
#                                           exp = "all") {
#     #Plot van yield (y) over Concentration (X), met daarin (optioneel) de IC50/IC90.
#     growth_params_tibble <- growth_params_tibble %>%
#         mutate(dilution = as.numeric(dilution),
#                yield2 = K - N0)
#     if(exp != "all") {
#         growth_params_tibble <- growth_params_tibble %>%
#             filter(series %in% exp)
#     }
#
#     #growth_params_tibble <- remove_extreme_yield_outliers(growth_params_tibble)
#
#     all_model_data <- model_dose_response(growth_params_tibble)
#
#     fitted_data <- all_model_data$fitted_data
#
#     max_y <- max(growth_params_tibble$yield)
#     relative_y_positions <- c(1, 0.92, 0.84, 0.76, 0.68, 0.6)[1 : (2 * length(unique(growth_params_tibble$series)))]
#     #print(relative_y_positions)
#     IC50_IC90_data <- all_model_data$IC50_IC90 %>%
#         tidyr::pivot_longer(cols = 2:3,
#                             names_to = "criterion",
#                             values_to = "dilution") %>%
#         dplyr::arrange(dilution) %>%
#         dplyr::mutate(label = paste0(criterion, "=", dilution),
#                       y_pos = relative_y_positions * max_y)
#     #print(IC50_IC90_data)
#     #print(model_data)
#
#     p <- ggplot(data = growth_params_tibble,
#                 mapping = aes(x = dilution, y = yield, color = series)) +
#         # {if(model == "nls") geom_smooth(method = "nls", se = FALSE,
#         #             formula = y ~ a * exp(r * x),
#         #             method.args = list(start = c(a = 10, r = -0.01)),
#         #             size = 0.5)} +
#         # {if(model == "glm") geom_smooth(method = "glm",
#         #             method.args = list(family = "binomial"),
#         #             se = FALSE,
#         #             size = 0.5)} +
#         geom_vline(mapping = aes(xintercept = dilution, color = series),
#                    linetype = "dotted",
#                    data = IC50_IC90_data,
#                    size = 1) +
#         geom_label(data = IC50_IC90_data,
#                    mapping = aes(x = dilution,
#                                  y = y_pos,
#                                  color = series,
#                                  label = label),
#
#                    hjust = "left",
#                    size = 6) +
#         geom_line(data = fitted_data,
#                   mapping = aes(x = dilution, y = predicted, color = series), size = 1) +
#         geom_point(mapping = aes(shape = replicate), alpha = 0.7, size = 3) +
#         scale_x_log10() +
#         theme_minimal(base_size = 18)
#
#     return(p)
# }


#' Plots one of a few selected parameters over concentration
#'
#' Creates a scatter plot of the selected growth parameter over concentration (dilution) for all replicates.
#'
#' @param growth_params_tibble a tibble containing results of growth statistics analysis.
#' @param all_model_data a list of models and fitted data (result of call to `model_dose_response`)
#' @param dependent_var the dependent variable to plot on the y axis Can be one of "AUC_l", "AUC_e", "yield"
#' @param exp the experiment (series) to plot
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
plot_dependent_var_over_concentration <- function(growth_params_tibble,
                                                  all_model_data,
                                                  dependent_var = "yield",
                                                  exp = "all") {
    #Plot van yield (y) over Concentration (X), met daarin (optioneel) de IC50/IC90.

    #all_model_data <- model_dose_response(growth_params_tibble, dependent_var)

    fitted_data <- all_model_data$fitted_data

    max_y <- max(growth_params_tibble[, dependent_var])
    relative_y_positions <- c(1, 0.92, 0.84, 0.76, 0.68, 0.6)[1 : (2 * length(unique(growth_params_tibble$series)))]
    #print(relative_y_positions)
    IC50_IC90_data <- all_model_data$IC50_IC90 %>%
        tidyr::pivot_longer(cols = 2:3,
                            names_to = "criterion",
                            values_to = "dilution") %>%
        dplyr::arrange(dilution) %>%
        dplyr::mutate(label = paste0(criterion, "=", dilution),
                      y_pos = relative_y_positions * max_y)
    #print(IC50_IC90_data)
    #print(model_data)

    p <- ggplot(data = growth_params_tibble,
                mapping = aes_string(x = "dilution", y = dependent_var, color = "series")) +
        geom_vline(mapping = aes(xintercept = dilution, color = series),
                   linetype = "dotted",
                   data = IC50_IC90_data,
                   size = 1) +
        geom_label(data = IC50_IC90_data,
                   mapping = aes(x = dilution,
                                 y = y_pos,
                                 color = series,
                                 label = label),

                   hjust = "left",
                   size = 6) +
        geom_line(data = fitted_data,
                  mapping = aes(x = dilution, y = predicted, color = series), size = 1) +
        geom_point(mapping = aes(shape = replicate), alpha = 0.7, size = 3) +
        scale_x_sqrt() +
        theme_minimal(base_size = 18)

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
