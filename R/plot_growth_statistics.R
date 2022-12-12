#' Create a heatmap of growth statistics
#'
#' Creates a heatmap based on a selected variable of a growth statistics dataset
#'
#' @param growth_params_tibble a tibble containing results of growth statistics analysis.
#' @param variable_name the variable to create a heatmap for
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


#' Plots one of a few selected parameters over concentration
#'
#' Creates a scatter plot of the selected growth parameter over concentration (dilution) for all replicates.
#'
#' @param growth_params_tibble a tibble containing results of growth statistics analysis.
#' @param all_model_data a list of models and fitted data (result of call to `model_dose_response`)
#' @param dependent_var the dependent variable to plot on the y axis Can be one of "AUC_l", "AUC_e", "yield"
#' @param data_key the variable to split out on usng color. Defaults to "series".
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
                                                  data_key = "series",
                                                  exp = "all") {
    #Plot van yield (y) over Concentration (X), met daarin (optioneel) de IC50/IC90.

    #all_model_data <- model_dose_response(growth_params_tibble, dependent_var)
    growth_params_tibble <- mutate(growth_params_tibble, dilution = as.numeric(dilution))
    fitted_data <- all_model_data$fitted_data
    max_y <- max(growth_params_tibble[, dependent_var])
    possible_y_positions <- seq(1, 0, by = -0.06)
    relative_y_positions <- possible_y_positions[1 : (2 * length(unique(all_model_data$IC50_IC90[, data_key])))]
    #print(relative_y_positions)
    IC50_IC90_data <- all_model_data$IC50_IC90 %>%
        tidyr::pivot_longer(cols = c(IC50, IC90),
                            names_to = "criterion",
                            values_to = "dilution") %>%
        dplyr::arrange(dilution) %>%
        dplyr::mutate(label = paste0(criterion, "=", dilution),
                      y_pos = relative_y_positions * max_y)

    #print(IC50_IC90_data)
    #print(data_key)
    #print(growth_params_tibble)
    #print(sort(unique(growth_params_tibble$dilution)))
    p <- ggplot(data = growth_params_tibble,
                mapping = aes_string(x = "dilution", y = dependent_var, color = data_key)) + #"series"
        geom_vline(mapping = aes_string(xintercept = "dilution", color = data_key),
                   linetype = "dotted",
                   data = IC50_IC90_data,
                   size = 1) +
        geom_label(data = IC50_IC90_data,
                   mapping = aes_string(x = "dilution",
                                 y = "y_pos",
                                 color = data_key,
                                 label = "label"),

                   hjust = "left",
                   size = 6) +
        geom_line(data = fitted_data,
                  mapping = aes_string(x = "dilution", y = "predicted", color = data_key), size = 1) +
        geom_point(mapping = aes(shape = replicate), alpha = 0.7, size = 3) +
        scale_x_sqrt(breaks = unique(growth_params_tibble$dilution),
                           guide = guide_axis(angle = 90)) +
        theme(axis.text.x = element_text(angle = 90)) +
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
