
dependent_var <- "AUC_l"
growth_params <- do_growth_analysis(growthis::`12-11-2021`)
model_data <- suppressMessages(model_dose_response(growth_params = growth_params,
                                      data_key = data_key))

plot_dependent_var_over_concentration(growth_params_tibble = growth_params,
                                      all_model_data = model_data,
                                      dependent_var = dependent_var)




growth_params_multi_with_key <- tidyr::unite(data = growth_params_multi,
                                    col = "date_series",
                                    date_started, series,
                                    sep = " ",
                                    remove = FALSE)


data_key <- "date_series"
dependent_var <- "AUC_l"
all_model_data_multi <- model_dose_response(growth_params = growth_params_multi_with_key,
                                      dependent_var = dependent_var,
                                      data_key = data_key)

#fitted_data <- all_model_data_multi$fitted_data

plot_dependent_var_over_concentration(growth_params_tibble = growth_params_multi_with_key,
                                      all_model_data = all_model_data_multi,
                                      data_key = "date_series",
                                      dependent_var = dependent_var)




ggplot(data = growth_params_tibble,
       mapping = aes_string(x = "dilution", y = dependent_var, color = data_key)) +
    geom_line(data = fitted_data,
              mapping = aes_string(x = "dilution", y = "predicted", color = data_key), size = 1) +
    geom_point(mapping = aes(shape = replicate), alpha = 0.7, size = 3) +
    scale_x_sqrt() +
    theme_minimal(base_size = 18)

+ #"series"
    # geom_vline(mapping = aes_string(xintercept = "dilution", color = data_key),
    #            linetype = "dotted",
    #            data = IC50_IC90_data,
    #            size = 1) +
    # geom_label(data = IC50_IC90_data,
    #            mapping = aes_string(x = "dilution",
    #                                 y = "y_pos",
    #                                 color = data_key,
    #                                 label = "label"),
    #
    #            hjust = "left",
    #            size = 6) +
    # geom_line(data = fitted_data,
    #           mapping = aes_string(x = "dilution", y = "predicted", color = data_key), size = 1) +
    # geom_point(mapping = aes(shape = replicate), alpha = 0.7, size = 3) +
    scale_x_sqrt() +
    theme_minimal(base_size = 18)
