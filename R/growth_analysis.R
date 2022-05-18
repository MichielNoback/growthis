#' Perform growth analysis on a varioscan data set
#'
#' @param varioscan the varioscan data in long format
#'
#' @export
#'
do_growth_analysis <- function(varioscan) {
    wide_format <- create_wide_data(varioscan)

    growth_params <- determine_growth_params(wide_format)

    return(growth_params)
}

#' Uses `growthcurver::SummarizeGrowth` to determine growth
#'
determine_growth_params <- function(wide_format_data) {
    time_vec <- wide_format_data$time

    fit_fun <- function(x) {
        fit_data <- growthcurver::SummarizeGrowth(time_vec, x)
        extracted_data <- c(
            "AUC_l" = fit_data$vals$auc_l,
            "AUC_e" = fit_data$vals$auc_e,
            "AUC_le_rat" = (fit_data$vals$auc_l / fit_data$vals$auc_e),
            "K" = fit_data$vals$k,
            "N0" = fit_data$vals$n0,
            "DT" = fit_data$vals$t_gen,
            "r" = fit_data$vals$r,
            "T_gen" = fit_data$vals$t_gen,
            "T_mid" = fit_data$vals$t_mid,
            "sigma" = fit_data$vals$sigma,
            "df" = fit_data$vals$df
        )
        return(extracted_data)
    }


    fitted_models <- sapply(wide_format_data[, -1], fit_fun)

    yield <- determine_yield(wide_format_data)
    fitted_models <- rbind(fitted_models, yield)

    #print(fitted_models)

    fitted_models <- t(fitted_models)


    row_names <- rownames(fitted_models)
    #print(row_names)
    fitted_models <- dplyr::as_tibble(fitted_models) %>%
        dplyr::mutate(ID = row_names) %>%
        dplyr::select(ID, everything()) %>%
        tidyr::separate(col = ID,
                        into = c("date_started", "series", "dilution", "replicate"),
                        sep = "_")

    return(fitted_models)
}

determine_yield <- function(wide_format) {
    yields <- apply(X = wide_format[, -1],
                    MARGIN = 2,
                    FUN = function(x){c("yield" = max(x) - x[1])})
    yields
}


#' Pivot wider and mutate to match requirements of the `growthcurver` package
#'
#' This function will export the background_corrected data only.
#'
#' @param varioscan the varioscan data in long format
#'
#' @export
#'
create_wide_data <- function(varioscan) {
    wide_format <- varioscan %>%
    dplyr::filter(replicate %in% c("1C", "2C", "3C")) %>%
    dplyr::mutate(ID = paste(start_date, series, dilution, replicate, sep = "_"),
                  time = as.numeric(duration)) %>%
    dplyr::select(time, ID, OD) %>%
    tidyr::pivot_wider(names_from = ID, values_from = OD)
    return(wide_format)
}

