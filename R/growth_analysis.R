#' Perform growth analysis on a varioscan data set
#'
#' @param varioscan the varioscan data in long format
#'
#' @details
#' Most data comes from the growthcurver package. Only `yield` is added here.
#'
#' The population size at the beginning of the growth curve is given by `N0`.
#' The maximum possible population size in a particular environment, or the
#' carrying capacity, is given by `K`. The intrinsic growth rate of the population,
#' `r`, is the growth rate that would occur if there were no restrictions imposed on
#' total population size.
#'
#' The most useful values are `k`, `n0`, and `r`, which are the values of the parameters for the logistic
#' equation that best fit the data. The fitting algorithm provides a measure of uncertainty for each,
#' which is available (for n) in the n_p and n_se values, for example. The values `sigma` and `df` are
#' both determined during the nonlinear regression fit. Df is the degrees of freedom and sigma is a
#' measure of the goodnesss of fit of the parameters of the logistic equation for the data; it is
#' the residual standard error from the nonlinear regression model. Smaller sigma values indicate
#' a better fit of the logistic curve to the data than larger values.
#'
#' `t_mid` is the time at which the population density reaches 12K (which occurs at the inflection point),
#' `t_gen` is the fastest possible generation time (also called the doubling time), `auc_l` is the area
#' under the logistic curve obtained by taking the integral of the logistic equation, and `auc_e` is
#' the empirical area under the curve which is obtained by summing up the area under the experimental
#' curve from the measurements in the input data. If you decide to use auc_l or auc_e, make sure that
#' you specify the parameter t_trim so that these metrics are comparable across samples or plates that
#'  were grown for different lengths of time.
#'
#'  The note value provides additional information about problems with fitting the logistic curve to
#'  your data. No common problems were identified if it is empty.
#'
#' @export
#'
do_growth_analysis <- function(varioscan) {
    wide_format <- create_wide_data(varioscan)
    growth_params <- determine_growth_params(wide_format)
    return(growth_params)
}

#' Models the yield as a function of dilution
#'
#' @param growth_params the tibble with growth parameters (a result of `do_growth_analysis()`)
#' @param dependent the dependent variable, either one of `yield` and `auc_l`
#' @param nls_trace when TRUE gives the tarce of the nls function
#'
#' @export
#'
model_dose_response <- function(growth_params,
                                dependent_var = "AUC_l",
                                data_key = "series",
                                nls_trace = FALSE) {
    if(! dependent_var %in% c("AUC_l", "AUC_e", "K", "yield")) {
        stop(paste0("this dependent variable can not be modeled: "), dependent_var)
    }

    growth_params <- mutate(growth_params, dilution = as.numeric(dilution))

    dilution_seq <- seq(from = 0,
                        to = max(growth_params$dilution),
                        length.out = 250)

    ## Build a model for each series individually
    fitted_data <- list()
    all_models <- list()
    all_IC50_IC90 <- list()

    message(paste0("modeling on ", dependent_var))

    for(current_key in unique(pull(growth_params, {{data_key}}))) {
        message(paste0("analysing: ", current_key))

        series_data <- growth_params %>%
            filter(.data[[data_key]] == current_key)
        #print(series_data)

        ## Build model
        if(dependent_var == "yield") {
            default_power <- 3
            the_model <- build_model(series = current_key,
                                   series_data = series_data,
                                   dilution_seq = dilution_seq,
                                   dependent_var = dependent_var,
                                   default_power = default_power,
                                   nls_trace = nls_trace)
        } else {
            default_power <- 2
            the_model <- build_model(series = current_key,
                                   series_data = series_data,
                                   dilution_seq = dilution_seq,
                                   dependent_var = dependent_var,
                                   default_power = default_power,
                                   nls_trace = nls_trace)
        }

        all_models[[current_key]] <- the_model

        ## Predict
        model_data <- data.frame(dilution = dilution_seq)
        model_data[, data_key] <- current_key
        model_data$predicted <- predict(the_model, newdata = model_data)
        fitted_data[[current_key]] <- model_data

        ## Get metadata
        #init_H0 = max(series_data$yield)
        all_IC50_IC90[[current_key]] <- calculate_series_meta_data(current_key, the_model, default_power, data_key)

        # all_IC50_IC90[[current_key]] <- series_meta_data
    }

    #print(str(results))
    all_results <- list()
    all_results$fitted_data <- dplyr::bind_rows(fitted_data) #, .id = "series"
    all_results$models <- all_models
    all_results$IC50_IC90 <- dplyr::bind_rows(all_IC50_IC90)

    return(all_results)
}

#' Builds the model using nls
build_model <- function(series,
                        series_data,
                        dilution_seq,
                        dependent_var = "yield",
                        default_power = 3,
                        nls_trace = FALSE) {
    init_H0 = max(series_data[, dependent_var])
    #print(dependent_var)
    first_formula <- paste0(dependent_var, " ~ H0 * exp(-r * dilution^", default_power, ")")
    #print(first_formula)
    ## generate first model to get values for H0 and r
    first_model <- nls(formula = first_formula, #
                        data = series_data,
                        start = list(H0 = init_H0,
                                     r = 1E4),
                        trace = nls_trace,
                        control = list(maxiter = 100))

    #print(first_model)
    yield_model <- tryCatch(expr = {
        message("trying secondary nls model")
        ## generate second model to get values for all three parameters
        second_formula <- paste0(dependent_var, " ~ H0 * exp(-r * dilution^power)")
        #print(second_formula)
        second_model <- nls(second_formula, #
                            data = series_data,
                            start = list(H0 = coef(first_model)["H0"],
                                         r = coef(first_model)["r"],
                                         power = default_power),
                            trace = nls_trace,
                            control = list(maxiter = 300,
                                           minFactor = 1E-4))
        ##succeeded second model, return this
        second_model
    }, error=function(cond) {
        message("model failed; returning simpler one")
        first_model
    })
    #yield_model <<- yield_model
    return(yield_model)
}



#' Calculates IC50 and IC90
calculate_series_meta_data <- function(current_key, the_model, default_power, data_key){
    ## calculate IC90 and IC50
    found_r <- coef(the_model)["r"]
    found_H0 <- coef(the_model)["H0"]
    power  <- coef(the_model)["power"]
    if(is.na(power)) power <- default_power
    IC50 <- found_H0 * 0.5
    IC90 <- found_H0 * 0.1

    ## MODEL 2: yield ~ H0 * exp(-r * dilution**2)
    ## yield = init_H0 * exp(-r * dilution^2)
    ## yield / init_H0 = exp(-r * dilution^2)
    ## log(yield / init_H0) = -r * dilution^2
    ## log(yield / init_H0) / -r = dilution^2
    ## dilution = sqrt(log(yield / init_H0) / -r)


    ## USING MODEL 2
    ## If the yield_C50 is negative this will produce an NaN
    dilution_IC50 <- (log(IC50 / found_H0) / -found_r)^(1/power)
    dilution_IC90 <- (log(IC90 / found_H0) / -found_r)^(1/power)

    series_meta_data <- data.frame(IC50 = round(dilution_IC50, 4),
                                   IC90 = round(dilution_IC90, 4))
    #print(data_key)
    series_meta_data[, data_key] <- current_key
    #print(series_meta_data)
    return(series_meta_data)
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

