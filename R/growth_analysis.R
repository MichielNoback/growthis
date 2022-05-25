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

#' Models the yield as a function of dilution
#' returns a dataframe with fitted
model_dose_response <- function(growth_params, nls_trace = FALSE) {
    # yield_model <- model_dose_response(growth_params_tibble, exp, model)
    dilution_seq <- seq(from = 0,
                        to = max(growth_params$dilution),
                        length.out = 100)

    ## Build a model for each series individually
    fitted_data <- list()
    all_models <- list()
    all_IC50_IC90 <- list()

    for(current_series in unique(growth_params$series)) {
        message(paste0("analysing: ", current_series))

        series_data <- growth_params %>%
            filter(series == current_series) %>%
            mutate(dilution = as.numeric(dilution))
        #print(series_data)

        ## Build model
        default_power = 3
        yield_model <- build_model(current_series, series_data, dilution_seq, default_power, nls_trace)
        all_models[[current_series]] <- yield_model

        ## Predict
        model_data <- data.frame(dilution = dilution_seq)
        model_data$series <- current_series
        model_data$predicted <- predict(yield_model, newdata = model_data)
        fitted_data[[current_series]] <- model_data

        ## Get metadata
        #yield_H0 = max(series_data$yield)
        all_IC50_IC90[[current_series]] <- calculate_series_meta_data(current_series, yield_model, default_power)

        # all_IC50_IC90[[current_series]] <- series_meta_data
    }

    #print(str(results))
    all_results <- list()
    all_results$fitted_data <- dplyr::bind_rows(fitted_data) #, .id = "series"
    all_results$models <- all_models
    all_results$IC50_IC90 <- dplyr::bind_rows(all_IC50_IC90)

    return(all_results)
}

#' Builds the model using nls
build_model <- function(current_series, series_data, dilution_seq, default_power, nls_trace = FALSE) {
    yield_H0 = max(series_data$yield)

    ## generate first model to get values for H0 and r
    yield_model1 <- nls(yield ~ H0 * exp(-r * dilution^default_power), #yield ~ H0 * exp(-r * dilution),
                        data = series_data,
                        start = list(H0 = yield_H0,
                                     r = 1E4),
                        trace = nls_trace,
                        control = list(maxiter = 100))

    yield_model <- tryCatch(expr = {
        message("trying second nls model")
        ## generate second model to get values for all three parameters
        yield_model2 <- nls(yield ~ H0 * exp(-r * dilution^power), #yield ~ H0 * exp(-r * dilution),
                            data = series_data,
                            start = list(H0 = coef(yield_model1)["H0"],
                                         r = coef(yield_model1)["r"],
                                         power = default_power),
                            trace = nls_trace,
                            control = list(maxiter = 300,
                                           minFactor = 1E-4))
        ##succeeded second model, return this
        yield_model2
    }, error=function(cond) {
        message("model failed; returning simpler one")
        yield_model1
    })
    yield_model <<- yield_model
    return(yield_model)
}



#' Calculates IC50 and IC90
calculate_series_meta_data <- function(current_series, yield_model, default_power){
    ## calculate IC90 and IC50
    found_r <- coef(yield_model)["r"]
    found_H0 <- coef(yield_model)["H0"]
    power  <- coef(yield_model)["power"]
    if(is.na(power)) power <- default_power
    yield_IC50 <- found_H0 * 0.5
    yield_IC90 <- found_H0 * 0.1

    ## MODEL 1: yield = H0 * exp(-r * dilution)
    ## H0 <- 0.56656
    ## found_r <- 293.1988
    ## yield_IC50 <- 0.5 * yield_H0
    ## yield_IC50 = yield_H0 * exp(-found_r * dilution_IC50)
    ## yield_IC50 / yield_H0 = exp(-found_r * dilution_IC50)
    ## log(yield_IC50 / yield_H0) = -found_r * dilution_IC50
    ## dilution_IC50 = log(yield_IC50 / yield_H0) / -found_r


    ## MODEL 2: yield ~ H0 * exp(-r * dilution**2)
    ## yield = yield_H0 * exp(-r * dilution^2)
    ## yield / yield_H0 = exp(-r * dilution^2)
    ## log(yield / yield_H0) = -r * dilution^2
    ## log(yield / yield_H0) / -r = dilution^2
    ## dilution = sqrt(log(yield / yield_H0) / -r)


    ## USING MODEL 2
    ## If the yield_C50 is negative this will produce an NaN
    dilution_IC50 <- (log(yield_IC50 / found_H0) / -found_r)^(1/power) #log(yield_IC50 / yield_H0) / -found_r
    dilution_IC90 <- (log(yield_IC90 / found_H0) / -found_r)^(1/power)  #log(yield_IC90 / yield_H0) / -found_r

    series_meta_data <- data.frame(series = current_series,
                                   IC50 = round(dilution_IC50, 4),
                                   IC90 = round(dilution_IC90, 4))
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

