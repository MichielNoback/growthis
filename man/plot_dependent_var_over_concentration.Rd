% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_growth_statistics.R
\name{plot_dependent_var_over_concentration}
\alias{plot_dependent_var_over_concentration}
\title{Plots one of a few selected parameters over concentration}
\usage{
plot_dependent_var_over_concentration(
  growth_params_tibble,
  all_model_data,
  dependent_var = "yield",
  data_key = "series",
  exp = "all"
)
}
\arguments{
\item{growth_params_tibble}{a tibble containing results of growth statistics analysis.}

\item{all_model_data}{a list of models and fitted data (result of call to \code{model_dose_response})}

\item{dependent_var}{the dependent variable to plot on the y axis Can be one of "AUC_l", "AUC_e", "yield"}

\item{data_key}{the variable to split out on usng color. Defaults to "series".}

\item{exp}{the experiment (series) to plot}
}
\description{
Creates a scatter plot of the selected growth parameter over concentration (dilution) for all replicates.
}
