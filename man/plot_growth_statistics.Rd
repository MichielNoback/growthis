% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_growth_statistics.R
\name{plot_growth_statistics}
\alias{plot_growth_statistics}
\title{Create a heatmap of growth statistics}
\usage{
plot_growth_statistics(growth_params_tibble, variable_name, do_scale = FALSE)
}
\arguments{
\item{growth_params_tibble}{a tibble containing results of growth statistics analysis.}

\item{variable_name}{the variable to create a heatmap for}

\item{do_scale}{a logical indicating wheter the data should be scaled}
}
\description{
Creates a heatmap based on a selected variable of a growth statistics dataset
}
