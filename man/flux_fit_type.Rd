% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flux_fit_type.R
\name{flux_fit_type}
\alias{flux_fit_type}
\title{to check the type of fit}
\usage{
flux_fit_type(
  df,
  fit_type = c(),
  fit_type_list = c("exp_hm", "exp_tz", "exp_zhao18", "exponential", "linear",
    "quadratic")
)
}
\arguments{
\item{df}{any dataframe}

\item{fit_type}{type of fit that was applied in flux_fitting. Needs to be
filled only if the df was produced outside of the Fluxible workflow.}

\item{fit_type_list}{list of fit types in use with Fluxible.}
}
\description{
extracts the type of fit that was applied in flux_fitting
or checks that the fit_type provided by the user is compatible with Fluxible
}
