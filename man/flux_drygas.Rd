% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flux_drygas.R
\name{flux_drygas}
\alias{flux_drygas}
\title{wet air correction}
\usage{
flux_drygas(conc_df, gas_wet, h2o_wet)
}
\arguments{
\item{conc_df}{dataframe of gas concentration over time}

\item{gas_wet}{the gas to correct}

\item{h2o_wet}{water vapor concentration before correction (in mmol/mol)}
}
\value{
the same dataframe with the additional column \verb{[gas_wet]_dry} in the
same unit as \code{gas_wet}
}
\description{
Corrects for the amount of water vapor inside the air
}
\details{
the correction is done as follows
\ifelse{html}{\out{gas_dry = gas_wet / (1 - (h2o_wet / 1000))}}{\eqn{gas_dry = gas_wet / (1 - (h2o_wet / 1000}{ASCII}}
}
\examples{
data(wet_conc)
flux_drygas(wet_conc, co2, h2o)
}
