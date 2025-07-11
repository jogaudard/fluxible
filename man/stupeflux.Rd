% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stupeflux.R
\name{stupeflux}
\alias{stupeflux}
\title{From raw gas concentration over time to clean fluxes}
\usage{
stupeflux(
  raw_conc,
  field_record,
  f_datetime,
  start_col,
  end_col,
  f_conc,
  setup_volume,
  measurement_length,
  fit_type,
  temp_air_col,
  atm_pressure,
  plot_area,
  conc_unit,
  flux_unit,
  cols_keep = c(),
  cols_ave = c(),
  cols_sum = c(),
  cols_med = c(),
  ratio_threshold = 0.5,
  time_diff = 0,
  start_cut = 0,
  end_cut = 0,
  cz_window = 15,
  b_window = 10,
  a_window = 10,
  roll_width = 15,
  t_zero = 0,
  force_discard = c(),
  force_ok = c(),
  force_zero = c(),
  ambient_conc = 421,
  error = 100,
  pvalue_threshold = 0.3,
  rsquared_threshold = 0.7,
  rmse_threshold = 25,
  cor_threshold = 0.5,
  b_threshold = 1,
  temp_air_unit = "celsius",
  cut = TRUE,
  slope_correction = TRUE
)
}
\arguments{
\item{raw_conc}{dataframe of CO2 concentration measured continuously.
Has to contain at least a datetime column in ymd_hms format and
a gas concentration column as double.}

\item{field_record}{dataframe recording which measurement happened when.
Has to contain at least a column containing the start of each measurement,
and any other column identifying the measurements.}

\item{f_datetime}{datetime column in raw_conc (dmy_hms format)}

\item{start_col}{start column in field_record (dmy_hms format)}

\item{end_col}{end columne in field_record (\code{ymd_hms} format)}

\item{f_conc}{concentration column in raw_conc}

\item{setup_volume}{volume of the flux chamber and instrument together in L,
can also be a column in case it is a variable}

\item{measurement_length}{length of the measurement (in seconds)
from the start specified in the field_record}

\item{fit_type}{\code{exp_zhao18}, \code{exp_tz}, \code{exp_hm}, \code{quadratic} or \code{linear.}
\code{exp_zhao18} is using the exponential model
\ifelse{html}{\out{C(t) = C_m + a (t - t_z) + (C_z - C_m) exp(-b (t - t_z))}}{\eqn{C(t) = C_m + a (t - t_z) + (C_z - C_m) \exp(-b (t - t_z))}{ASCII}}
from Zhao et al (2018).
\code{expt_tz} is a modified version which allows the user to fix \code{t_zero}:
\ifelse{html}{\out{C(t) = C_m + a * t + (C_z - C_m) exp(-b * t)}}{\eqn{C(t) = C_m + a * t + (C_z - C_m) \exp(-b * t)}{ASCII}}
\code{exp_hm} is using the HM model
(Pedersen et al., 2010; Hutchinson and Mosier, 1981)
\ifelse{html}{\out{C(t) = C_m + (C_z - C_m) exp(-b * t)}}{\eqn{C(t) = C_m + (C_z - C_m) \exp(-b * t)}{ASCII}}}

\item{temp_air_col}{column containing the air temperature used
to calculate fluxes. Will be averaged with NA removed.}

\item{atm_pressure}{atmospheric pressure,
can be a constant (numerical) or a variable (column name)}

\item{plot_area}{area of the plot in m^2,
can also be a column in case it is a variable}

\item{conc_unit}{unit in which the concentration of gas was measured
\code{ppm} or \code{ppb}}

\item{flux_unit}{unit in which the calculated flux will be
\code{mmol} outputs fluxes in
\ifelse{html}{\out{mmol * m<sup>-2</sup> * h<sup>-1</sup>}}{\eqn{mmol*m^{-2}*h^{-1}}{ASCII}};
\code{micromol} outputs fluxes in
\ifelse{html}{\out{micromol * m<sup>-2</sup> * h<sup>-1</sup>}}{\eqn{micromol*m^{-2}*h^{-1}}{ASCII}}}

\item{cols_keep}{columns to keep from the input to the output.
Those columns need to have unique values for each flux,
as distinct() is applied.}

\item{cols_ave}{columns with values that should be averaged
for each flux in the output. Note that NA are removed in mean calculation.}

\item{cols_sum}{columns with values for which is sum is provided
for each flux in the output. Note that NA are removed in sum calculation.}

\item{cols_med}{columns with values for which is median is provided
for each flux in the output. Note that NA are removed in median calculation.}

\item{ratio_threshold}{ratio of gas concentration data points over length of
measurement (in seconds) below which the measurement will be considered as
not having enough data points to be considered for calculations}

\item{time_diff}{time difference (in seconds) between the two datasets.
Will be added to the datetime column of the raw_conc dataset.
For situations where the time was not synchronized correctly.}

\item{start_cut}{time to discard at the start of the measurements
(in seconds)}

\item{end_cut}{time to discard at the end of the measurements (in seconds)}

\item{cz_window}{window used to calculate Cz, at the beginning of cut window
(\code{exp_zhao18} and \code{exp_tz} fits)}

\item{b_window}{window to estimate b. It is an interval after tz where
it is assumed that the model fits the data perfectly
(\code{exp_zhao18} and \code{exp_tz} fits)}

\item{a_window}{window at the end of the flux to estimate a
(\code{exp_zhao18} and \code{exp_tz} fits)}

\item{roll_width}{width of the rolling mean for CO2 when looking for tz,
ideally same as cz_window (\code{exp_zhao18} and \code{exp_tz} fits)}

\item{t_zero}{time at which the slope should be calculated
(for \code{quadratic} and \code{exp_tz} fits)}

\item{force_discard}{vector of fluxIDs that should be discarded
by the user's decision}

\item{force_ok}{vector of fluxIDs for which the user wants to keep
the calculated slope despite a bad quality flag}

\item{force_zero}{vector of fluxIDs that should be replaced by zero by
the user's decision}

\item{ambient_conc}{ambient gas concentration in ppm at the site of
measurement (used to detect measurement that started with a polluted setup)}

\item{error}{error of the setup, defines a window outside of which
the starting values indicate a polluted setup}

\item{pvalue_threshold}{threshold of p-value below which the change of
gas concentration over time is considered not significant
(linear and quadratic fit)}

\item{rsquared_threshold}{threshold of r squared value below which
the linear model is considered an unsatisfactory fit
(linear and quadratic fit)}

\item{rmse_threshold}{threshold for the RMSE of each flux above which
the fit is considered unsatisfactory (\code{exp_zhao18} and \code{exp_tz} fits)}

\item{cor_threshold}{threshold for the correlation coefficient of
gas concentration with time below which the correlation
is considered not significant (\code{exp_zhao18} and \code{exp_tz} fits)}

\item{b_threshold}{threshold for the b parameter.
Defines a window with its opposite inside which the fit is
considered good enough (\code{exp_zhao18} and \code{exp_tz} fits)}

\item{temp_air_unit}{units in which air temperature was measured.
Has to be either \code{celsius} (default), \code{fahrenheit} or \code{kelvin.}}

\item{cut}{if 'TRUE' (default), the measurements will be cut according to
'f_cut' before calculating fluxes. This has no influence on the flux itself
since the slope is provided from \link[fluxible:flux_fitting]{flux_fitting},
but it will influence the values of the columns in \code{cols_ave}.}

\item{slope_correction}{logical. If \code{TRUE}, the flux will be calculated with
the slope corrected according to the recommandations of the quality flags.}
}
\value{
a dataframe containing flux IDs, datetime of measurements' starts,
fluxes in
\ifelse{html}{\out{mmol * m<sup>-2</sup> * h<sup>-1</sup>}}{\eqn{mmol*m^{-2}*h^{-1}}{ASCII}}
or
\ifelse{html}{\out{micromol * m<sup>-2</sup> * h<sup>-1</sup>}}{\eqn{micromol*m^{-2}*h^{-1}}{ASCII}}
(\code{f_flux}) according to \code{flux_unit}, temperature average for each flux in
Kelvin (\code{f_temp_ave}), the total volume of the setup for each measurement
(\code{f_volume_setup}), the model used in
\link[fluxible:flux_fitting]{flux_fitting}, any column specified in
\code{cols_keep}, any column specified in \code{cols_ave} with
their value averaged over the measurement after cuts and discarding NA.
}
\description{
Wrapper function for the Fluxible workflow. We recommand using
the step-by-step workflow for more control over the process.
}
\examples{
data(co2_df_short)
data(record_short)
stupeflux(
raw_conc = co2_df_short,
field_record = record_short,
f_datetime = datetime,
start_col = start,
f_conc = conc,
measurement_length = 180,
fit_type = "exp_zhao18",
temp_air_col = temp_air,
conc_unit = "ppm",
flux_unit = "mmol",
setup_volume = 24.575,
atm_pressure = 1,
plot_area = 0.0625
)
}
\references{
Pedersen, A.R., Petersen, S.O., Schelde, K., 2010.
A comprehensive approach to soil-atmosphere trace-gas flux estimation with
static chambers. European Journal of Soil Science 61, 888–902.
https://doi.org/10.1111/j.1365-2389.2010.01291.x

Hutchinson, G.L., Mosier, A.R., 1981. Improved Soil Cover Method
for Field Measurement of Nitrous Oxide Fluxes.
Soil Science Society of America Journal 45, 311–316.
https://doi.org/10.2136/sssaj1981.03615995004500020017x

Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
On the calculation of daytime CO2 fluxes measured by automated closed
transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
https://doi.org/10.1016/j.agrformet.2018.08.022
}
