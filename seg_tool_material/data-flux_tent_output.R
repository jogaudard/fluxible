#' CO2 concentration
#'
#' CO2 fluxes calculated with segmentation method used in PFTC7
#' (for comparison of final result)
#' @format a tibble with 19 columns and 13 rows
#' \describe{
#' \item{file_name}{Name of individual flux file.}
#' \item{avg_temp}{Air temperature inside the flux chamber in Celsius
#' averaged over the flux measurement.}
#' \item{flux_value}{CO2 flux in micromol/sqm/s.}
#' \item{flux_type}{Type of flux
#' (photo = nee; a = ambient; resp = respiration).}
#' \item{plot_id}{Unique ID for each plot.}
#' \item{seg_sd}{Standard deviation of the mean slope of segments slopes.}
#' \item{avg_co2}{Averaged CO2 concentration of the measurement in ppm.}
#' \item{avg_h2o}{Averaged H2O concentration of the measurement.}
#' \item{r2}{Average of R² of each segment.}
#' \item{aic}{AIC of lm model of CO2 concentration over time.}
#' \item{param}{Gas used in flux calculation.}
#' \item{flux_direction_flag}{Flag indicating if
#' segments are going in different directions.}
#' \item{redo}{Indicating if the measurement is a redo.}
#' \item{day_night}{Indicating if the measurement was done during day or night.}
#' \item{par_flag}{Flagging if PAR is too low.}
#' \item{r2_flag}{Flagging if R² is too low.}
#' \item{seg_flag}{Flagging if segments do not meet quality standards.}
#' \item{flux_flag}{Flagging if flux should be kept or discarded.}
#' \item{pressure}{Pressure inside the chamber in atm.}
#' }
#' @examples
#' flux_tent_output
"flux_tent_output"
