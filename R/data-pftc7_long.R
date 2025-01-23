#' CO2 concentration
#'
#' CO2 concentration with measurements meta data, measured with a tent setup
#'
#' @format a tibble with 18 columns and 5701 rows
#' \describe{
#' \item{co2_conc}{CO2 concentration in ppm.}
#' \item{h2o_conc}{Water vapour concentration.}
#' \item{temperature_c}{Air temperature inside the flux chamber in Celsius.}
#' \item{signal_strength}{Strength of the signal in the sensor.}
#' \item{date_time}{Datetime at which CO2 concentration was recorded.}
#' \item{start_time}{Datetime at which the measurement was started.}
#' \item{file_name}{Name of individual flux file.}
#' \item{site}{Experimental site where measurements took place.}
#' \item{elevation}{Elevation of plot, in masl.}
#' \item{aspect}{Aspect of plots (west or east facing slope of the mountain).}
#' \item{plot}{Plot number.}
#' \item{day_night}{Indicating if the measurement was done during day or night.}
#' \item{measurement}{Type of flux
#' (photo = nee; a = ambient; resp = respiration).}
#' \item{redo}{Indicating if the measurement is a redo.}
#' \item{plot_id}{Unique ID for each plot.}
#' \item{par}{Photosynthetically active radiation inside the chamber
#' in micromol/s/sqm.}
#' \item{f_end}{Datetime at which the measurement ended.}
#' \item{pressure}{Pressure inside the chamber in atm.}
#' }
#' @examples
#' pftc7_long
"pftc7_long"
