#' Slopes for each flux
#'
#' Slopes of C(t) for each flux without cut.
#'
#' @format A tibble with 1251 rows and 24 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber
#' in micromol/s/sqm.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{f_start}{Datetime at which the measurement was started.}
#' \item{f_end}{Datetime at which the measurement ended.}
#' \item{f_fluxid}{Unique ID for each flux.}
#' \item{f_flag_match}{Flags from flux_match.}
#' \item{n_conc}{Number of data point per flux.}
#' \item{ratio}{Ratio of n_conc over length of the measurement (in seconds).}
#' \item{flag}{Data quality flags.}
#' \item{time}{Time variable of the flux in seconds.}
#' \item{cut}{Indicating if the measurement should be kept (keep)
#' or discarded (cut).}
#' \item{Cm_est}{Estimation of the Cm parameter.}
#' \item{a_est}{Estimation of the a parameter.}
#' \item{b_est}{Estimation of the b parameter.}
#' \item{tz_est}{Estimation of the tz parameter.}
#' \item{Cz}{Cz parameter of the C(t) function.}
#' \item{Cm}{Cm parameter of the C(t) function, calculated by optim() with
#' Cm_est as starting point.}
#' \item{a}{a parameter of the C(t) function, calculated by optim() with
#' a_est as starting point.}
#' \item{b}{b parameter of the C(t) function, calculated by optim() with
#' b_est as starting point.}
#' \item{tz}{tz parameter of the C(t) function, calculated by optim() with
#' tz_est as starting point.}
#' \item{slope_tz}{Slope of C(t) at tz}
#' \item{fit}{C(t), modeled CO2 concentration as a function of time.}
#' \item{fit_slope}{Output of linear model of CO2 concentration passing by
#' C(tz) and a slope of slope_tz.}
#' \item{start_z}{Datetime format of tz}
#' \item{volume}{volume of chamber in L}
#' \item{tube_vol}{volume of tubes in L}
#' }
#' @examples
#' slopes0_vol_tube
"slopes0_vol_tube"
