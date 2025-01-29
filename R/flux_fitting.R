#' Fitting a model to concentration data and estimating the slope
#' @description fits gas concentration over time data with a model
#' (exponential, quadratic or linear) and provides the slope later used
#' to calculate gas fluxes with flux_calc
#' @param fit_type exponential, quadratic or linear.
#' Exponential is using the exponential model from Zhao et al (2018)
#' @references Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
#' On the calculation of daytime CO2 fluxes measured by automated closed
#' transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
#' https://doi.org/10.1016/j.agrformet.2018.08.022
#' @param conc_df dataframe of gas concentration over time
#' @param t_window enlarge focus window before and after tmin and tmax
#' (exponential fit)
#' @param cz_window window used to calculate Cz, at the beginning of cut window
#' (exponential fit)
#' @param b_window window to estimate b. It is an interval after tz where
#' it is assumed that the model fits the data perfectly (exponential fit)
#' @param a_window window at the end of the flux to estimate a (exponential fit)
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as cz_window (exponential fit)
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' Note that if there are duplicated datetime in the same f_fluxID only
#' the first row will be kept
#' @param conc_col column with gas concentration data
#' @param fluxid_col column with ID of each flux
#' @param t_zero time at which the slope should be calculated
#' (for quadratic fit)
#' @return a dataframe with the slope at t zero,
#' and parameters of a model of gas concentration over time
#' @examples
#' data(co2_conc)
#' flux_fitting(co2_conc, fit_type = "exp")
#' flux_fitting(co2_conc, fit_type = "quadratic", t_zero = 10, end_cut = 30)
#' @export

flux_fitting <- function(conc_df,
                         start_cut = 0,
                         end_cut = 0,
                         start_col = "f_start",
                         end_col = "f_end",
                         datetime_col = "f_datetime",
                         conc_col = "f_conc",
                         fluxid_col = "f_fluxID",
                         t_window = 20,
                         cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15,
                         t_zero = 0,
                         fit_type) {
  args_ok <- flux_fun_check(list(
    start_cut = ((start_cut)),
    end_cut = ((end_cut))
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  conc_df_check <- conc_df |>
    select(
      all_of(((conc_col))),
      all_of(((start_col))),
      all_of(((end_col))),
      all_of(((datetime_col)))
    )

  conc_df_ok <- flux_fun_check(conc_df_check,
                               fn = list(
                                 is.numeric,
                                 is.POSIXct,
                                 is.POSIXct,
                                 is.POSIXct
                               ),
                               msg = rep(c(
                                 "has to be numeric",
                                 "has to be POSIXct"
                               ),
                               c(1, 3)
                               ),
                               origdf = conc_df)


  if (any(!c(args_ok, conc_df_ok)))
    stop("Please correct the arguments", call. = FALSE)

  conc_df <- conc_df |>
    rename(
      f_start = all_of((start_col)),
      f_end = all_of((end_col)),
      f_datetime = all_of((datetime_col)),
      f_conc = all_of((conc_col)),
      f_fluxID = all_of((fluxid_col))
    )

  conc_df <- conc_df |>
    group_by(.data$f_fluxID) |>
    distinct(.data$f_datetime, .keep_all = TRUE) |>
    ungroup()

  fit_type <- flux_fit_type(
    ((conc_df)),
    fit_type = ((fit_type))
  )

  if (((fit_type)) == "exponential") {
    conc_fitting <- flux_fitting_exp(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut)),
      t_window = ((t_window)),
      cz_window = ((cz_window)),
      b_window = ((b_window)),
      a_window = ((a_window)),
      roll_width = ((roll_width))
    )
  }


  if (((fit_type)) == "linear") {
    conc_fitting <- flux_fitting_lin(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut))
    )
  }

  if (((fit_type)) == "quadratic") {
    conc_fitting <- flux_fitting_quadratic(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut)),
      t_zero = ((t_zero))
    )
  }

  attr(conc_fitting, "fit_type") <- ((fit_type))

  conc_fitting
}
