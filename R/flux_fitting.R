#' wrap up function for fitting
#' @description fits gas concentration over time with an exponential or
#' linear model
#' @param fit_type exponential or linear, depending on the wish of the user.
#' Exponential is using the fit described in Zhao 2018
#' @param conc_df dataframe of gas concentration over time
#' @param t_window enlarge focus window before and after tmin and tmax
#' @param Cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz where
#' it is assumed that C fits the data perfectly
#' @param a_window window at the end of the flux to estimate a
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as Cz_window
#' @param start_cut to cut at the start
#' @param end_cut to cut at the end, if you notice on the plots that the match
#' was not precise enough
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxID_col column with ID of each flux
#' @param t_zero time at which the slope should be calculated (for fits that do not include t_zero as a parameter)
#' @return a dataframe with the slope at t zero,
#' modelled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when ungroup
#' filter distinct left_join rowwise summarize pull
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @examples
#' data(co2_conc)
#' flux_fitting(co2_conc, fit_type = "exp")
#' flux_fitting(co2_conc, fit_type = "quadratic", t_zero = 10, end_cut = 30)
#' @export
#'

flux_fitting <- function(conc_df,
                         start_cut = 0,
                         end_cut = 0,
                         start_col = "f_start",
                         end_col = "f_end",
                         datetime_col = "f_datetime",
                         conc_col = "f_conc",
                         fluxID_col = "f_fluxID",
                         t_window = 20,
                         Cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15,
                         t_zero = 0,
                         fit_type) {
  fit_type <- match.arg(((fit_type)), c("exponential", "linear", "quadratic"))

  if (((fit_type)) == "exponential") {
    conc_fitting <- flux_fitting_exp(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut)),
      start_col = ((start_col)),
      end_col = ((end_col)),
      datetime_col = ((datetime_col)),
      conc_col = ((conc_col)),
      fluxID_col = ((fluxID_col)),
      t_window = ((t_window)),
      Cz_window = ((Cz_window)),
      b_window = ((b_window)),
      a_window = ((a_window)),
      roll_width = ((roll_width))
    )
  }


  if (((fit_type)) == "linear") {
    conc_fitting <- flux_fitting_lin(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut)),
      start_col = ((start_col)),
      end_col = ((end_col)),
      datetime_col = ((datetime_col)),
      conc_col = ((conc_col)),
      fluxID_col = ((fluxID_col))
    )
  }

  if (((fit_type)) == "quadratic") {
    conc_fitting <- flux_fitting_quadratic(
      conc_df,
      start_cut = ((start_cut)),
      end_cut = ((end_cut)),
      start_col = ((start_col)),
      end_col = ((end_col)),
      datetime_col = ((datetime_col)),
      conc_col = ((conc_col)),
      fluxID_col = ((fluxID_col)),
      t_zero = ((t_zero))
    )
  }


  conc_fitting
}
