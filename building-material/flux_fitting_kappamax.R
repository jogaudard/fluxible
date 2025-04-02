#' Fitting a model to the gas concentration curve and estimating the slope over
#' time, using the HM model
#' (Pedersen et al., 2010; Hutchinson and Mosier, 1981).
#' @references Pedersen, A.R., Petersen, S.O., Schelde, K., 2010.
#' A comprehensive approach to soil-atmosphere trace-gas flux estimation with
#' static chambers. European Journal of Soil Science 61, 888–902.
#' https://doi.org/10.1111/j.1365-2389.2010.01291.x
#' @references Hutchinson, G.L., Mosier, A.R., 1981. Improved Soil Cover Method
#' for Field Measurement of Nitrous Oxide Fluxes.
#' Soil Science Society of America Journal 45, 311–316.
#' https://doi.org/10.2136/sssaj1981.03615995004500020017x
#' @description Fits the exponential expression to the concentration evolution
#' `C(t) = C_m + (C_z - C_m) exp(-b * t)`
#' @param conc_df dataframe of gas concentration over time
#' @param conc_df_cut dataframe of gas concentration over time, cut
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each concentration measurement
#' Note that if there are duplicated datetime in the same f_fluxid only
#' the first row will be kept
#' @param f_start column with datetime when the measurement started
#' @param f_end column with datetime when the measurement ended
#' @param f_fluxid column with ID of each flux
#' @param cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz
#' where it is assumed that C fits the data perfectly
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as cz_window
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param t_zero time at which the slope should be calculated
#' (for quadratic fit)
#' @return a dataframe with the slope at t zero,
#' modeled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename mutate select group_by case_when
#' ungroup filter distinct left_join rowwise summarize pull slice
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data
#' @importFrom broom tidy



flux_fitting_kappamax <- function(conc_df_cut,
                            conc_df,
                            conc_col,
                            datetime_col,
                            f_start,
                            f_end,
                            f_fluxid,
                            cz_window,
                            b_window,
                            roll_width,
                            start_cut,
                            end_cut,
                            t_zero) {

  conc_fitting_hm <- flux_fitting_hm(
      conc_df_cut,
      conc_df,
      {{conc_col}},
      {{datetime_col}},
      {{f_start}},
      {{f_end}},
      {{f_fluxid}},
      start_cut = start_cut,
      end_cut = end_cut,
      t_zero = t_zero,
      cz_window = cz_window,
      b_window = b_window,
      roll_width = roll_width
    )

    # calculating kappamax

    conc_fitting <- conc_fitting_hm |>
        mutate(
            kappamax = inst_error / .data$f_length_window,
            f_fit = case_when(
                abs(.data$b) <= kappamax ~ f_fit,
                abs(.data$b) > kappamax ~ f_fit_lm
            ),
            model = case_when(
                abs(.data$b) <= kappamax ~ "hm",
                abs(.data$b) > kappamax ~ "linear"
            )
        )


  message("Done.")


  conc_fitting
}
