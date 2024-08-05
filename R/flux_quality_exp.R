#' assessing fluxes quality calculated with an exponential fit
#' quality assessment for the fluxes calculated with the exponential model
#' @description indicates if fluxes should be discarded or replaced
#' by 0 according to parameters set by user
#' @param slopes_df dataset containing slopes, fluxID,
#' and parameters of the exponential expression
#' @param fluxID_col column containing unique IDs for each flux
#' @param conc_col column containing the measured gas concentration
#' @param b_col column containing the b parameter of the exponential expression
#' @param time_col column containing the time of each measurement in seconds
#' @param fit_col column containing the modeled data
#' @param slope_col column containing the slope of the exponential expression
#' used for the calculation
#' @param cut_col column containing the cutting information
#' @param weird_fluxesID vector of fluxIDs that should be discarded
#' by the user's decision
#' @param RMSE_threshold threshold for the RMSE of each flux above
#' which the fit is considered unsatisfactory
#' @param cor_threshold threshold for the correlation coefficient
#' of gas concentration with time below which the correlation
#' is considered non significant
#' @param b_threshold threshold for the b parameter. Defines a window
#' with its opposite inside which the fit is considered good enough.
#' @param ambient_conc ambient gas concentration in ppm at the site of
#' measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window in which
#' the starting values are considered acceptable
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor



flux_quality_exp <- function(slopes_df,
                             slope_col = "f_slope_tz",
                             weird_fluxesID = c(),
                             b_col = "f_b",
                             RMSE_threshold = 25,
                             cor_threshold = 0.5,
                             b_threshold = 1) {

  slopes_df <- slopes_df |>
    rename(
      f_b = all_of(((b_col))),
      f_slope_tz = all_of(((slope_col)))
    )


  quality_par <- slopes_df |>
    # we want to evaluate the part of the flux that we are keeping
    group_by(.data$f_fluxID, .data$f_cut) |>
    nest() |>
    rowwise() |>
    summarise(
      f_cor_coef = cor(data$f_conc, data$f_time),
      f_RMSE = sqrt((1 / length(data$f_time))
      * sum((data$f_fit - data$f_conc)^2))
    ) |>
    unnest(c("f_fluxID", "f_cut")) |>
    ungroup()

  

  quality_flag <- slopes_df |>
    left_join(quality_par, by = c("f_fluxID", "f_cut")) |>
    mutate(
      f_fit_quality = case_when(
        .data$f_b >= ((b_threshold)) ~ "bad_b",
        .data$f_RMSE > ((RMSE_threshold)) ~ "bad_RMSE"
      ),
      f_correlation = case_when(
        abs(.data$f_cor_coef) < ((cor_threshold)) ~ "no",
        TRUE ~ "yes"
      ),
      f_quality_flag = case_when(
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((weird_fluxesID)) ~ "weird_flux",
        .data$f_start_error == "error" ~ "start_error",
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "no" ~ "zero",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "no" ~ "zero",
        TRUE ~ "ok"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA_real_,
        .data$f_quality_flag == "weird_flux" ~ NA_real_,
        .data$f_quality_flag == "start_error" ~ NA_real_,
        .data$f_quality_flag == "discard" ~ NA_real_,
        .data$f_quality_flag == "zero" ~ 0,
        .data$f_quality_flag == "ok" ~ .data$f_slope_tz
      )
    )

  quality_flag
}
