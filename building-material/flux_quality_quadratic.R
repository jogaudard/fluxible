#' quality assessment for the fluxes calculated with the quadratic model
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user
#' @param slopes_df dataset containing slopes, fluxID, p.value and r.squared
#' @param pvalue_threshold threshold of p-value below which the change
#' of gas concentration over time is considered not significant (user decided)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' @param fluxID_col column containing unique IDs for each flux
#' @param conc_col column containing the measured gas concentration
#' @param slope_col column containing the slope of each flux
#' (as calculated by the flux_fitting function)
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared to be used for
#' the quality assessment
#' @param ambient_conc ambient gas concentration in ppm at the site of
#' measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window in
#' which the starting values are considered acceptable
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename left_join



flux_quality_quadratic <- function(slopes_df,
                             force_discard = c(),
                             pvalue_col = "f_pvalue",
                             rsquared_col = "f_rsquared",
                             pvalue_threshold = 0.3,
                             rsquared_threshold = 0.7) {
  slopes_df <- slopes_df |>
    rename(
      f_pvalue = all_of(((pvalue_col))),
      f_rsquared = all_of(((rsquared_col)))
    )

  quality_par_start <- slopes_df |>
    group_by(.data$f_fluxid) |>
    nest() |>
    rowwise() |>
    summarise(
      f_start_error = case_when(
        data$f_conc[1] < (((ambient_conc)) - ((error))) ~ "error",
        data$f_conc[1] > (((ambient_conc)) + ((error))) ~ "error",
        TRUE ~ "ok"
      )
    ) |>
    unnest("f_fluxid") |>
    ungroup()

  slopes_df <- slopes_df |>
    left_join(quality_par_start, by = "f_fluxid") |>
    mutate(
      f_quality_flag = case_when(
        .data$f_fluxid %in% ((force_discard)) ~ "force_discard",
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue >= ((pvalue_threshold)) ~ "discard",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue < ((pvalue_threshold)) ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "force_discard" ~ NA,
        .data$f_quality_flag == "ok" ~ .data$f_slope,
        .data$f_quality_flag == "discard" ~ NA,
        .data$f_quality_flag == "zero" ~ 0
      )
    )

  slopes_df
}
