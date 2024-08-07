#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID, p.value and r.squared
#' @param slope_col column containing the slope of each flux
#' (as calculated by the flux_fitting function)
#' @param pvalue_threshold threshold of p-value below which the change
#' of gas concentration over time is considered not significant (user decided)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' @param weird_fluxesID vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_okID vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared to be used for
#' the quality assessment
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename left_join



flux_quality_lm <- function(slopes_df,
                            slope_col = "f_slope",
                             weird_fluxesID = c(),
                             force_okID = c(),
                             pvalue_col = "f_pvalue",
                             rsquared_col = "f_rsquared",
                             pvalue_threshold = 0.3,
                             rsquared_threshold = 0.7) {
  slopes_df <- slopes_df |>
    rename(
      f_pvalue = all_of(((pvalue_col))),
      f_rsquared = all_of(((rsquared_col))),
      f_slope = all_of(((slope_col)))
    )

  

  slopes_df <- slopes_df |>
      group_by(.data$f_fluxID, .data$f_cut) |>
    mutate(
      f_quality_flag = case_when(
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((weird_fluxesID)) ~ "weird_flux",
        .data$f_fluxID %in% ((force_okID)) ~ "force_ok",
        .data$f_start_error == "error" ~ "start_error",
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue >= ((pvalue_threshold)) ~ "discard",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue < ((pvalue_threshold)) ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA_real_,
        .data$f_quality_flag == "weird_flux" ~ NA_real_,
        .data$f_quality_flag == "force_ok" ~ .data$f_slope,
        .data$f_quality_flag == "ok" ~ .data$f_slope,
        .data$f_quality_flag == "discard" ~ NA_real_,
        .data$f_quality_flag == "zero" ~ 0
      )
    ) |>
    ungroup()

  slopes_df
}
