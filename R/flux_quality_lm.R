#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID, p.value and r.squared
#' @param pvalue_threshold threshold of p-value below which the change
#' of gas concentration over time is considered not significant (user decided)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared to be used for
#' the quality assessment
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename left_join



flux_quality_lm <- function(slopes_df,
                            force_discard,
                            force_ok,
                            pvalue_col,
                            rsquared_col,
                            pvalue_threshold,
                            rsquared_threshold) {
  args_ok <- flux_fun_check(list(
    pvalue_threshold = ((pvalue_threshold)),
    rsquared_threshold = ((rsquared_threshold))
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  slopes_df_check <- slopes_df |>
    select(
      all_of(((pvalue_col))),
      all_of(((rsquared_col)))
    )

  slopes_df_ok <- flux_fun_check(slopes_df_check,
    fn = list(
      is.numeric,
      is.numeric
    ),
    msg = rep("has to be numeric", 2),
    origdf = slopes_df
  )


  if (any(!c(args_ok, slopes_df_ok)))
    stop("Please correct the arguments", call. = FALSE)

  slopes_df <- slopes_df |>
    rename(
      f_pvalue = all_of(((pvalue_col))),
      f_rsquared = all_of(((rsquared_col)))
    )



  slopes_df <- slopes_df |>
    group_by(.data$f_fluxID, .data$f_cut) |>
    mutate(
      f_quality_flag = case_when(
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue >= ((pvalue_threshold)) ~ "zero",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue < ((pvalue_threshold)) ~ "discard",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_start_error == "error" ~ "start_error",
        .data$f_fluxID %in% ((force_discard)) ~ "force_discard",
        .data$f_fluxID %in% ((force_ok)) ~ "force_ok",
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA_real_,
        .data$f_quality_flag == "force_discard" ~ NA_real_,
        .data$f_quality_flag == "force_ok" ~ .data$f_slope,
        .data$f_quality_flag == "ok" ~ .data$f_slope,
        .data$f_quality_flag == "discard" ~ NA_real_,
        .data$f_quality_flag == "zero" ~ 0
      )
    ) |>
    ungroup()

  slopes_df
}
