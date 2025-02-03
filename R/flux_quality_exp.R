#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID,
#' and parameters of the exponential expression
#' @param b_col column containing the b parameter of the exponential expression
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param rmse_threshold threshold for the RMSE of each flux above
#' which the fit is considered unsatisfactory
#' @param cor_threshold threshold for the correlation coefficient
#' of gas concentration with time below which the correlation
#' is considered non significant
#' @param b_threshold threshold for the b parameter. Defines a window
#' with its opposite inside which the fit is considered good enough.
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor



flux_quality_exp <- function(slopes_df,
                             conc_col,
                             fluxid_col,
                             slope_col,
                             time_col,
                             fit_col,
                             cut_col,
                             b_col,
                             force_discard,
                             force_ok,
                             rmse_threshold,
                             cor_threshold,
                             b_threshold) {

  name_df <- deparse(substitute(slopes_df))


  args_ok <- flux_fun_check(list(
    rmse_threshold = rmse_threshold,
    cor_threshold = cor_threshold,
    b_threshold = b_threshold
  ),
  fn = list(is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 3))

  slopes_df_check <- slopes_df |>
    select({{b_col}})

  slopes_df_ok <- flux_fun_check(slopes_df_check,
                                 fn = list(is.numeric),
                                 msg = "has to be numeric",
                                 name_df = name_df)


  if (any(!c(args_ok, slopes_df_ok)))
    stop("Please correct the arguments", call. = FALSE)



  quality_par <- slopes_df |>
    group_by({{fluxid_col}}, {{cut_col}}) |>
    summarise(
      f_cor_coef = cor({{conc_col}}, {{time_col}}),
      f_RMSE =
        sqrt((1 / length({{time_col}})) * sum(({{fit_col}} - {{conc_col}})^2)),
      .groups = "drop"
    )



  quality_flag <- slopes_df |>
    left_join(quality_par, by = dplyr::join_by(
      {{fluxid_col}} == {{fluxid_col}},
      {{cut_col}} == {{cut_col}}
    )
    ) |>
    mutate(
      f_fit_quality = case_when(
        {{b_col}} >= b_threshold ~ "bad_b",
        .data$f_RMSE > rmse_threshold ~ "bad_RMSE"
      ),
      f_correlation = case_when(
        abs(.data$f_cor_coef) < cor_threshold ~ "no",
        TRUE ~ "yes"
      ),
      f_quality_flag = case_when(
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "no" ~ "zero",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "no" ~ "zero",
        .data$f_RMSE <= rmse_threshold ~ "ok",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_start_error == "error" ~ "start_error",
        {{fluxid_col}} %in% force_discard ~ "force_discard",
        {{fluxid_col}} %in% force_ok ~ "force_ok"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA,
        .data$f_quality_flag == "force_discard" ~ NA,
        .data$f_quality_flag == "force_ok" ~ {{slope_col}},
        .data$f_quality_flag == "start_error" ~ NA,
        .data$f_quality_flag == "discard" ~ NA,
        .data$f_quality_flag == "zero" ~ 0,
        .data$f_quality_flag == "ok" ~ {{slope_col}}
      )
    )

  quality_flag
}
