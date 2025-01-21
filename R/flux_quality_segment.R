#' Quality control for segmented measurements
#' @description provides quality flags for the segments and entire measurements
#' following the same standards as for the linear model
#' @param slopes_df dataset containing slopes
#' @param pvalue_col column containing the p-value of each flux
#' (linear, quadratic, segment)
#' @param rsquared_col column containing the r squared of each flux
#' (linear, quadratic, segment)
#' @param f_flag_fit_col column flagging measurements that were too short to
#' find segments (optional; provided by \link[flux_fitting]{flux_fitting}).
#' @param par_col column containing segment average PAR data
#' @param sign_str_col column containing segment average signal strength
#' @param par_threshold PAR value threshold under which a segment should be
#' discarded (if PAR data are not provided, it will just be ignored)
#' @param sign_str_threshold signal strength threshold under which a segment
#' should be discarded
#' (if signal strength is not provided it will just be ignored)
#' @param pvalue_threshold threshold of p-value below which the change of
#' gas concentration over time is considered not significant
#' (linear, quadratic, segment)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' (linear, quadratic, segment)
#' @param sd_threshold standard deviation threshold under which a measurement
#' should be discarded. The standard deviation is calculated as
#' [sqrt(sum(f_segment_length * (f_slope_corr - f_mean_slope)^2) /
#' (((nb_segments_ok - 1) * sum(f_segment_length) / nb_segments_ok))]
#' where 'f_segment_length' is the length of each segment;
#' 'f_slope_corr' the slope of each segment after quality assessment
#' based on R² and p-value;
#' 'f_mean_slope' the mean of the slope for the entire measurement
#' weighed with the length of each segment;
#' 'nb_segments_ok' the number of segments, excluding those discarded based on
#' R² and p-value, in each segment.
#' The full equation is described in Smooth (1997).
#' @references SMOOTH, Y. DATAPLOT Reference Manual, 1997 2-66
#' @param cut_arg argument defining that the data point should be cut out
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @importFrom dplyr select rename mutate distinct filter case_when group_by
#' summarise n_distinct ungroup n left_join arrange
#' @importFrom tidyselect all_of any_of
#' @importFrom tidyr drop_na

flux_quality_segment <- function(slopes_df,
                                 pvalue_col,
                                 rsquared_col,
                                 f_flag_fit_col,
                                 par_col,
                                 sign_str_col,
                                 par_threshold,
                                 sign_str_threshold,
                                 pvalue_threshold,
                                 rsquared_threshold,
                                 sd_threshold,
                                 cut_arg,
                                 force_discard,
                                 force_ok) {

  args_ok_seg <- flux_fun_check(list(
    par_threshold = ((par_threshold)),
    sign_str_threshold = ((sign_str_threshold)),
    sd_threshold = ((sd_threshold))
  ),
  fn = list(is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 3))

  if (!is.null(((sign_str_col)))) {

    sign_str_check <- slopes_df |>
      select(
        all_of(((sign_str_col)))
      )

    sign_str_ok <- flux_fun_check(sign_str_check,
                                  fn = list(is.numeric),
                                  msg = c("has to be numeric"),
                                  origdf = slopes_df)

    slopes_df <- slopes_df |>
      rename(
        f_sign_str_seg = all_of(((sign_str_col)))
      )
  }

  if (is.null(((sign_str_col)))) {
    slopes_df <- slopes_df |>
      mutate(
        f_sign_str_seg = NA_real_
      )
    sign_str_ok <- TRUE
  }

  if (!is.null(((par_col)))) {

    par_check <- slopes_df |>
      select(
        all_of(((par_col)))
      )

    par_ok <- flux_fun_check(par_check,
                             fn = list(is.numeric),
                             msg = c("has to be numeric"),
                             origdf = slopes_df)

    slopes_df <- slopes_df |>
      rename(
        f_par_seg = all_of(((par_col)))
      )
  }

  if (is.null(((par_col)))) {
    slopes_df <- slopes_df |>
      mutate(
        f_par_seg = NA_real_
      )
    par_ok <- TRUE
  }


  if (any(!c(args_ok_seg, sign_str_ok, par_ok)))
    stop("Please correct the arguments", call. = FALSE)

  slopes_df <- slopes_df |>
    rename(
      f_flag_fit = all_of(((f_flag_fit_col)))
    )

  quality_flag <- slopes_df |>
    select(any_of(c(
      "f_fluxID",
      "f_cut",
      "f_segment_id",
      "f_par_seg",
      "f_sign_str_seg",
      "f_rsquared",
      "f_pvalue",
      "f_slope",
      "f_segment_length"
    ))) |>
    distinct() |>
    filter(
      .data$f_cut != ((cut_arg))
    ) |>
    mutate(
      f_quality_flag_seg = case_when(
        .data$f_par_seg <= ((par_threshold)) ~ "discard",
        .data$f_sign_str_seg <= ((sign_str_threshold)) ~ "discard",
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue <= ((pvalue_threshold)) ~ "discard",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue > ((pvalue_threshold)) ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag_seg == "ok" ~ .data$f_slope,
        .data$f_quality_flag_seg == "discard" ~ NA_real_,
        .data$f_quality_flag_seg == "zero" ~ 0
      )
    )

  quality_par <- quality_flag |>
    drop_na("f_slope_corr") |>
    group_by(.data$f_fluxID) |>
    summarise(
      f_mean_slope = sum(.data$f_slope_corr * .data$f_segment_length) /
        sum(.data$f_segment_length),
      nb_segments_ok = n_distinct(.data$f_segment_id),
      f_sd_slope = case_when(
        .data$nb_segments_ok < 2 ~ NA_real_,
        .data$nb_segments_ok >= 2 ~ sqrt(sum(
          .data$f_segment_length * (.data$f_slope_corr - .data$f_mean_slope)^2
        ) / ((
          (.data$nb_segments_ok - 1) * sum(.data$f_segment_length)
        ) / .data$nb_segments_ok
        ))
      ),
    ) |>
    ungroup()

  segment_flag <- quality_flag |>
    select("f_fluxID", "f_quality_flag_seg") |>
    drop_na("f_quality_flag_seg") |>
    distinct() |>
    group_by(.data$f_fluxID) |>
    mutate(
      count = n()
    ) |>
    ungroup() |>
    filter(.data$count == 1) |>
    rename(
      f_quality_flag = "f_quality_flag_seg"
    ) |>
    select(!"count")


  slopes_df <- slopes_df |>
    select(!any_of(c(
      "f_par_seg",
      "f_sign_str_seg",
      "f_rsquared",
      "f_pvalue",
      "f_slope",
      "f_segment_length"
    ))) |>
    left_join(quality_flag, by = c("f_fluxID", "f_cut", "f_segment_id")) |>
    left_join(quality_par, by = "f_fluxID") |>
    left_join(segment_flag, by = "f_fluxID") |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_quality_flag = case_when(
        !is.na(.data$f_quality_flag) ~ .data$f_quality_flag,
        .data$f_flag_fit == "too short" ~ "discard",
        .data$f_sd_slope >= ((sd_threshold)) &
          !is.na(.data$f_sd_slope) ~ "discard",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((force_discard)) ~ "weird_flux",
        .data$f_fluxID %in% ((force_ok)) ~ "force_ok",
        .data$f_start_error == "error" ~ "start_error",
        TRUE ~ "ok"
      ),
      f_mean_slope_corr = case_when(
        .data$f_quality_flag == "discard" ~ NA_real_,
        .data$f_quality_flag == "no_data" ~ NA_real_,
        .data$f_quality_flag == "start_error" ~ NA_real_,
        .data$f_quality_flag == "zero" ~ 0,
        .data$f_quality_flag == "ok" ~ .data$f_mean_slope
      )
    ) |>
    ungroup() |>
    arrange(.data$f_datetime)

  if (is.null(((sign_str_col)))) {
    slopes_df <- slopes_df |>
      select(!c("f_sign_str_seg"))
  }

  if (is.null(((par_col)))) {
    slopes_df <- slopes_df |>
      select(!c("f_par_seg"))
  }

  if (is.null(((f_flag_fit_col)))) {
    slopes_df <- slopes_df |>
      select(!c("f_flag_fit"))
  }

  slopes_df
}