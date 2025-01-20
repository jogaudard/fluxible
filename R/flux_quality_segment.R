#' Quality control for segmented measurements
#' @description provides quality flags for the segments and entire measurements following the same standards as for the linear model
#' @param

#' @importFrom dplyr n n_distinct

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
  fn = list(is.logical, is.numeric),
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
    unique() |>
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
      nb_segments_ok = dplyr::n_distinct(.data$f_segment_id),
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
      count = dplyr::n()
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

  slopes_df
}