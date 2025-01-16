#' @description flags measurements where at least one segment has a bad fit
#' flags measurements where segments are going in opposite direction
#' flags measurements where segments are too different
#' @importFrom dplyr n

flux_quality_segment <- function(slopes_df,
                                 pvalue_col,
                                 rsquared_col,
                                 f_flag_fit_col,
                                 par_threshold,
                                 sign_str_threshold,
                                 pvalue_threshold,
                                 rsquared_threshold,
                                 sd_threshold,
                                 cut_arg,
                                 weird_fluxes_id,
                                 force_ok_id
) {

#   if (!is.na(((par_threshold)))) {
#      slopes_df <- slopes_df |>
#     group_by(.data$f_fluxID, .data$f_cut, .data$f_segment_id) |>
#     mutate(
#       f_quality_flag_seg = case_when(
#         .data$f_par_seg <= ((par_threshold)) ~ "low_par"
#       )
#       ) |>
#       ungroup()
#   }

#   if (!is.na(((sign_str_threshold)))) {
#      slopes_df <- slopes_df |>
#     group_by(.data$f_fluxID, .data$f_cut, .data$f_segment_id) |>
#     mutate(
#       f_quality_flag_seg = case_when(
#         .data$f_sign_str_seg <= ((sign_str_threshold)) ~ "low_signal_threshold"
#       )
#       ) |>
#       ungroup()
#   }

  slopes_df <- slopes_df |>
    rename(
      f_flag_fit = all_of(((f_flag_fit_col)))
    )

  quality_flag <- slopes_df |>
  select(any_of(c("f_fluxID", "f_cut", "f_segment_id", "f_par_seg", "f_sign_str_seg", "f_rsquared", "f_pvalue", "f_slope", "f_segment_length"))) |>
  unique() |>
  filter(
    .data$f_cut != ((cut_arg))
  ) |>
    # group_by(.data$f_fluxID, .data$f_segment_id) |>
    mutate(
      f_quality_flag_seg = case_when(
        # !is.na(((par_threshold))) &
          .data$f_par_seg <= ((par_threshold)) ~ "low_par",
        # !is.na(((sign_str_threshold))) &
          .data$f_sign_str_seg <= ((sign_str_threshold)) ~ "low_sign_str", # we should keep just ok and discard here, makes it easier for plotting
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue >= ((pvalue_threshold)) ~ "discard",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue < ((pvalue_threshold)) ~ "zero"
      ),
      f_slope_corr = case_when(
        # .data$f_quality_flag_seg == "low_par" ~ NA_real_,
        # .data$f_quality_flag_seg == "low_sign_str" ~ NA_real_,
        .data$f_quality_flag_seg == "ok" ~ .data$f_slope,
        .data$f_quality_flag_seg == "discard" ~ NA_real_,
        .data$f_quality_flag_seg == "zero" ~ 0
      )
    )
    # ungroup()

    # flag at flux level
    # sd (using slope corr), ratio, wierd flux, force ok, start error
    # maybe this workflow does not allow weird and force ok?

# first we need the mean slope with sd

    quality_par <- quality_flag |>
    # select("f_slope_corr", "f_fluxID", "f_segment_length") |>
    drop_na("f_slope_corr") |>
    group_by(.data$f_fluxID) |>
    # nest() |>
    # rowwise() |>
    summarise(
    # mutate(
      # nb_segments = length(.data$f_segment_length),
      f_mean_slope = sum(.data$f_slope_corr * .data$f_segment_length) /
        sum(.data$f_segment_length),
        # https://franksaundersjr.com/2023/01/06/how-to-calculate-weighted-mean-and-weighted-standard-deviation-with-python/
      f_sd_slope = sqrt(sum(.data$f_segment_length * (.data$f_slope_corr - .data$f_mean_slope)^2) /
        ((length(.data$f_segment_length) - 1) * sum(.data$f_segment_length) / length(.data$f_segment_length))),
      # .groups = "drop"
    ) |>
    ungroup()
    # unnest("f_cut")

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
    select(!any_of(c("f_par_seg", "f_sign_str_seg", "f_rsquared", "f_pvalue", "f_slope", "f_segment_length"))) |>
    left_join(quality_flag, by = c("f_fluxID", "f_cut", "f_segment_id")) |>
    left_join(quality_par, by = "f_fluxID") |>
    left_join(segment_flag, by = "f_fluxID") |>
    group_by(.data$f_fluxID) |>
    mutate(
      # f_mean_slope = fill(.data$f_mean_slope, .direction = "downup"),
      # f_sd_slope = fill(.data$f_sd_slope, .direction = "downup"),
      f_quality_flag = case_when(
        !is.na(.data$f_quality_flag) ~ .data$f_quality_flag,
        .data$f_flag_fit == "too short" ~ "discard",
        .data$f_sd_slope >= ((sd_threshold)) ~ "discard",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((weird_fluxes_id)) ~ "weird_flux",
        .data$f_fluxID %in% ((force_ok_id)) ~ "force_ok",
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
    ungroup()

  slopes_df
}