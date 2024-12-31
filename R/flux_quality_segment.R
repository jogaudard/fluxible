#' @description flags measurements where at least one segment has a bad fit
#' flags measurements where segments are going in opposite direction
#' flags measurements where segments are too different
#' 

flux_quality_segment <- function(slopes_df,
                                 par_threshold,
                                 sign_str_threshold,
                                 pvalue_threshold,
                                 rsquared_threshold,
                                 sd_threshold,
                                 cut_arg
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

  quality_flag <- slopes_df |>
  select(any_of("f_fluxID", "f_cut", "f_segment_id", "f_par_seg", "f_sign_str_seg", "f_rsquared", "f_pvalue", "f_slope", "f_segment_length")) |>
  unique() |>
  filter(
    .data$f_cut != ((cut_arg))
  ) |>
    # group_by(.data$f_fluxID, .data$f_segment_id) |>
    mutate(
      f_quality_flag_seg = case_when(
        !is.na(((par_threshold))) &
          .data$f_par_seg <= ((par_threshold)) ~ "low_par",
        !is.na(((sign_str_threshold))) &
          .data$f_sign_str_seg <= ((sign_str_threshold)) ~ "low_sign_str",
        .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue >= ((pvalue_threshold)) ~ "discard",
        .data$f_rsquared < ((rsquared_threshold)) &
          .data$f_pvalue < ((pvalue_threshold)) ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag_seg == "low_par" ~ NA_real_,
        .data$f_quality_flag_seg == "low_sign_str" ~ NA_real_,
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
    group_by(.data$f_fluxID) |>
    nest() |>
    rowwise() |>
    summarise(
      mean_slope = sum(.data$f_slope * .data$f_segment_length) /
        sum(.data$f_segment_length),
      .groups = "drop"
    ) |>
    unnest("f_cut")


    slopes_df <- slopes_df |>
    group_by(.data$f_fluxID, .data$f_cut) |>
    mutate(
      f_quality_flag = case_when(
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((weird_fluxes_id)) ~ "weird_flux",
        .data$f_fluxID %in% ((force_ok_id)) ~ "force_ok",
        .data$f_start_error == "error" ~ "start_error",
        )
    )
}