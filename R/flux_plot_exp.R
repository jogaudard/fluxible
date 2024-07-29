#' ploting fluxes for fit evaluation
#' @description plots the fluxes and indicates what should be
#' discarded or replaced by zero
#' @param slopes_df dataset containing slopes
#' @param datetime_col column containing datetime of
#' each concentration measurement
#' @param conc_col column containing gas concentration data
#' @param cut_col column containing cut factor from the
#' flux_fitting function ("cut" or "keep")
#' @param fit_col column containing the modelled fit of the flux
#' @param quality_flag_col column containing the flags produced by flux_quality
#' @param fluxID_col column containing unique IDs for each flux
#' @param fit_slope_col column containing the modelled slope at tz
#' @param b_col column containing the b parameter of the exponential fit
#' @param cor_coef_col column containing the correlation coefficient
#' produced by flux_quality
#' @param RMSE_col column containing the RMSE produced by flux_quality
#' @param start_col column containing the datetime of the start of each flux
#' @param y_text_position position of the text box
#' but will take time depending on the size of the dataset
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_exp <- function(slopes_df,
                          datetime_col = "f_datetime",
                          conc_col = "f_conc",
                          cut_col = "f_cut",
                          fit_col = "f_fit",
                          fit_slope_col = "f_fit_slope",
                          quality_flag_col = "f_quality_flag",
                          fluxID_col = "f_fluxID",
                          start_col = "f_start",
                          b_col = "f_b",
                          cor_coef_col = "f_cor_coef",
                          RMSE_col = "f_RMSE",
                          y_text_position = 500
                          ) {
  



  slopes_df <- slopes_df |>
    rename(
      f_fit_slope = all_of(((fit_slope_col))),
      f_b = all_of(((b_col))),
      f_cor_coef = all_of(((cor_coef_col))),
      f_RMSE = all_of(((RMSE_col)))
    )

  param_df <- slopes_df |>
    select(
      "f_conc", "f_start", "f_fluxID", "f_RMSE", "f_cor_coef", "f_b", "f_cut",
      "f_quality_flag"
    ) |>
    filter(.data$f_cut == "keep") |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_RMSE = round(.data$f_RMSE, digits = 1),
      f_cor_coef = round(.data$f_cor_coef, digits = 2),
      f_b = round(.data$f_b, digits = 5),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n", "Corr coef = ",
        .data$f_cor_coef, "\n", "b = ", .data$f_b,
        sep = ""
      )
    )

  plot_exp <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_cut),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = .data$f_fit, color = .data$f_quality_flag),
      linetype = "longdash",
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = .data$f_fit_slope, color = .data$f_quality_flag),
      linetype = "dashed",
      na.rm = TRUE
    ) +
    geom_text(
      data = param_df,
      aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )


  plot_exp
  
}
