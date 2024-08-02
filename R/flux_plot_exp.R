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
                          # datetime_col = "f_datetime",
                          # conc_col = "f_conc",
                          # cut_col = "f_cut",
                          # cut_arg = "cut",
                          # fit_col = "f_fit",
                          fit_slope_col = "f_fit_slope",
                          # quality_flag_col = "f_quality_flag",
                          # fluxID_col = "f_fluxID",
                          # start_col = "f_start",
                          b_col = "f_b",
                          cor_coef_col = "f_cor_coef",
                          RMSE_col = "f_RMSE"
                          # y_text_position = 500,
                          # f_ylim_upper = 800,
                          # f_ylim_lower = 400
                          ) {
  



  slopes_df <- slopes_df |>
    rename(
      f_fit_slope = all_of(((fit_slope_col))),
      f_b = all_of(((b_col))),
      f_cor_coef = all_of(((cor_coef_col))),
      f_RMSE = all_of(((RMSE_col)))
    )

  param_df <- flux_param_exp(
    ((slopes_df)),
    # conc_col = "f_conc",
    # quality_flag_col = "f_quality_flag",
    # fluxID_col = "f_fluxID",
    # start_col = "f_start",
    # b_col = "f_b",
    # cor_coef_col = "f_cor_coef",
    # RMSE_col = "f_RMSE",
    # cut_col = "f_cut",
    cut_arg = ((cut_arg))
  )

  slopes_df <- flux_plot_flag(((slopes_df)),
                              ((param_df))
                              )

  



  plot_exp <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_quality_flag),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = .data$f_fit),
      linetype = "longdash",
      linewidth = 0.3,
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = .data$f_fit_slope),
      linetype = "dashed",
      linewidth = 0.2,
      na.rm = TRUE
    ) +
    geom_text(
      aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    ) 

  plot_exp
  
}
