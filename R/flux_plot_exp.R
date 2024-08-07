#' plotting fluxes with exponential fit
#' @description plots the fluxes that were fitted with
#' an exponential model
#' @param slopes_df dataset containing slopes
#' @param fit_slope_col column containing the modelled slope at tz
#' @param b_col column containing the b parameter of the exponential fit
#' @param cor_coef_col column containing the correlation coefficient
#' produced by flux_quality
#' @param RMSE_col column containing the RMSE produced by flux_quality
#' @param y_text_position position of the text box
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_exp <- function(slopes_df,
                          cut_arg = "cut",
                          fit_slope_col = "f_fit_slope",
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

  param_df <- flux_param_exp(
    ((slopes_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- flux_plot_flag(((slopes_df)),
                              ((param_df)),
                              cut_arg = ((cut_arg))
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
