#' plotting fluxes with exponential fit
#' @description plots the fluxes that were fitted with
#' an exponential model
#' @param slopes_df dataset containing slopes
#' @param y_text_position position of the text box
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @importFrom tidyr pivot_longer



flux_plot_exp <- function(slopes_df,
                          cut_arg,
                          y_text_position = 500) {
  param_df <- flux_param_exp(
    ((slopes_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- flux_plot_flag(((slopes_df)),
    ((param_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- slopes_df |>
    rename(
      fit = "f_fit",
      slope = "f_fit_slope"
    ) |>
    pivot_longer(
      cols = c("fit", "slope"),
      names_to = "linetype",
      values_to = "fit"
    )



  plot_exp <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_quality_flag),
      size = 0.2,
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
