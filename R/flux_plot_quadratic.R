#' plotting fluxes with a quadratic fit
#' @description specific part of flux_plot for
#' quadratic fit
#' @param slopes_df dataset containing slopes
#' @param y_text_position position of the text box
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr rename select distinct
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @importFrom tidyr pivot_longer



flux_plot_quadratic <- function(slopes_df,
                                conc_col,
                                datetime_col,
                                y_text_position,
                                cut_arg) {
  param_df <- flux_param_lm(((slopes_df)), {{conc_col}})

  slopes_df <- flux_plot_flag(((slopes_df)), ((param_df)))

  slopes_df <- slopes_df |>
      pivot_longer(
      cols = c("f_fit", "f_fit_slope"),
      names_to = "linetype",
      values_to = "f_fit"
    )

  plot_quadratic <- slopes_df |>
    ggplot(aes({{datetime_col}})) +
    theme_bw() +
    geom_point(aes(y = {{conc_col}}, color = .data$f_quality_flag),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_text(
      data = param_df,
      aes(
        x = .data$f_start, y = ((y_text_position)),
        label = .data$print_col
      ),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

  plot_quadratic
}
