#' ploting fluxes for fit evaluation
#' @description plots the fluxes and indicates what should be
#' discarded or replaced by zero
#' @param slopes_df dataset containing slopes
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared
#' used for the quality assessment
#' @param y_text_position position of the text box
#' but will take time depending on the size of the dataset
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_quadratic <- function(slopes_df,
                          pvalue_col = "f_pvalue",
                          rsquared_col = "f_rsquared",
                          y_text_position = 500,
                          cut_arg = "cut"
                          ) {
  


 plot_quadratic <- slopes_df |>
        flux_plot_lin(
          pvalue_col = ((pvalue_col)),
          rsquared_col = ((rsquared_col)),
          y_text_position = ((y_text_position)),
          cut_arg = ((cut_arg))
        )

 plot_quadratic <- plot_quadratic +
  geom_line(
      aes(y = .data$f_fit_slope, color = .data$f_quality_flag),
      linetype = "dashed",
      na.rm = TRUE
    )

plot_quadratic

}
