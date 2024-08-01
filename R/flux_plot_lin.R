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
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared
#' used for the quality assessment
#' @param start_col column containing the datetime of the start of each flux
#' @param y_text_position position of the text box
#' but will take time depending on the size of the dataset
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_lin <- function(slopes_df,
                          datetime_col = "f_datetime",
                          conc_col = "f_conc",
                          cut_col = "f_cut",
                          fit_col = "f_fit",
                          quality_flag_col = "f_quality_flag",
                          fluxID_col = "f_fluxID",
                          pvalue_col = "f_pvalue",
                          rsquared_col = "f_rsquared",
                          start_col = "f_start",
                          y_text_position = 500
                          ) {
  

  slopes_df <- slopes_df |>
    rename(
      f_datetime = all_of(((datetime_col))),
      f_conc = all_of(((conc_col))),
      f_cut = all_of(((cut_col))),
      f_fit = all_of(((fit_col))),
      f_quality_flag = all_of(((quality_flag_col))),
      f_fluxID = all_of(((fluxID_col))),
      f_pvalue = all_of(((pvalue_col))),
      f_rsquared = all_of(((rsquared_col))),
      f_start = all_of(((start_col)))
    )

  param_df <- slopes_df |>
    select("f_conc", "f_start", "f_fluxID", "f_rsquared", "f_pvalue", "f_quality_flag") |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_rsquared = round(.data$f_rsquared, digits = 2),
      f_pvalue = round(.data$f_pvalue, digits = 4),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "R2 = ", .data$f_rsquared, "\n", "p-value = ", .data$f_pvalue,
        sep = ""
      )
    ) |>
    select("f_fluxID", "conc_start", "print_col", "f_quality_flag")

  slopes_df <- slopes_df |>
    select(!c("f_quality_flag")) |>
      left_join(param_df, by = "f_fluxID")

  plot_lin <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(aes(y = .data$f_conc, color = .data$f_cut),
      size = 0.2,
      na.rm = TRUE
      ) +
    geom_line(
      aes(y = .data$f_fit, color = .data$f_quality_flag),
      linetype = "longdash",
      na.rm = TRUE
    ) +
    geom_text(
      aes(
        x = .data$f_start, y = ((y_text_position)),
        label = .data$print_col
      ),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

plot_lin

}
