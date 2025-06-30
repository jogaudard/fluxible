#' plotting fluxes with a quadratic fit
#' @description specific part of flux_plot for
#' quadratic fit
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text geom_vline
#' @importFrom tidyr pivot_longer



flux_plot_quadratic <- function(slopes_df,
                                f_conc,
                                f_datetime,
                                y_text_position) {

  param_df <- flux_param_qua(
    slopes_df,
    f_datetime = {{f_datetime}},
    y_text_position = y_text_position
    )

  # slopes_df <- flux_plot_flag(slopes_df, param_df)

slopes_df <- slopes_df |>
    pivot_longer(
      cols = c({{f_conc}}, "f_fit", "f_fit_slope", "f_fit_lm"),
      names_to = "linetype",
      values_to = "f_value"
    ) |>
    mutate(
      f_quality_flag = case_when(
        .data$linetype %in% c("f_fit", "f_fit_slope", "f_fit_lm") ~ "f_fits",
        .default = .data$f_quality_flag
      )
    ) |>
    bind_rows(param_df)



  plot_quadratic <- slopes_df |>
    ggplot(aes(
      y = .data$f_value,
      x = {{f_datetime}},
      color = .data$f_quality_flag,
      linetype = .data$linetype,
      label = .data$print_col
    )) +
    theme_bw() +
    geom_vline(xintercept = slopes_df$f_start_z,
               color = "grey", linewidth = 0.5, na.rm = TRUE) +
    geom_point(
      data = slopes_df |> filter(
        !(.data$f_quality_flag %in% c("f_fits", "text"))
      ),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_line(
      data = slopes_df |> filter(
        .data$f_quality_flag == "f_fits"
      ),
      linewidth = 0.3,
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    geom_text(
      data = slopes_df |> drop_na("print_col"),
      hjust = "inward",
      vjust = "bottom"
    )

  plot_quadratic
}
