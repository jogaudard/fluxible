#' plotting fluxes with linear fit
#' @description plots the fluxes that were fitted
#' with a linear model
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param x_nudge x nudge for position_nudge in geom_text
#' @param y_nudge y nudge for position_nudge in geom_text
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw position_nudge
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text




flux_plot_lin <- function(slopes_df,
                          f_conc,
                          f_datetime,
                          x_nudge,
                          y_nudge) {
  param_df <- flux_param_lm(slopes_df)

  slopes_df <- flux_plot_flag(slopes_df, param_df)

slopes_df <- slopes_df |>
    pivot_longer(
      cols = c({{f_conc}}, "f_fit"),
      names_to = "linetype",
      values_to = "f_value"
    ) |>
    mutate(
      f_quality_flag = case_when(
        .data$linetype == "f_fit" ~ "f_fits",
        .default = .data$f_quality_flag
      )
    )



  plot_lin <- slopes_df |>
    ggplot(aes(
      y = .data$f_value,
      x = {{f_datetime}},
      color = .data$f_quality_flag,
      linetype = .data$linetype,
      label = .data$print_col
    )) +
    theme_bw() +
    geom_text(
      data = slopes_df |> filter(
        .data$linetype == "f_fit"
      ),
      hjust = "outward",
      position = position_nudge(x = x_nudge, y = y_nudge),
      na.rm = TRUE
    ) +
    geom_line(
      data = slopes_df |> filter(
        .data$linetype == "f_fit"
      ),
      linewidth = 0.3,
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    geom_point(
      data = slopes_df |> filter(
        .data$linetype != "f_fit"
      ),
      size = 0.2,
      na.rm = TRUE
    )

  plot_lin
}
