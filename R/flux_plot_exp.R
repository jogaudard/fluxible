#' plotting fluxes with exponential fit
#' @description plots the fluxes that were fitted with
#' an exponential model
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param kappamax indicating if kappamax was applied
#' @param x_nudge x nudge for position_nudge in geom_text
#' @param y_nudge y nudge for position_nudge in geom_text
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw geom_vline
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' position_nudge
#' @importFrom tidyr pivot_longer



flux_plot_exp <- function(slopes_df,
                          f_conc,
                          f_datetime,
                          kappamax,
                          x_nudge,
                          y_nudge) {



  if (!is.null(kappamax) && kappamax == TRUE) {
    param_df <- flux_param_kappamax(slopes_df)
  }

  if (is.null(kappamax)) {
    param_df <- flux_param_exp(slopes_df)
  }

  slopes_df <- flux_plot_flag(slopes_df, param_df)

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
    )



  plot_exp <- slopes_df |>
    ggplot(aes(
      y = .data$f_value,
      x = {{f_datetime}},
      color = .data$f_quality_flag,
      linetype = .data$linetype,
      label = .data$print_col
    )) +
    theme_bw() +
    geom_vline(xintercept = slopes_df$f_start_z,
               color = "grey", linewidth = 0.5) +
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
        .data$linetype %in% c("f_fit", "f_fit_slope", "f_fit_lm")
      ),
      linewidth = 0.3,
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    geom_point(
      data = slopes_df |> filter(
        !(.data$linetype %in% c("f_fit", "f_fit_slope", "f_fit_lm"))
      ),
      size = 0.2,
      na.rm = TRUE
    )

  plot_exp
}
