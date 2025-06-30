#' plotting fluxes with exponential fit
#' @description plots the fluxes that were fitted with
#' an exponential model
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param kappamax indicating if kappamax was applied
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw geom_vline
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom tidyr pivot_longer



flux_plot_exp <- function(slopes_df,
                          f_conc,
                          f_datetime,
                          kappamax,
                          y_text_position) {



  if (!is.null(kappamax) && kappamax == TRUE) {
    param_df <- flux_param_kappamax(
      slopes_df,
      f_datetime = {{f_datetime}},
      y_text_position = y_text_position
    )
  }

  if (is.null(kappamax)) {
    param_df <- flux_param_exp(
      slopes_df,
      f_datetime = {{f_datetime}},
      y_text_position = y_text_position
    )
  }


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



  plot_exp <- slopes_df |>
    ggplot(aes(
      y = .data$f_value,
      x = {{f_datetime}},
      color = .data$f_quality_flag,
      linetype = .data$linetype,
      label = .data$print_col
    )) +
    theme_bw() +
    geom_point(
      data = slopes_df |> filter(
        !(.data$f_quality_flag %in% c("f_fits", "text"))
      ),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_vline(xintercept = slopes_df$f_start_z,
               color = "grey", linewidth = 0.5, na.rm = TRUE) +
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

  plot_exp
}
