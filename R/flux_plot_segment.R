#' plots fluxes fitted with segmentation tool
#' @description provides plots for measurements that were fitted using
#' the segment model in flux_fitting
#' @importFrom dplyr select left_join mutate case_when
#' @importFrom ggplot2 ggplot theme_bw geom_point geom_text


flux_plot_segment <- function(slopes_df,
                              y_text_position,
                              cut_arg) {

  param_df <- flux_param_segment(((slopes_df)), cut_arg = ((cut_arg)))

  slopes_df <- slopes_df |>
    select(!c("f_quality_flag")) |>
    left_join(param_df, by = "f_fluxID") |>
    mutate(
      f_quality_flag = case_when(
        f_cut == ((cut_arg)) ~ f_cut,
        f_cut != ((cut_arg)) & f_quality_flag != "ok" ~ f_quality_flag,
        f_cut != ((cut_arg)) & f_quality_flag == "ok" ~ f_quality_flag_seg
      )
    )

  slopes_df <- slopes_df |>
    mutate(
      fit = .data$f_fit,
      linetype = "fit"
    )

  plot_seg <- slopes_df |>
    ggplot(aes(.data$f_datetime, group = .data$f_segment_id)) +
    theme_bw() +
    geom_point(aes(y = .data$f_conc, color = .data$f_quality_flag),
      size = 0.2,
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

  plot_seg
}