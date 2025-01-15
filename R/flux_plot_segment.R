#' plots the fluxes as part of flux_plot, with flags from flux_quality and modelled value from flux_segment
#' 
#' 


flux_plot_segment <- function(slopes_df,
                              y_text_position = 500,
                              cut_arg = "cut"
) {
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
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(aes(y = .data$f_conc, color = .data$f_quality_flag, group = .data$f_segment_id),
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