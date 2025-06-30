#' creates the flag column to be used by flux_plot
#' @description creates a column with quality flags (from flux_quality)
#' for the part of the rows to be kept, and cut flag for rows to be discarded
#' @param slopes_df as provided in flux_plot
#' @importFrom dplyr select left_join mutate case_when

flux_plot_flag <- function(slopes_df) {
  slopes_df <- slopes_df |>
    mutate(
      f_quality_flag = case_when(
        f_cut == "cut" ~ "cut",
        f_cut != "cut" ~ f_quality_flag
      )
    )

  slopes_df
}
