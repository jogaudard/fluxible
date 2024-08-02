flux_plot_flag <- function(slopes_df,
                           param_df,
                           quality_flag_col = "f_quality_flag",
                           fluxID_col = "f_fluxID",
                           cut_arg = "cut"

){
    slopes_df <- slopes_df |>
    select(!c("f_quality_flag")) |>
      left_join(param_df, by = "f_fluxID") |>
        mutate(
          f_quality_flag = case_when(
            f_cut == ((cut_arg)) ~ f_cut,
            f_cut != ((cut_arg)) ~ f_quality_flag
          )
        )

    slopes_df
}