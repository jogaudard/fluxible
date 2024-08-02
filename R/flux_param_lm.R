flux_param_lm <- function(slopes_df){
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

    param_df
}