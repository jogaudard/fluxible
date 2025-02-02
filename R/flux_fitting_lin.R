#' linear fit to gas concentration over time
#' @description fits a linear model to the gas concentration over time
#' @param conc_df dataframe of gas concentration over time
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @return a df with the modeled gas concentration, slope, intercept,
#' std error, r square and p value of the linear model
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when
#' ungroup filter left_join distinct pull bind_cols
#' @importFrom tidyr drop_na pivot_wider fill
#' @importFrom haven as_factor
#' @importFrom stringr str_c


flux_fitting_lin <- function(conc_df,
                             conc_col,
                             datetime_col,
                             start_col,
                             end_col,
                             fluxid_col,
                             start_cut,
                             end_cut) {

name_conc <- names(select(conc_df, {{conc_col}}))

  by_fluxID <- dplyr::join_by({{fluxid_col}} == {{fluxid_col}})

  conc_df <- conc_df |>
    mutate(
      f_time = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      {{start_col}} := {{start_col}} + ((start_cut)),
      {{end_col}} := {{end_col}} - ((end_cut)),
      f_cut = case_when(
        {{datetime_col}} < {{start_col}} | {{datetime_col}} >= {{end_col}}
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      f_n_conc = sum(!is.na(.data[[name_conc]])),
      .by = {{fluxid_col}}
    )

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na({{conc_col}}) |>
    mutate(
      f_time_cut = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      length_window = max(.data$f_time_cut),
      length_flux = difftime({{end_col}}, {{start_col}}, units = "sec"),
      time_diff = .data$f_time - .data$f_time_cut,
      f_n_conc_cut = sum(!is.na(.data[[name_conc]])),
      .by = {{fluxid_col}}
    )

  fitting_par <- conc_df_cut |>
    group_by({{fluxid_col}}) |>
    nest() |>
    mutate(
      model = map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
      tidy = map(.data$model, broom::tidy),
      glance = map(.data$model, broom::glance)
    ) |>
    select(!c("data", "model")) |>
    unnest("tidy") |>
    select({{fluxid_col}}, "term", "estimate", "glance") |>
    pivot_wider(names_from = "term", values_from = "estimate") |>
    unnest("glance") |>
    rename(
      f_slope = "f_time_cut",
      f_intercept = "(Intercept)",
      f_rsquared = "r.squared",
      f_adj_rsquared = "adj.r.squared",
      f_pvalue = "p.value"
    ) |>
    select({{fluxid_col}}, "f_rsquared", "f_adj_rsquared", "f_slope", "f_intercept", "f_pvalue") |>
    ungroup()

  conc_fitting <- conc_df |>
    left_join(fitting_par, by = by_fluxID) |>
    mutate(
      f_fit = .data$f_intercept + .data$f_slope * (.data$f_time - ((start_cut)))
    )

  warning_msg <- conc_df |>
    left_join(conc_df_cut,
      by = dplyr::join_by(
        {{datetime_col}} == {{datetime_col}},
        {{fluxid_col}} == {{fluxid_col}},
        "f_n_conc" == "f_n_conc"
      )
    ) |>
    select({{fluxid_col}}, "f_n_conc", "f_n_conc_cut", "length_flux") |>
    distinct() |>
    mutate(
      low_data = paste(
        "\n", "fluxID", {{fluxid_col}}, ": slope was estimated on",
        .data$f_n_conc_cut, "points out of", .data$length_flux,
        "seconds"
      ),
      no_data = paste(
        "\n", "fluxID", {{fluxid_col}},
        "dropped (no data in the conc column)"
      ),
      warnings = case_when(
        .data$f_n_conc == 0 ~ .data$no_data,
        .data$f_n_conc_cut != .data$length_flux ~ .data$low_data
      ),
      warnings = as.character(.data$warnings)
    ) |>
    drop_na(warnings) |>
    pull(.data$warnings)

  warnings <- str_c(warning_msg)

  if (any(!is.na(warnings))) warning(warnings)


  conc_fitting
}
