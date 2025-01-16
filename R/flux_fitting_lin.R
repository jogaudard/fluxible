#' linear fit to gas concentration over time
#' @description fits a linear model to the gas concentration over time
#' @param conc_df dataframe of gas concentration over time
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxid_col column with ID of each flux
#' @return a df with the modeled gas concentration, slope, intercept,
#' std error, r square and p value of the linear model
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when
#' ungroup filter left_join distinct pull bind_cols
#' @importFrom tidyr drop_na pivot_wider fill
#' @importFrom haven as_factor
#' @importFrom stringr str_c


flux_fitting_lin <- function(conc_df,
                             start_cut,
                             end_cut,
                             start_col,
                             end_col,
                             datetime_col,
                             conc_col,
                             fluxid_col) {
  args_ok <- flux_fun_check(list(
    start_cut = ((start_cut)),
    end_cut = ((end_cut))
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  if (any(!args_ok))
    stop("Please correct the arguments", call. = FALSE)




  length_flux_max <- conc_df |>
    mutate(
      length_flux = difftime(.data$f_end, .data$f_start, units = "sec"),
      length_flux = as.double(.data$length_flux)
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements!"
    )
  }

  conc_df <- conc_df |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_time = difftime(.data$f_datetime[seq_along(.data$f_datetime)],
        .data$f_datetime[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      f_start = .data$f_start + ((start_cut)),
      f_end = .data$f_end - ((end_cut)),
      f_cut = case_when(
        .data$f_datetime < .data$f_start | .data$f_datetime >= .data$f_end
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      n_conc = sum(!is.na(.data$f_conc))
    ) |>
    ungroup()

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na("f_conc") |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_time_cut = difftime(.data$f_datetime[seq_along(.data$f_datetime)],
        .data$f_datetime[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      length_window = max(.data$f_time_cut),
      length_flux = difftime(.data$f_end, .data$f_start, units = "sec"),
      time_diff = .data$f_time - .data$f_time_cut,
      n_conc_cut = sum(!is.na(.data$f_conc))
    ) |>
    ungroup()

  fitting_par <- conc_df_cut |>
    group_by(.data$f_fluxID) |>
    nest() |>
    mutate(
      temp = map(.data$data, \(d) {
        model <- lm(f_conc ~ f_time_cut, data = d)
        glance <- broom::glance(((model))) |>
          select("r.squared", "adj.r.squared", "p.value")
        tidy <- broom::tidy(((model))) |>
          select("term", "estimate") |>
          pivot_wider(names_from = "term", values_from = "estimate")
        bind_cols(((glance)), ((tidy)))
      })
    ) |>
    select(!"data") |>
    unnest("temp") |>
    rename(
      f_slope = "f_time_cut",
      f_intercept = "(Intercept)",
      f_rsquared = "r.squared",
      f_adj_rsquared = "adj.r.squared",
      f_pvalue = "p.value"
    ) |>
    fill("f_intercept", .direction = "down") |>
    drop_na("f_slope") |>
    ungroup()

  conc_fitting <- conc_df |>
    left_join(fitting_par, by = c("f_fluxID")) |>
    mutate(
      f_fit = .data$f_intercept + .data$f_slope * (.data$f_time - ((start_cut)))
    )

  warning_msg <- conc_df |>
    left_join(conc_df_cut, by = c("f_datetime", "f_fluxID", "n_conc")) |>
    select("f_fluxID", "n_conc", "n_conc_cut", "length_flux") |>
    distinct() |>
    mutate(
      low_data = paste(
        "\n", "fluxID", .data$f_fluxID, ": slope was estimated on",
        .data$n_conc_cut, "points out of", .data$length_flux,
        "seconds"
      ),
      no_data = paste(
        "\n", "fluxID", .data$f_fluxID,
        "dropped (no data in the conc column)"
      ),
      warnings = case_when(
        .data$n_conc == 0 ~ .data$no_data,
        .data$n_conc_cut != .data$length_flux ~ .data$low_data
      ),
      warnings = as.character(.data$warnings)
    ) |>
    drop_na(warnings) |>
    pull(.data$warnings)

  warnings <- str_c(warning_msg)

  if (any(!is.na(warnings))) warning(warnings)


  conc_fitting
}
