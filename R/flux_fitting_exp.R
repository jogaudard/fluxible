#' Fitting a model to the gas concentration curve and estimating the slope
#' @description Fits an exponential expression to the concentration evolution
#' over time, as described in Zhao 2018
#' @param conc_df dataframe of gas concentration over time
#' @param t_window enlarge focus window before and after tmin and tmax
#' @param Cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz
#' where it is assumed that C fits the data perfectly
#' @param a_window window at the end of the flux to estimate a
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as Cz_window
#' @param start_cut to cut at the start
#' @param end_cut to cut at the end, if you notice on the plots
#' that the match was not precise enough
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxID_col column with ID of each flux
#' @return a dataframe with the slope at t zero,
#' modelled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when
#' ungroup filter distinct left_join rowwise summarize pull
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data
#' @examples
#' data(co2_conc)
#' flux_fitting_exp(co2_conc)
#' @export


flux_fitting_exp <- function(conc_df,
                             t_window = 20,
                             Cz_window = 15,
                             b_window = 10,
                             a_window = 10,
                             roll_width = 15,
                             start_cut = 0,
                             end_cut = 0,
                             start_col = "start",
                             end_col = "end",
                             datetime_col = "datetime",
                             conc_col = "conc",
                             fluxID_col = "fluxID") {
  # renaming columns
  conc_df <- conc_df |>
    rename(
      start = all_of((start_col)),
      end = all_of((end_col)),
      datetime = all_of((datetime_col)),
      conc = all_of((conc_col)),
      fluxID = all_of((fluxID_col))
    )



  if (!is.double(t_window)) stop("t_window has to be a double")
  if (!is.double(Cz_window)) stop("Cz_window has to be a double")
  if (!is.double(b_window)) stop("b_window has to be a double")
  if (!is.double(a_window)) stop("a_window has to be a double")
  if (!is.double(roll_width)) stop("roll_width has to be a double")
  if (!is.double(start_cut)) stop("start_cut has to be a double")
  if (!is.double(end_cut)) stop("end_cut has to be a double")




  length_flux_max <- conc_df |>
    mutate(
      length_flux = difftime(.data$end, .data$start, units = "sec"),
      length_flux = as.double(.data$length_flux)
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements! ((start_cut + end_cut) >= length_flux_max)"
    )
  }



  conc_df <- conc_df |>
    group_by(.data$fluxID) |>
    mutate(
      time = difftime(.data$datetime[seq_along(.data$datetime)],
        .data$datetime[1],
        units = "secs"
      ),
      time = as.double(.data$time),
      start = .data$start + ((start_cut)),
      end = .data$end - ((end_cut)),
      cut = case_when(
        .data$datetime < .data$start | .data$datetime >= .data$end ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(.data$cut),
      n_conc = sum(!is.na(.data$conc))
    ) |>
    ungroup()

  conc_df_cut <- conc_df |>
    filter(
      cut == "keep"
    ) |>
    drop_na("conc") |>
    group_by(.data$fluxID) |>
    mutate(
      time_cut = difftime(.data$datetime[seq_along(.data$datetime)],
        .data$datetime[1],
        units = "secs"
      ),
      time_cut = as.double(.data$time_cut),
      length_window = max(.data$time_cut),
      length_flux = difftime(.data$end, .data$start, units = "sec"),
      time_diff = .data$time - .data$time_cut,
      n_conc_cut = sum(!is.na(.data$conc))
    ) |>
    ungroup()


  Cm_temp <- conc_df_cut |>
    group_by(.data$fluxID) |>
    distinct(.data$conc, .keep_all = TRUE) |>
    mutate(
      Cmax = max(.data$conc),
      Cmin = min(.data$conc),
      tmax = .data$time_cut[.data$conc == .data$Cmax],
      tmin = .data$time_cut[.data$conc == .data$Cmin]
    ) |>
    select("fluxID", "Cmax", "Cmin", "tmax", "tmin") |>
    ungroup() |>
    distinct(.data$Cmax, .data$Cmin, .keep_all = TRUE)

  Cm_slope <- conc_df_cut |>
    group_by(.data$fluxID) |>
    nest() |>
    mutate(
      model_Cm = map(.data$data, \(d) {
        lm(conc ~ time_cut, data = d) |>
          broom::tidy() |>
          select("term", "estimate") |>
          pivot_wider(names_from = "term", values_from = "estimate")
      })
    ) |>
    unnest("model_Cm") |>
    rename(slope_Cm = "time_cut") |>
    select("fluxID", "slope_Cm") |>
    ungroup()

  Cm_df <- left_join(Cm_temp, Cm_slope) |>
    mutate(
      Cm_est = case_when(
        # Cm is the max mixing point, which is the lim C(t) when t tends to infinite.
        .data$slope_Cm < 0 ~ .data$Cmin,
        .data$slope_Cm > 0 ~ .data$Cmax
      ),
      tm = case_when(
        .data$slope_Cm < 0 ~ .data$tmin,
        .data$slope_Cm > 0 ~ .data$tmax
      )
    ) |>
    select("fluxID", "Cm_est", "tm", "slope_Cm") |>
    ungroup()

  Cz_df <- conc_df_cut |>
    group_by(.data$fluxID) |>
    filter(
      .data$time_cut <= ((Cz_window))
    ) |>
    nest() |>
    mutate(
      model_Cz = map(.data$data, \(d) {
        lm(conc ~ time_cut, data = d) |>
          broom::tidy() |>
          select("term", "estimate") |>
          pivot_wider(names_from = "term", values_from = "estimate")
      })
    ) |>
    unnest("model_Cz") |>
    rename(
      slope_Cz = "time_cut",
      Cz = "(Intercept)"
    ) |>
    select("fluxID", "slope_Cz", "Cz") |>
    ungroup()



  tz_df <- conc_df_cut |>
    left_join(Cz_df) |>
    group_by(.data$fluxID) |>
    filter(
      # tz should be in the first half of the flux
      .data$time_cut < .data$length_window / 2
    ) |>
    mutate(
      conc_roll = zoo::rollmean(.data$conc,
        k = ((roll_width)),
        fill = NA, align = "right"
      ),
      Cd = abs(.data$conc_roll - .data$Cz),
      minCd = min(.data$Cd, na.rm = TRUE),
      tz_est = min(.data$time_cut[.data$Cd == .data$minCd], na.rm = TRUE)
    ) |>
    ungroup() |>
    select("fluxID", "tz_est") |>
    distinct()



  Cb_df <- conc_df_cut |>
    left_join(tz_df) |>
    group_by(.data$fluxID) |>
    mutate(
      Cb = .data$conc[.data$time_cut == .data$tz_est - ((b_window))]
    ) |>
    ungroup() |>
    select("fluxID", "Cb") |>
    distinct()

  a_df <- conc_df_cut |>
    group_by(.data$fluxID) |>
    mutate(
      ta = .data$length_window - ((a_window)),
      Ca = .data$conc[.data$time_cut == .data$ta]
    ) |>
    ungroup() |>
    select("fluxID", "ta", "Ca") |>
    distinct()

  estimates_df <- left_join(Cm_df, Cz_df) |>
    left_join(tz_df) |>
    left_join(a_df) |>
    left_join(Cb_df) |>
    mutate(
      b_est = case_when(
        .data$Cb == .data$Cm_est ~ 0, # special case or flat flux
        .data$Cz == .data$Cm_est ~ 0, # special case or flat flux
        TRUE ~ log(abs((.data$Cb - .data$Cm_est) / (.data$Cz - .data$Cm_est)))
        * (1 / ((b_window))),
      ),
      a_est = case_when(
        # tz_est = ta is a special case that is undefined
        .data$ta == .data$tz_est ~ 0,
        TRUE ~
          (.data$Ca - .data$Cm_est - (.data$Cz - .data$Cm_est)
          * exp(-.data$b_est * (.data$ta - .data$tz_est)))
          / (.data$ta - .data$tz_est)
      )
    )




  myfn <- function(time, conc, par, Cz) {
    sqrt((1 / length(time)) * sum((par[1] + par[2] * (time - exp(par[4]))
      + (Cz - par[1])
      * exp(-par[3] * (time - exp(par[4])))
      - conc)^2))
  }



  fitting_par <- conc_df_cut |>
    left_join(estimates_df) |>
    select(
      "fluxID", "Cm_est", "a_est", "b_est", "tz_est",
      "Cz", "time_cut", "conc", "time_diff"
    ) |>
    group_by(
      .data$fluxID, .data$Cm_est, .data$a_est, .data$b_est,
      .data$tz_est, .data$Cz, .data$time_diff
    ) |>
    nest() |>
    rowwise() |>
    summarize(
      results = list(optim(
        par = c(
          .data$Cm_est, .data$a_est, .data$b_est,
          log(.data$tz_est)
        ),
        fn = myfn, conc = data$conc,
        time = data$time_cut, Cz = .data$Cz
      )),
      Cm = .data$results$par[1],
      a = .data$results$par[2],
      b = .data$results$par[3],
      tz = exp(.data$results$par[4]), # we force tz to be positive
      slope_tz = .data$a + .data$b * (.data$Cm - .data$Cz),
    ) |>
    ungroup() |>
    select(!"results")


  conc_fitting <- conc_df |>
    left_join(fitting_par) |>
    group_by(.data$fluxID) |>
    mutate(
      fit = .data$Cm + .data$a * (.data$time - .data$tz - .data$time_diff)
        + (.data$Cz - .data$Cm) * exp(-.data$b
        * (.data$time - .data$tz - .data$time_diff)),
      fit_slope = .data$slope_tz * (.data$time) + .data$Cz - .data$slope_tz
        * (.data$tz + .data$time_diff),
      start_z = .data$start + .data$tz
    ) |>
    ungroup()





  warning_msg <- conc_df |>
    left_join(conc_df_cut) |> # we want n_conc after cutting
    select("fluxID", "n_conc", "n_conc_cut", "length_flux") |>
    distinct() |>
    mutate(
      low_data = paste(
        "\n", "fluxID", .data$fluxID, ": slope was estimated on",
        .data$n_conc_cut, "points out of", .data$length_flux,
        "seconds because data are missing"
      ),
      no_data = paste(
        "\n", "fluxID", .data$fluxID,
        ": slope could not be estimated because there are no data in the conc column"
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

  attr(conc_fitting, "fit_type") <- "exponential"

  conc_fitting
}
