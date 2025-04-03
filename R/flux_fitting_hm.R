#' Fitting a model to the gas concentration curve and estimating the slope over
#' time, using the HM model
#' (Pedersen et al., 2010; Hutchinson and Mosier, 1981).
#' @references Pedersen, A.R., Petersen, S.O., Schelde, K., 2010.
#' A comprehensive approach to soil-atmosphere trace-gas flux estimation with
#' static chambers. European Journal of Soil Science 61, 888–902.
#' https://doi.org/10.1111/j.1365-2389.2010.01291.x
#' @references Hutchinson, G.L., Mosier, A.R., 1981. Improved Soil Cover Method
#' for Field Measurement of Nitrous Oxide Fluxes.
#' Soil Science Society of America Journal 45, 311–316.
#' https://doi.org/10.2136/sssaj1981.03615995004500020017x
#' @description Fits the exponential expression to the concentration evolution
#' `C(t) = C_m + (C_z - C_m) exp(-b * t)`
#' @param conc_df dataframe of gas concentration over time
#' @param conc_df_cut dataframe of gas concentration over time, cut
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each concentration measurement
#' Note that if there are duplicated datetime in the same f_fluxid only
#' the first row will be kept
#' @param f_start column with datetime when the measurement started
#' @param f_end column with datetime when the measurement ended
#' @param f_fluxid column with ID of each flux
#' @param cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz
#' where it is assumed that C fits the data perfectly
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as cz_window
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param t_zero time at which the slope should be calculated
#' (for quadratic fit)
#' @return a dataframe with the slope at t zero,
#' modeled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename mutate select group_by case_when
#' ungroup filter distinct left_join rowwise summarize pull slice
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data
#' @importFrom broom tidy



flux_fitting_hm <- function(conc_df_cut,
                            conc_df,
                            conc_col,
                            datetime_col,
                            f_start,
                            f_end,
                            f_fluxid,
                            cz_window,
                            b_window,
                            roll_width,
                            start_cut,
                            end_cut,
                            t_zero) {

  args_ok <- flux_fun_check(list(
    cz_window = cz_window,
    b_window = b_window,
    roll_width = roll_width,
    t_zero = t_zero
  ),
  fn = list(
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric
  ),
  msg = rep("has to be numeric", 4))

  if (any(!args_ok))
    stop("Please correct the arguments", call. = FALSE)


  message("Cutting measurements...")

  name_conc <- names(select(conc_df, {{conc_col}}))


  message("Estimating starting parameters for optimization...")



  # cm_temp_min <- conc_df_cut |>
  #   group_by({{f_fluxid}}) |>
  #   select({{f_fluxid}}, {{conc_col}}, "f_time_cut") |>
  #   distinct(.data[[name_conc]], .keep_all = TRUE) |>
  #   dplyr::slice(which.min(.data[[name_conc]])) |>
  #   rename(
  #     Cmin = {{conc_col}},
  #     tmin = "f_time_cut"
  #   ) |>
  #   ungroup()

  # cm_temp_max <- conc_df_cut |>
  #   group_by({{f_fluxid}}) |>
  #   select({{f_fluxid}}, {{conc_col}}, "f_time_cut") |>
  #   distinct(.data[[name_conc]], .keep_all = TRUE) |>
  #   dplyr::slice(which.max(.data[[name_conc]])) |>
  #   rename(
  #     Cmax = {{conc_col}},
  #     tmax = "f_time_cut"
  #   ) |>
  #   ungroup()

  # cm_temp <- left_join(cm_temp_max, cm_temp_min,
  #   by = dplyr::join_by({{f_fluxid}})
  # )



  # cm_slope <- conc_df_cut |>
  #   group_by({{f_fluxid}}) |>
  #   nest() |>
  #   mutate(
  #     model_Cm =
  #       map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
  #     tidy = map(.data$model_Cm, broom::tidy)
  #   ) |>
  #   unnest("tidy") |>
  #   filter(.data$term == "f_time_cut") |>
  #   rename(slope_Cm = "estimate") |>
  #   unnest({{f_fluxid}}) |>
  #   select({{f_fluxid}}, "slope_Cm")



  # cm_df <- left_join(cm_temp, cm_slope, by = dplyr::join_by({{f_fluxid}})) |>
  #   mutate(
  #     f_Cm_est = case_when(
  #       .data$slope_Cm < 0 ~ .data$Cmin,
  #       .data$slope_Cm > 0 ~ .data$Cmax
  #     ),
  #     tm = case_when(
  #       .data$slope_Cm < 0 ~ .data$tmin,
  #       .data$slope_Cm > 0 ~ .data$tmax
  #     ),
  #     f_Cm_est = replace(.data$f_Cm_est, .data$f_Cm_est <= 0, 1e-10),
  #     .by = {{f_fluxid}}
  #   ) |>
  #   select({{f_fluxid}}, "f_Cm_est", "tm", "slope_Cm")


  # cz_df <- conc_df_cut |>
  #   filter(
  #     .data$f_time_cut <= cz_window
  #   ) |>
  #   group_by({{f_fluxid}}) |>
  #   nest() |>
  #   mutate(
  #     model_Cz =
  #       map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
  #     tidy = map(.data$model_Cz, broom::tidy)
  #   ) |>
  #   unnest("tidy") |>
  #   filter(.data$term == "(Intercept)") |>
  #   rename(f_Cz_est = "estimate") |>
  #   unnest({{f_fluxid}}) |>
  #   select({{f_fluxid}}, "f_Cz_est") |>
  #   ungroup()


  # cb_df <- conc_df_cut |>
  #   group_by({{f_fluxid}}) |>
  #   mutate(
  #     diff = .data$f_time_cut + b_window
  #   ) |>
  #   distinct(.data$diff, .keep_all = TRUE) |>
  #   dplyr::slice(which.min(abs(.data$diff))) |>
  #   rename(f_Cb = {{conc_col}}) |>
  #   select({{f_fluxid}}, "f_Cb") |>
  #   ungroup()


  # estimates_df <- left_join(cm_df, cz_df,
  #   by = dplyr::join_by({{f_fluxid}})
  # ) |>
  #   left_join(cb_df, by = dplyr::join_by({{f_fluxid}})) |>
  #   mutate(
  #     f_b_est = case_when(
  #       .data$f_Cb == .data$f_Cm_est ~ 0, # special case or flat flux
  #       .data$f_Cz_est == .data$f_Cm_est ~ 0, # special case or flat flux
  #       TRUE ~ log(
  #         abs((.data$f_Cb - .data$f_Cm_est) / (.data$f_Cz_est - .data$f_Cm_est))
  #       )
  #       * (1 / b_window)
  #     ),
  #     f_b_est = replace(.data$f_b_est, .data$f_b_est <= 0, 1e-10)
  #   )




  # fc_myfn <- function(fc_time, fc_conc, par) {
  #   sqrt(
  #     (1 / length(fc_time))
  #     * sum((exp(par[1]) + (par[3] - exp(par[1]))
  #            * exp(-exp(par[2]) * fc_time)
  #            - fc_conc)^2)
  #   )
  # }


  # message("Optimizing fitting parameters...")

  # fitting_par <- conc_df_cut |>
  #   left_join(estimates_df, by = dplyr::join_by({{f_fluxid}})) |>
  #   select(
  #     {{f_fluxid}}, "f_Cm_est", "f_b_est",
  #     "f_Cz_est", "f_time_cut", {{conc_col}}, "f_time_diff"
  #   ) |>
  #   group_by(
  #     {{f_fluxid}}, .data$f_Cm_est, .data$f_b_est,
  #     .data$f_Cz_est, .data$f_time_diff
  #   ) |>
  #   nest() |>
  #   rowwise() |>
  #   summarize(
  #     results = list(tryCatch(
  #       optim(
  #         par = c(
  #           log(.data$f_Cm_est), log(.data$f_b_est), .data$f_Cz_est
  #         ),
  #         fn = fc_myfn, fc_conc = data[name_conc],
  #         fc_time = data$f_time_cut#, fc_cz = .data$f_Cz
  #       ),
  #       error = function(err) list(par = rep(NA, 3))
  #     )),
  #     f_Cm = exp(.data$results$par[1]),
  #     f_b = exp(.data$results$par[2]),
  #     f_Cz = .data$results$par[3],
  #     f_slope = .data$f_b * (.data$f_Cm - .data$f_Cz) *
  #       exp(-.data$f_b * t_zero),
  #     .groups = "drop"
  #   ) |>
  #   select(!c("results", "f_Cm_est", "f_b_est"))

   fitting_par <- conc_df_cut |>
    select(
      {{f_fluxid}}, "f_time_cut", {{conc_col}}, "f_time_diff"
    ) |>
    group_by(
      {{f_fluxid}}, .data$f_time_diff
    ) |>
    nest() |>
    # rowwise() |>
    mutate(
      model = tryCatch(map(.x = data, \(.x) nls(.x[[name_conc]] ~ cbind(1, exp(-exp(logb) * f_time_cut)/(-exp(logb))), 
               start = c(logb = log(1.5)), algorithm = "plinear",
               control=nls.control(maxiter=100, minFactor=1e-10, scaleOffset = 1), data = .x)),
               error = function(err) list(par = rep(NA, 3))),
      # glance = map(.data$model, broom::glance),
      tidy = map(.data$model, broom::tidy)
     ) |>
     select(!c("data", "model")) |>
    unnest("tidy") |>
    select({{f_fluxid}}, "term", "estimate", "f_time_diff") |>
    pivot_wider(names_from = "term", values_from = "estimate") |>
    rename(
      f_Cm = ".lin1",
      f_slope_z = ".lin2"
    ) |>
    mutate(
      f_b = exp(logb),
      f_Cz = .data$f_Cm - (.data$f_slope_z / .data$f_b),
      f_slope = .data$f_slope_z * exp(-.data$f_b * t_zero)
    ) |>
    select(!"logb")
        
# fitting_par

    

    # mutate(
  #     model_Cm =
  #       map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
  #     tidy = map(.data$model_Cm, broom::tidy)
  #   ) |>
  #   unnest("tidy") |>
  #   filter(.data$term == "f_time_cut") |>
  #   rename(slope_Cm = "estimate") |>
  #   unnest({{f_fluxid}}) |>
  #   select({{f_fluxid}}, "slope_Cm")

  message("Calculating fits and slopes...")

  conc_fitting <- conc_df |>
    left_join(fitting_par, by = dplyr::join_by({{f_fluxid}})) |>
    mutate(
      f_fit = .data$f_Cm + .data$f_slope_z * (exp(-.data$f_b * (.data$f_time - .data$f_time_diff)) / (-.data$f_b)),
      # f_fit = .data$f_Cm + (.data$f_Cz - .data$f_Cm)
      # * exp(-.data$f_b * (.data$f_time - .data$f_time_diff)),
      # f_fit_slope = 
      f_fit_slope = .data$f_Cm
      + (.data$f_Cz - .data$f_Cm) * exp(-.data$f_b * t_zero)
      - .data$f_slope * (t_zero - .data$f_time),
      f_start_z = {{f_start}} + t_zero
      # .by = {{f_fluxid}}
    ) |>
    select(!c("f_time_diff", "f_slope_z"))


  message("Done.")


  conc_fitting
}
