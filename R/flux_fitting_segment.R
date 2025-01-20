#' Fitting function while accounting for leaks
#' @description separate measurements in linear segments segments
#' @param conc_df dataframe of gas concentration over time
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param par_col column containing PAR data (segments; optional).
#' @param h2o_col column containing water vapour concentration
#' (segments; optional).
#' @param signal_strength_col column containing signal strength
#' (segments; optional).
#' @param h2o_correction Should the gas concentration be corrected
#' for water vapor? (logical; segments)
#' @param min_seg_length minimum length (in seconds) for each segments
#' (segments). Note that the minimum segment length cannot be longer than half
#' the length of the measurements.
#' @return a df with modeled gas concentration in linear segments, with slope,
#' r squared, adjusted r squared, p-value, average PAR (if provided) and
#' signal strength (if provided) for each segments.
#' @importFrom cpop cpop
#' @importFrom dplyr mutate rename select group_by case_when ungroup arrange
#' filter distinct pull left_join
#' @importFrom tidyselect any_of all_of
#' @importFrom forcats as_factor
#' @importFrom stringr str_c
#' @importFrom tidyr drop_na
#' @importFrom progress progress_bar
#' @importFrom stats lm predict



flux_fitting_segment <- function(conc_df,
                                 start_cut,
                                 end_cut,
                                 par_col,
                                 h2o_col,
                                 sign_str_col,
                                 h2o_correction,
                                 min_seg_length) {

  args_ok_seg <- flux_fun_check(list(
    h2o_correction = ((h2o_correction)),
    min_seg_length = ((min_seg_length))
  ),
  fn = list(is.logical, is.numeric),
  msg = c("has to be a logical", "has to be numeric"))

  if (!is.null(((sign_str_col)))) {

    sign_str_check <- conc_df |>
      select(
        all_of(((sign_str_col)))
      )

    sign_str_ok <- flux_fun_check(sign_str_check,
                                  fn = list(is.numeric),
                                  msg = c("has to be numeric"),
                                  origdf = conc_df)

    conc_df <- conc_df |>
      rename(
        f_signal_strength = all_of(((sign_str_col)))
      )
  }

  if (is.null(((sign_str_col)))) {
    conc_df <- conc_df |>
      mutate(
        f_signal_strength = NA_real_
      )
    sign_str_ok <- TRUE
  }

  if (!is.null(((par_col)))) {

    par_check <- conc_df |>
      select(
        all_of(((par_col)))
      )

    par_ok <- flux_fun_check(par_check,
                             fn = list(is.numeric),
                             msg = c("has to be numeric"),
                             origdf = conc_df)

    conc_df <- conc_df |>
      rename(
        f_par = all_of(((par_col)))
      )
  }

  if (is.null(((par_col)))) {
    conc_df <- conc_df |>
      mutate(
        f_par = NA_real_
      )
    par_ok <- TRUE
  }

  if (!is.null(((h2o_col)))) {

    h2o_check <- conc_df |>
      select(
        all_of(((h2o_col)))
      )

    h2o_ok <- flux_fun_check(h2o_check,
                             fn = list(is.numeric),
                             msg = c("has to be numeric"),
                             origdf = conc_df)

    conc_df <- conc_df |>
      rename(
        f_h2o_conc = all_of(((h2o_col)))
      )
  }

  if (is.null(((h2o_col)))) {
    conc_df <- conc_df |>
      mutate(
        f_h2o_conc = NA_real_
      )
    h2o_ok <- TRUE
    h2o_correction <- FALSE
  }


  if (any(!c(args_ok_seg, sign_str_ok, par_ok, h2o_ok)))
    stop("Please correct the arguments", call. = FALSE)

  segmented_fluxes <- tibble()

  message("Cutting measurements...")

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
      n_conc = sum(!is.na(.data$f_conc)),
      f_flag_fit = case_when(
        ((min_seg_length)) >
          ((.data$n_conc - ((start_cut)) - ((end_cut))) / 2) ~ "too short",
      ),
      f_cut = case_when(
        .data$f_datetime < .data$f_start | .data$f_datetime >= .data$f_end
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      f_conc = case_when(
        h2o_correction == TRUE ~ f_conc / (1 - (f_h2o_conc / 1000)),
        h2o_correction == FALSE ~ f_conc
      ),
      corrected_for_water_vapor = case_when(
        h2o_correction == TRUE ~ "yes",
        h2o_correction == FALSE ~ "no"
      )
    ) |>
    ungroup() |>
    arrange("f_datetime")

  short_df <- conc_df |>
    filter(
      .data$f_flag_fit == "too short"
    ) |>
    select("f_fluxID") |>
    distinct() |>
    mutate(
      f_warning = paste(
        "\n", "fluxID", .data$f_fluxID,
        "dropped: measurement too short to find changing points."
      ),
      f_warning = as.character(.data$f_warning)
    ) |>
    pull(.data$f_warning)

  explanation <- paste(
    "\n",
    "\n",
    "If the measurement is shorter than double the minimum segment length,",
    "\n",
    "there cannot be changepoints."
  )
  f_warning <- str_c(short_df, explanation)

  if (any(!is.na(conc_df$f_flag_fit))) warning(f_warning)

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
      & is.na(.data$f_flag_fit)
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
    ungroup() |>
    select(
      any_of(c(
        "f_fluxID",
        "f_conc",
        "f_datetime",
        "f_time_cut",
        "f_h2o_conc",
        "f_par",
        "f_signal_strength"
      ))
    )


  message("Starting segmentation...")

  pb <- progress_bar$new(
    format =
      "Segmenting flux :current out of :total [:bar] (:percent)",
    total = length(unique(conc_df_cut$f_fluxID))
  )
  pb$tick(0)
  Sys.sleep(3)

  for (flux in unique(conc_df_cut$f_fluxID)){

    pb$tick()
    Sys.sleep(0.1)

    dt_sub <- conc_df_cut |>
      filter(.data$f_fluxID == flux)

    res <- suppressMessages(cpop(
      dt_sub$f_conc, minseglen = ((min_seg_length))
    ))
    f_conc_seg <- cpop::fitted(res)

    segs <- seq_len(nrow(f_conc_seg))

    dt_sub <- dt_sub |>
      mutate(f_fit = as.numeric(NA),
             f_slope = as.numeric(NA),
             f_rsquared = as.numeric(NA),
             f_adj_rsquared = as.numeric(NA),
             f_pvalue = as.numeric(NA),
             f_segment_id = as.character(NA),
             f_par_seg = as.numeric(NA),
             f_sign_str_seg = as.numeric(NA),
             f_segment_length = as.numeric(NA))

    for (s in segs){
      s1 <- f_conc_seg$x0[s] + 1
      s2 <- f_conc_seg$x1[s] + 1
      time_m <- dt_sub$f_time_cut[s1:s2] - (dt_sub$f_time_cut[s1] - 1)
      linear_fit <- lm(dt_sub$f_conc[s1:s2] ~ (time_m))

      dt_sub[s1:s2, ]$f_slope <- as.numeric(linear_fit$coeff[2])
      dt_sub[s1:s2, ]$f_rsquared <- as.numeric(summary(linear_fit)$r.sq)
      dt_sub[s1:s2, ]$f_adj_rsquared <- as.numeric(
        summary(linear_fit)$adj.r.squared
      )
      dt_sub[s1:s2, ]$f_pvalue <- as.numeric(
        summary(linear_fit)$coefficients["time_m", 4]
      )
      dt_sub[s1:s2, ]$f_par_seg <- mean(dt_sub$f_par[s1:s2])
      dt_sub[s1:s2, ]$f_sign_str_seg <- mean(dt_sub$f_signal_strength[s1:s2])
      dt_sub[s1:s2, ]$f_segment_id <- paste0("segment_", s)
      dt_sub[s1:s2, ]$f_segment_length <- length(time_m)


      if (h2o_correction == TRUE) {
        dt_sub[s1:s2, ]$f_fit <- predict(
          linear_fit
        ) * (1 - (mean(dt_sub$f_h2o_conc[s1:s2]) / 1000))
      }else if(h2o_correction == FALSE) {
        dt_sub[s1:s2, ]$f_fit <- predict(linear_fit)
      }
    }

    dt_sub <- dt_sub  |>
      mutate(
        f_cut = case_when(
          !is.na(f_slope) ~ "keep",
          is.na(f_slope) ~ "cut"
        )
      )

    segmented_fluxes <- rbind(dt_sub, segmented_fluxes)
  }

  segmented_fluxes_final <- segmented_fluxes |>
    mutate(
      f_fluxID = as_factor(.data$f_fluxID),
      f_cut = as_factor(.data$f_cut)
    ) |>
    select(!c("f_par", "f_signal_strength", "f_h2o_conc"))

  conc_df <- conc_df |>
    mutate(
      f_fluxID = as_factor(.data$f_fluxID)
    )

  conc_fitting <- conc_df |>
    left_join(
      segmented_fluxes_final,
      by = c("f_fluxID", "f_datetime", "f_conc", "f_cut")
    ) |>
    arrange(.data$f_datetime)

  if (is.null(((sign_str_col)))) {
    conc_fitting <- conc_fitting |>
      select(!c("f_signal_strength"))
  }

  if (is.null(((par_col)))) {
    conc_fitting <- conc_fitting |>
      select(!c("f_par"))
  }

  if (is.null(((h2o_col)))) {
    conc_fitting <- conc_fitting |>
      select(!c("f_h2o_conc"))
  }

  conc_fitting
}
