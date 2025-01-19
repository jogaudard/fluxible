#' Fitting function while accounting for leaks
#' @description separate fluxes in segments of similar slopes with stable PAR
#' @param flux_df
#' @param signal_strength_tresh Threshold for valid signal strength
#' @param par_thresh Photosynthetically Active Radiation (PAR) threshold
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @return a df with modeled gas concentration (NA outside of selected segments)
#' @importFrom cpop cpop
#' @importFrom ggplot2
#' @importFrom dplyr
#' @importFrom lubridate
#' @importFrom reshape2
#' @importFrom cowplot
#' @importFrom sarima
#' @importFrom data.table


flux_fitting_segment <- function(
  conc_df,
  start_cut,
  end_cut,
  par_col,
  h2o_col,
  signal_strength_col,
  h2o_correction,
  min_seg_length
) {

args_ok_seg <- flux_fun_check(list(
    h2o_correction = ((h2o_correction)),
    min_seg_length = ((min_seg_length))
  ),
  fn = list(is.logical, is.numeric),
  msg = c("has to be a logical", "has to be numeric"))

  if (!is.null(((signal_strength_col)))) {

    sign_str_check <- conc_df |>
      select(
        all_of(((signal_strength_col)))
      )

    sign_str_ok <- flux_fun_check(sign_str_check,
                                 fn = list(is.numeric),
                                 msg = c("has to be numeric"),
                                 origdf = conc_df)

    conc_df <- conc_df |>
      rename(
        f_signal_strength = all_of(((signal_strength_col)))
      )
  }

  if (is.null(((signal_strength_col)))) {
    conc_df <- conc_df |>
      mutate(
        f_signal_strength = NA_real_
      )
    message("f_signal_strength column added")
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
    message("f_par column added")
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
    message("f_h2o_conc column added")
    h2o_ok <- TRUE
  }

  

  # conc_df_check <- conc_df |>
  #   select(
  #     all_of(((par_col))),
  #     all_of(((h2o_col))),
  #     all_of(((signal_strength_col)))
  #   )

  # conc_df_ok <- flux_fun_check(conc_df_check,
  #                              fn = list(
  #                                is.numeric,
  #                                is.numeric,
  #                                is.numeric
  #                              ),
  #                              msg = rep(
  #                                "has to be numeric", 3
  #                              ),
  #                              origdf = conc_df)


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
      "there cannot be changepoints.")
    f_warning <- stringr::str_c(short_df, explanation)

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
    select(any_of(c("f_fluxID", "f_conc", "f_datetime", "f_time_cut", "f_h2o_conc", "f_par", "f_signal_strength")))
    
    # # Determine PAR values; if no PAR data is available or if it's a respiration measurement, set PAR to threshold + 1
    # if(check_night_resp == TRUE | sum(is.na(dt_sub[[par_col]])) == nrow(dt_sub)){ 
    #   par <- rep(par_thresh + 1, nrow(dt_sub))  # Default PAR value
    # }else{ 
    #   par <- as.numeric(dt_sub[[par_col]])  # Use actual PAR data
    # }
    
    # Prepare other parameters for calculations
    # signal_strength <- as.numeric(dt_sub[[signal_strength_col]])  # Signal strength
    
    # if(correct_for_h2o_conc == TRUE){
      # Calculate c' and w' with respect to the H2O concentration
      # conc_df_cut <- conc_df_cut |>
      #     group_by(.data$f_fluxID) |>
      #   mutate(
      #     f_conc = case_when(
      #         h2o_correction == TRUE ~ f_conc / (1 - (h2o_conc / 1000)),
      #         h2o_correction == FALSE ~ f_conc
      #       )
      #       c_prime = case_when(
      #         correct_for_h2o_conc == TRUE ~ co2 / (1 - (h2o / 1000)),
      #         correct_for_h2o_conc == FALSE ~ co2
      #       ),
      # w_prime = case_when(
      #         correct_for_h2o_conc == TRUE ~ h2o / (1 - (h2o / 1000)),
      #         correct_for_h2o_conc == FALSE ~ h2o
      # ),
      # wav_dil = mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
      # cw_prime = case_when(
      #   param == "co2" ~ c_prime,
      #   param == "h2o" ~ w_prime
      # ) |>
      # ungroup() |>
          # select(any_of(c("f_fluxID", "f_conc", "f_datetime", "f_time_cut", "h2o_conc", "par", "signal_strength")))
     
      
      # c_prime <- co2 / (1 - (h2o / 1000))  
      # w_prime <- h2o / (1 - (h2o / 1000))  
      # wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    # }else{
      # c_prime <- co2  
      # w_prime <- h2o  
      # wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    # }
    
    # if ("co2" == param) {  
    #   cw_prime <- c_prime  # Use c_prime for CO2
    #   tag <- "c_prime"  
    # } else if ("h2o" == param) {  
    #   cw_prime <- w_prime  # Use w_prime for H2O
    #   tag <- "w_prime"  
    # }

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
    
  #   if(is.na(flux)){next}  # Skip if the flux file_name is NA
    
  #   # Subset the flux data frame for the current file_name and keep distinct rows
  #   dt_sub <- flux_df[flux_df[[flux_id_col]] == {{flux}}, ] %>% unique()
  dt_sub <- conc_df_cut |>
    filter(.data$f_fluxID == flux)
    # unique()
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop::cpop(dt_sub$f_conc, minseglen = ((min_seg_length))))  # Identify change points with a minimum segment length of 30 seconds
    # cpop::changepoints(res)  # Extract change points from the result
    f_conc_seg <- cpop::fitted(res)  # Get the fitted values from the change point analysis
    
    # Create a sequence of segment indices based on the number of rows in f_conc_seg data frame
    segs <- c(1:nrow(f_conc_seg))  
    
    dt_sub <- dt_sub |>
      mutate(f_fit = as.numeric(NA),
             f_slope = as.numeric(NA),
            #  f_time_cut = as.numeric(NA),
             f_rsquared = as.numeric(NA),
             f_adj_rsquared = as.numeric(NA),
             f_pvalue = as.numeric(NA),
             f_segment_id = as.character(NA),
             f_par_seg = as.numeric(NA),
             f_sign_str_seg = as.numeric(NA),
             f_segment_length = as.numeric(NA))

 
    # Loop over each segment to perform calculations
    for (s in segs){
      # also mutate, and get rid of +1?
      s1 <- f_conc_seg$x0[s] + 1 # Starting index for current segment
      s2 <- f_conc_seg$x1[s] + 1 # Ending index for current segment
      
      # dt_sub <- dt_sub |>
      #   mutate(
      #     s1 = f_conc_seg$x0[s],
      #     s2 = f_conc_seg$x1[s],
      #     time_m = f_time_cut - s1,
      #     f_fit = case_when(
      #       h2o_correction == TRUE ~ predict(linear.fit)*(1 - (mean(h2o[s1:s2]) / 1000)),
      #       h2o_correction == FALSE ~ predict(linear.fit)
      #     ),
      #     f_slope = ,
      #     f_time = ,
      #     f_rsq = ,
      #     f_rsq_adj = ,
      #     f_pval = ,
      #     f_segment_id = ,
      #     corrected_for_water_vapor = h2o_correction,
      #     f_par_seg = ,
      #     f_sign_str_seg = 
      #   )

# linear.fit <- stats::lm(dt_sub$f_conc[s1:s2] ~ (time_m))
      
      # Calculate the mean signal strength and PAR for the current segment
      # if (is.na(((signal_strength_col)))) {
      #   mean_si_st <- "no_si_st"
      # } else {
      #   mean_si_st <- mean(signal_strength[s1:s2])
      # }

      # if (is.na(((par_col)))) {
      #   mean_par <- "no_par"
      # } else {
      #   mean_par <- mean(par[s1:s2])
      # }
        
      
      # Select the concentration variable based on the parameter specified
      # Tag to identify the parameter being processed
      # if ("co2" == param) {  
      #   cw_prime <- c_prime  # Use c_prime for CO2
      #   tag <- "c_prime"  
      # } else if ("h2o" == param) {  
      #   cw_prime <- w_prime  # Use w_prime for H2O
      #   tag <- "w_prime"  
      # }
      
      # Proceed only if both signal strength and PAR exceed the respective thresholds
      # if ((mean_si_st > signal_strength_tresh || mean_si_st == "no_si_st")
      # && (mean_par > par_thresh || mean_par == "no_par")) {
        
        # time_m <- time[s1:s2] - (time[s1] - 1)
                time_m <- dt_sub$f_time_cut[s1:s2] - (dt_sub$f_time_cut[s1] - 1)

        
        # Fit a linear model to the current segment
        # linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time_m))  
                linear.fit <- stats::lm(dt_sub$f_conc[s1:s2] ~ (time_m))  

        # replce by mutate?
        dt_sub[s1:s2, ]$f_slope <- as.numeric(linear.fit$coeff[2])
        # dt_sub[s1:s2, ]$f_time <- c(time_m)
        dt_sub[s1:s2, ]$f_rsquared <- as.numeric(summary(linear.fit)$r.sq)
        dt_sub[s1:s2, ]$f_adj_rsquared <- as.numeric(summary(linear.fit)$adj.r.squared)
        dt_sub[s1:s2, ]$f_pvalue <- as.numeric(summary(linear.fit)$coefficients["time_m", 4])
        dt_sub[s1:s2, ]$f_par_seg <- mean(dt_sub$f_par[s1:s2])
        dt_sub[s1:s2, ]$f_sign_str_seg <- mean(dt_sub$f_signal_strength[s1:s2])
        dt_sub[s1:s2, ]$f_segment_id <- paste0("segment_", s)
        # dt_sub[s1:s2, ]$f_segment_length <- difftime(dt_sub$f_time_cut[s2], dt_sub$f_time_cut[s1], units = "secs")
        dt_sub[s1:s2, ]$f_segment_length <- length(time_m)

        
        # use case when
        if(h2o_correction == TRUE){
          dt_sub[s1:s2, ]$f_fit <- predict(linear.fit)*(1 - (mean(dt_sub$f_h2o_conc[s1:s2]) / 1000)) 
        }else if(h2o_correction == FALSE){
          dt_sub[s1:s2, ]$f_fit <- predict(linear.fit)
          }
        # Print the current segment index
       # print(paste0(s, " successful"))  
      } 
      
    #  print(s)  
      
    # }
    
    dt_sub <- dt_sub  |>
      mutate(
        f_cut = case_when(
          !is.na(f_slope) ~ "keep",
          is.na(f_slope) ~ "cut"
        )
      )
      # mutate(f_cut = ifelse(is.na(f_slope), "cut", "keep"))
    
    #print(setdiff(names(segmented_fluxes), names(dt_sub)))
    #print(setdiff(names(dt_sub), names(segmented_fluxes)))
    
    # Append the new results to the overall results data table
    segmented_fluxes <- rbind(dt_sub, segmented_fluxes)
   
    # print(paste0(flux, " done")) # should replace with a progress bar
    
  }

  segmented_fluxes_final <- segmented_fluxes |>
      mutate(
        f_fluxID = as.factor(.data$f_fluxID),
        f_cut = as.factor(.data$f_cut)
      ) |>
      select(!c("f_par", "f_signal_strength", "f_h2o_conc"))
      # select("f_fluxID", "f_datetime", "f_conc", "f_cut", "f_slope")

  conc_df <- conc_df |>
    # select(!any_of(c("h2o_conc", "signal_strength"))) |>
    mutate(
      f_fluxID = as.factor(f_fluxID)
    )
    # select("f_fluxID", "f_datetime", "f_conc", "f_cut")

  conc_fitting <- conc_df |>
  left_join(
    segmented_fluxes_final,
    by = c("f_fluxID", "f_datetime", "f_conc", "f_cut")
  ) |>
  arrange(f_datetime)

  # Return the combined data table with segmented fluxes
  conc_fitting
  # segmented_fluxes
  # conc_df
}
