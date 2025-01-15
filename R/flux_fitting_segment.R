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
#' flag cut/keep indicating if the row is used in a segment or not (needed for plotting)
#' ideally flux_segment would be integrated in flux_fitting as an option


flux_fitting_segment <- function(
    conc_df,
    start_cut,
    end_cut,
    # start_col,
    # end_col,
    # signal_strength_tresh = c(),
    # par_thresh = c(),
    # conc_col,  # Parameter to analyze, default is CO2
    par_col,  # Column name for PAR in flux_df
    # datetime_col,  # Column name for datetime in flux_df
    # co2_col = "co2_conc",  # Column name for CO2 concentration in flux_df
    h2o_col,  # Column name for H2O concentration in flux_df
    signal_strength_col,  # Column name for signal strength in flux_df
    # flux_type_col = "measurement",  # Column name for flux type in flux_df
    # day_night_col = "day_night",
    # fluxid_col,
    # start_time_col = "start_time",
    # min_length = 60,  # minimum flux length 
    # skip = 7, #number of rows to skip from each flux measurement, 
    h2o_correction,
    min_seg_length
) {

  if (!is.na(((signal_strength_col)))) {
    conc_df <- conc_df |>
    rename(
      signal_strength = all_of((signal_strength_col))
    )
  }

  if (!is.na(((par_col)))) {
    conc_df <- conc_df |>
    rename(
      par = all_of((par_col))
    )
  }

  if (!is.na(((h2o_col)))) {
    conc_df <- conc_df |>
    rename(
      h2o_conc = all_of((h2o_col))
    )
  }

  if (is.na(((h2o_col)))) {
    h2o_correction <- FALSE
  }

  
  
  
  # if(is.null(flux_df)){print("Please provide a dataframe with gas concentrations")}
  
  segmented_fluxes <- tibble()
  
  # flux <- "data/rawData/LI7500/LI7500_Site 5//5_2800_west_5_day_photo.txt"
  # Loop through each unique flux measurement file_name in the flux data frame
  
  ####flux <- t[3]
  # for(flux in unique(flux_df[[flux_id_col]])){
    
  #   if(is.na(flux)){next}  # Skip if the flux file_name is NA
    
  #   # Subset the flux data frame for the current file_name and keep distinct rows
  #   dt_sub <- flux_df[flux_df[[flux_id_col]] == {{flux}}, ] %>% unique()
    
  #   if(nrow(dt_sub) < min_length + skip){
  #     print(paste0("Flux ", flux, " is too short (", nrow(dt_sub), " rows). Let's skip this one."))
      
  #     next
  #   }  # Skip if n is too small 
    # this should be a warning at the end of the process

    
    # Get the unique flux type from the subset data
    # flux_type <- unique(dt_sub[[flux_type_col]]) #%>% pull()
    
    # If more than one flux type is found, get the mode to select the more common one 
    # if(length(flux_type) > 1){
      
      # print(paste0("Warning: more than one flux type detected for flux with id: ", flux))
      
      # table(dt_sub[[flux_type_col]])  
      
    #   Mode <- function(x, na.rm = TRUE) {
    #     if(na.rm){x = x[!is.na(x)]}
    #     ux <- unique(x)  
    #     return(ux[which.max(tabulate(match(x, ux)))])  # Return the most frequent value (mode)
    #   }
      
    #   flux_type <- Mode(dt_sub[[flux_type_col]])  # Assign mode to flux_type
    #   # this should usually not be necessary 
    # }
    
    # # Skip if the flux type is ambient measurements
    # if(flux_type == "a"){next} 
    
    # # Define ambient measurement for comparison
    
    # # Check if the measurement is a night measurement or respiration
    # check_night_resp <- FALSE
    
    # if(grepl("resp", flux, fixed = TRUE)){ 
    #   check_night_resp <- TRUE 
    # }
    
    # if(grepl("resp", unique(dt_sub[[flux_type_col]]))){ 
    #   check_night_resp <- TRUE 
    # }
    
    
    # ## remove first rows of the flux 
    
    # dt_sub <- dt_sub[!1:skip, ]
    
    
    ## Define parameters for calculations
    # R <- 8.314472  # Universal gas constant in J/(K·mol)
    
    # Prepare variables for calculations
    # time <- as.numeric(dt_sub[[date_time_col]]) - min(as.numeric(dt_sub[[date_time_col]]))  # Time since the start of the measurement
    # co2 <- as.numeric(dt_sub[[co2_col]])  # CO2 concentration
    # h2o <- as.numeric(dt_sub[[h2o_col]])  # H2O concentration

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
        ((min_seg_length)) > ((.data$n_conc - ((start_cut)) - ((end_cut))) / 2) ~ "too short",
      ),
      f_cut = case_when(
        .data$f_datetime < .data$f_start | .data$f_datetime >= .data$f_end
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      f_conc = case_when(
              h2o_correction == TRUE ~ f_conc / (1 - (h2o_conc / 1000)),
              h2o_correction == FALSE ~ f_conc
            ),
      corrected_for_water_vapor = case_when(
              h2o_correction == TRUE ~ "yes",
              h2o_correction == FALSE ~ "no"
             )
    ) |>
    ungroup()

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
    select(any_of(c("f_fluxID", "f_conc", "f_datetime", "f_time_cut", "h2o_conc", "par", "signal_strength")))
    
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
    filter(f_fluxID == flux)
    # unique()
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop::cpop(dt_sub$f_conc, minseglen = min_seg_length))  # Identify change points with a minimum segment length of 30 seconds
    # changepoints(res)  # Extract change points from the result
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
                time_m <- dt_sub$f_time_cut[s1:s2] - (dt_sub$f_time_cut[s1])

        
        # Fit a linear model to the current segment
        # linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time_m))  
                linear.fit <- stats::lm(dt_sub$f_conc[s1:s2] ~ (time_m))  

        # replce by mutate?
        dt_sub[s1:s2, ]$f_slope <- as.numeric(linear.fit$coeff[2])
        # dt_sub[s1:s2, ]$f_time <- c(time_m)
        dt_sub[s1:s2, ]$f_rsquared <- as.numeric(summary(linear.fit)$r.sq)
        dt_sub[s1:s2, ]$f_adj_rsquared <- as.numeric(summary(linear.fit)$adj.r.squared)
        dt_sub[s1:s2, ]$f_pvalue <- as.numeric(summary(linear.fit)$coefficients["time_m", 4])
        dt_sub[s1:s2, ]$f_par_seg <- mean(dt_sub$par[s1:s2])
        dt_sub[s1:s2, ]$f_sign_str_seg <- mean(dt_sub$signal_strength[s1:s2])
        dt_sub[s1:s2, ]$f_segment_id <- paste0("segment_", s)
        # dt_sub[s1:s2, ]$f_segment_length <- difftime(dt_sub$f_time_cut[s2], dt_sub$f_time_cut[s1], units = "secs")
        dt_sub[s1:s2, ]$f_segment_length <- length(time_m)

        
        # use case when
        if(h2o_correction == TRUE){
          dt_sub[s1:s2, ]$f_fit <- predict(linear.fit)*(1 - (mean(dt_sub$h2o_conc[s1:s2]) / 1000)) 
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
      )
      # select("f_fluxID", "f_datetime", "f_conc", "f_cut", "f_slope")

  conc_df <- conc_df |>
    select(!any_of(c("h2o_conc", "signal_strength", "par"))) |>
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
