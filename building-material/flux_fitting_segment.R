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
#' @importFrom cpop
#' @importFrom ggplot2
#' @importFrom dplyr
#' @importFrom lubridate
#' @importFrom reshape2
#' @importFrom cowplot
#' @importFrom sarima
#' @importFrom data.table
#' flag cut/keep indicating if the row is used in a segment or not (needed for plotting)
#' ideally flux_segment would be integrated in flux_fitting as an option
#' 
#' 
# require()
# require()
# require()
# require()
# require(tidyverse)
# require()
# require()
# require()
# require()

flux_segment <- function(
    conc_df,
    start_cut,
    end_cut,
    signal_strength_tresh = 95.0,
    par_thresh = 650,
    param = "co2",  # Parameter to analyze, default is CO2
    par_col = "par",  # Column name for PAR in flux_df
    date_time_col = "date_time",  # Column name for datetime in flux_df
    co2_col = "co2_conc",  # Column name for CO2 concentration in flux_df
    h2o_col = "h2o_conc",  # Column name for H2O concentration in flux_df
    signal_strength_col = c(),  # Column name for signal strength in flux_df
    flux_type_col = "measurement",  # Column name for flux type in flux_df
    day_night_col = "day_night",
    flux_id_col = "file_name",
    # start_time_col = "start_time",
    min_length = 60,  # minimum flux length 
    skip = 7, #number of rows to skip from each flux measurement, 
    correct_for_h2o_conc = TRUE, 
    min_seg_length = 30
){ 

  if(!is.na(((signal_strength_col))) {
    conc_df <- conc_df |>
    rename(
      signal_strength = all_of((signal_strength_col))
    )
  }

  
  
  
  # if(is.null(flux_df)){print("Please provide a dataframe with gas concentrations")}
  
  # segmented_fluxes <- data.table::data.table() 
  
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
      conc_df <- conc_df |>
          group_by(.data$f_fluxID) |>
        mutate(
            c_prime = case_when(
              correct_for_h2o_conc == TRUE ~ co2 / (1 - (h2o / 1000)),
              correct_for_h2o_conc == FALSE ~ co2
            ),
      w_prime = case_when(
              correct_for_h2o_conc == TRUE ~ h2o / (1 - (h2o / 1000)),
              correct_for_h2o_conc == FALSE ~ h2o
      )
      ,
      wav_dil = mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
        ),
      cw_prime = case_when(
        param == "co2" ~ c_prime,
        param == "h2o" ~ w_prime
      ) |>
      ungroup()
      # c_prime <- co2 / (1 - (h2o / 1000))  
      # w_prime <- h2o / (1 - (h2o / 1000))  
      # wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    # }else{
      # c_prime <- co2  
      # w_prime <- h2o  
      # wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    }
    
    # if ("co2" == param) {  
    #   cw_prime <- c_prime  # Use c_prime for CO2
    #   tag <- "c_prime"  
    # } else if ("h2o" == param) {  
    #   cw_prime <- w_prime  # Use w_prime for H2O
    #   tag <- "w_prime"  
    # }
    for(flux in unique(conc_df$f_fluxid)){
    
  #   if(is.na(flux)){next}  # Skip if the flux file_name is NA
    
  #   # Subset the flux data frame for the current file_name and keep distinct rows
  #   dt_sub <- flux_df[flux_df[[flux_id_col]] == {{flux}}, ] %>% unique()
  dt_sub <- conc_df |>
    filter(f_fluxid == flux) |>
    unique()
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop(cw_prime, minseglen = min_seg_length))  # Identify change points with a minimum segment length of 30 seconds
    changepoints(res)  # Extract change points from the result
    cw_prime_seg <- fitted(res)  # Get the fitted values from the change point analysis
    
    # Create a sequence of segment indices based on the number of rows in cw_prime_seg data frame
    segs <- c(1:nrow(cw_prime_seg))  
    
    dt_sub <- dt_sub %>% 
      mutate(f_fit = as.numeric(NA), 
             f_slope = as.numeric(NA), 
             f_time = as.numeric(NA), 
             f_rsq = as.numeric(NA), 
             f_rsq_adj = as.numeric(NA),
             f_pval = as.numeric(NA),
             f_segment_id = as.character(NA), 
             corrected_for_water_vapor = correct_for_h2o_conc)

 
    # Loop over each segment to perform calculations
    for(s in segs){  
      
      s1 <- cw_prime_seg$x0[s] + 1 # Starting index for current segment
      s2 <- cw_prime_seg$x1[s] + 1 # Starting index for current segment
      
      
      # Calculate the mean signal strength and PAR for the current segment
      if(is.na(((signal_strength_col))) {
        mean_si_st <- "no_si_st"
      } else {
        mean_si_st <- mean(signal_strength[s1:s2])
      }
      mean_par <- mean(par[s1:s2])  
      
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
      if((mean_si_st > signal_strength_tresh | mean_si_st == "no_si_st") && (mean_par > par_thresh)){  
        
        time_m <- time[s1:s2] - (time[s1]-1)
        
        # Fit a linear model to the current segment
        # linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time_m))  
                linear.fit <- stats::lm(dt_sub$cw_prime[s1:s2] ~ (time_m))  

        
        dt_sub[s1:s2, ]$f_slope <- as.numeric(linear.fit$coeff[2])
        dt_sub[s1:s2, ]$f_time <- c(time_m)
        dt_sub[s1:s2, ]$f_rsq <- as.numeric(summary(linear.fit)$r.sq)
        dt_sub[s1:s2, ]$f_rsq_adj <- as.numeric(summary(linear.fit)$adj.r.squared)
        dt_sub[s1:s2, ]$f_pval <- as.numeric(summary(linear.fit)$coefficients["time_m", 4])
        
        dt_sub[s1:s2, ]$f_segment_id <- paste0("segment_", s)

        
        
        if(correct_for_h2o_conc == TRUE){
          dt_sub[s1:s2, ]$f_fit <- predict(linear.fit)*(1 - (mean(h2o[s1:s2]) / 1000)) 
        }else if(correct_for_h2o_conc == FALSE){
          dt_sub[s1:s2, ]$f_fit <- predict(linear.fit)
          }
        # Print the current segment index
       # print(paste0(s, " successful"))  
      } 
      
    #  print(s)  
      
    }
    
    dt_sub <- dt_sub  |>
      mutate(
        f_cut = case_when(
          f_cut == "keep" & !is.na(f_slope) ~ "keep",
          f_cut == "keep" & is.na(f_slope) ~ "cut",
          f_cut == "cut" ~ "cut"
        )
      )
      # mutate(f_cut = ifelse(is.na(f_slope), "cut", "keep"))
    
    #print(setdiff(names(segmented_fluxes), names(dt_sub)))
    #print(setdiff(names(dt_sub), names(segmented_fluxes)))
    
    # Append the new results to the overall results data table
    segmented_fluxes <- rbind(dt_sub, segmented_fluxes)  
   
    print(paste0(flux, " done")) # should replace with a progress bar
    
  }
  
  # Return the combined data table with segmented fluxes
  segmented_fluxes
}
