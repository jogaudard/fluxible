
require(cpop)
require(ggplot2)
require(dplyr)
require(lubridate)
require(tidyverse)
require(reshape2)
require(cowplot)
require(sarima)
require(data.table)

#' Calculate Tent Fluxes
#'
#' This function calculates the fluxes of a specified parameter (e.g., CO2 or H2O) 
#' from a provided dataset of flux measurements.
#' 
#' Specifically, the function devides the flux into different segments (based on brakpoints) and 
#' calculates the fluxes for each segment separately. Results are weighted by the length of the segment. 
#' We fit a linear and a non-linear function separately. For the linear one, segments with an rsq of < are discared. 
#' 
#' 
#' @param vol Volume of the measurement chamber (default = 2.197).
#' @param area Area of the measurement chamber (default = 1.69).
#' @param signal_strength_tresh Signal strength threshold for measurements (default = 90.0).
#' @param par_thresh Photosynthetically Active Radiation threshold (default = 650).
#' @param flux_df Data frame containing matched flux measurements (default = matchedFluxes).
#' @param param Parameter to calculate flux for: "co2" or "h2o" (default = "co2").
#' @param par_col Column name for PAR data (default = "PAR").
#' @param date_time_col Column name for date and time (default = "f_datetime").
#' @param co2_col Column name for CO2 concentration (default = "f_conc").
#' @param h2o_col Column name for H2O concentration (default = "ConcH2O").
#' @param sign_str_col Column name for signal strength measurements (default = "signal_strength").
#' @param temp_col Column name for air temperature (default = "AirTemperature").
#' @param pressure_col Column name for pressure in kPa (default = "PressureKPa").
#' @param flux_type_col Column name for flux type measurements (default = "measurement").
#' @param plot_id_col Column name for plot identifiers (default = "plot_id").
#'
#' @return A data.table containing calculated flux values, average temperature, pressure, 
#'         and other relevant statistics for each unique flux measurement file.
#'         

#flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_west_3_night_resp.txt"

# vol = 2.197 #default 
# area = 1.69 #default 
# signal_strength_tresh = 95.0 #default 
# par_thresh = 650 #default 
# flux_df = dt  
# param = "co2" #default 
# par_col = "par" #default 
# date_time_col = "date_time" #default 
# co2_col = "co2_conc" #default 
# h2o_col = "h2o_conc" #default 
# sign_str_col = "signal_strength" #default 
# temp_col = "temperature_c" #default 
# pressure_col = "pressure_kpa" #default 
# flux_type_col = "measurement" #default 
# plot_id_col = "plot_id" #default 
# skip = 7 
# redo_col = "redo"
# day_night_col = "day_night"
# file_name_col = "file_name"
# min_length = 60
# start_time_col = "start_time"
# correct_for_h2o_conc = TRUE
# min_seg_length = 30

calc_tent_fluxes <- function( 
    vol = 2.197, #default 
    area = 1.69, #default 
    signal_strength_tresh = 95.0,  # Threshold for valid signal strength
    par_thresh = 650,  # Photosynthetically Active Radiation (PAR) threshold
    flux_df = dt,  # Data frame containing flux data
    param = "co2",  # Parameter to analyze, default is CO2
    par_col = "par",  # Column name for PAR in flux_df
    date_time_col = "date_time",  # Column name for datetime in flux_df
    co2_col = "co2_conc",  # Column name for CO2 concentration in flux_df
    h2o_col = "h2o_conc",  # Column name for H2O concentration in flux_df
    sign_str_col = "signal_strength",  # Column name for signal strength in flux_df
    temp_col = "temperature_c",  # Column name for air temperature in flux_df
    pressure_col = "pressure_kpa",  # Column name for pressure in kPa in flux_df
    flux_type_col = "measurement",  # Column name for flux type in flux_df
    plot_id_col = "plot_id",  # Column name for plot ID in flux_df
    redo_col = "redo", 
    day_night_col = "day_night",
    file_name_col = "file_name",
    start_time_col = "start_time",
    min_length = 60,  # minimum flux length 
    skip = 7, #number of rows to skip from each flux measurement, 
    correct_for_h2o_conc = TRUE, 
    min_seg_length = 30
){ 
  
  
  if(is.na(vol)){print("Please provide the tent volume (in m^3)")}
  
  if(is.na(area)){print("Please provide the tent area (in m^2)")}
  
  if(is.null(flux_df)){print("Please provide a dataframe with gas concentrations")}
  
  
  # Initialize an empty data table to store calculated fluxes
  calculated_fluxes <- data.table::data.table() 
  
  # Loop through each unique flux measurement file_name in the flux data frame
  
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_1_day_photo.txt" 
  # flux <- "data/rawData/LI7500/LI7500_Site 4/4_2600_west_3_day_photo.txt"     
  # flux <- "data/rawData/LI7500/LI7500_Site 5/5_2800_east_2_day_photo.txt"   
   flux <- "data/rawData/LI7500/LI7500_Site 5//5_2800_west_3_day_photo.txt"  
 
   ####flux <- t[3]
  for(flux in unique(flux_df[[file_name_col]])){
    
    if(is.na(flux)){next}  # Skip if the flux file_name is NA
    
    # Subset the flux data frame for the current file_name and keep distinct rows
    dt_sub <- flux_df[flux_df[[file_name_col]] == {{flux}}, ] %>% unique()
    
    if(nrow(dt_sub) < min_length + skip){next}  # Skip if n is too small 
    
    
    # Get the unique flux type from the subset data
    flux_type <- unique(dt_sub[[flux_type_col]]) #%>% pull()
    
    # If more than one flux type is found, get the mode to select the more common one 
    if(length(flux_type) > 1){
      
      print(paste0("Warning: more than one flux type detected for flux with id: ", flux))
      
      table(dt_sub[[flux_type_col]])  
      
      Mode <- function(x, na.rm = TRUE) {
        if(na.rm){x = x[!is.na(x)]}
        ux <- unique(x)  
        return(ux[which.max(tabulate(match(x, ux)))])  # Return the most frequent value (mode)
      }
      
      flux_type <- Mode(dt_sub[[flux_type_col]])  # Assign mode to flux_type
      # this should usually not be necessary 
    }
    
    # Skip if the flux type is ambient measurements
    if(flux_type == "a"){next} 
    
    # Define ambient measurement for comparison
    flux_df$flux_type <- flux_df[[flux_type_col]]  
    
    # Select ambient measurements within 15 minutes of the current measurement
    dt_ambient <- suppressWarnings(flux_df %>%  
                                    mutate(time_diff_amb = difftime(.[[start_time_col]], dt_sub[[start_time_col]], units = "mins")) %>%  
                                    filter(
                                    .[[plot_id_col]] == dt_sub[[plot_id_col]] & abs(time_diff_amb) < 15 & flux_type == "a"  # Filter for matching plot ID and time difference
                                    ))
    
    # If no ambient data is found, take the first 5 rows of the current measurement
    if(nrow(dt_ambient) < 1){ 
      dt_ambient <- dt_sub[1:5,] 
    }
    
    # Check if the measurement is a night measurement or respiration
    check_night_resp <- FALSE
    
    if(grepl("resp", flux, fixed = TRUE)){ 
      check_night_resp <- TRUE 
    }
    
    ## remove first rows of the flux 
    
    dt_sub <- dt_sub[!1:skip, ]
    
    
    ## Define parameters for calculations
    R <- 8.314472  # Universal gas constant in J/(K·mol)
    
    # Prepare variables for calculations
    time <- as.numeric(dt_sub[[date_time_col]]) - min(as.numeric(dt_sub[[date_time_col]]))  # Time since the start of the measurement
    co2 <- as.numeric(dt_sub[[co2_col]])  # CO2 concentration
    h2o <- as.numeric(dt_sub[[h2o_col]])  # H2O concentration
    
    # Determine PAR values; if no PAR data is available or if it's a respiration measurement, set PAR to threshold + 1
    if(check_night_resp == TRUE | sum(is.na(dt_sub[[par_col]])) == nrow(dt_sub)){ 
      par <- rep(par_thresh + 1, nrow(dt_sub))  # Default PAR value
    }else{ 
      par <- as.numeric(dt_sub[[par_col]])  # Use actual PAR data
    }
    
    # Prepare other parameters for calculations
    press <- as.numeric(dt_sub[[pressure_col]])  # Pressure
    temp <- as.numeric(dt_sub[[temp_col]])  # Air temperature
    signal_strength <- as.numeric(dt_sub[[sign_str_col]])  # Signal strength
    
    if(correct_for_h2o_conc == TRUE){
      # Calculate c' and w' with respect to the H2O concentration
      c_prime <- co2 / (1 - (h2o / 1000))  
      w_prime <- h2o / (1 - (h2o / 1000))  
      wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
      co2_amb <- mean(as.numeric(as.character(dt_ambient[[co2_col]])) / (1 - (as.numeric(as.character(dt_ambient[[h2o_col]])) / 1000)))  # Ambient CO2 concentration
      h2o_amb <- mean(as.numeric(as.character(dt_ambient[[h2o_col]])) / (1 - (as.numeric(as.character(dt_ambient[[h2o_col]])) / 1000)))  # Ambient H2O concentration
    }else{
    
    c_prime <- co2  
    w_prime <- h2o  
    wav_dil <- mean(h2o / (1 - (h2o / 1000)))  # Mean H2O dilution factor
    co2_amb <- mean(dt_ambient[[co2_col]], na.rm = T)# Ambient CO2 concentration
    h2o_amb <- mean(dt_ambient[[h2o_col]], na.rm = T)

    }
    
    if ("co2" == param) {  
      cw_prime <- c_prime  # Use c_prime for CO2
      tag <- "c_prime"  
    } else if ("h2o" == param) {  
      cw_prime <- w_prime  # Use w_prime for H2O
      tag <- "w_prime"  
    }
    
    # Identify change points in the time series of c'
    res <- suppressMessages(cpop(cw_prime, minseglen = min_seg_length))  # Identify change points with a minimum segment length of 30 seconds
    changepoints(res)  # Extract change points from the result
    cw_prime_seg <- fitted(res)  # Get the fitted values from the change point analysis
    
    
    if(param == "co2"){
      conc_col <- co2_col
    }else if(param == "h2o"){
      conc_col <- h2o_col
      
    }
    # Create a ggplot for visualizing CO2 concentration over time
    p2 <- ggplot(data = dt_sub, aes(y = .data[[conc_col]],  
                                   x = as.numeric(.data[[date_time_col]]) - min(as.numeric(.data[[date_time_col]])), 
                                   color = as.numeric(.data[[sign_str_col]]))) +  
      geom_point() +  # Add points for CO2 data
      scale_color_gradient(high = "blue", low = "red", limits = c(79, 101)) +  # Color gradient for signal strength
      geom_line() +  # Add lines connecting the points
      theme(legend.position = "none") +  # Remove legend
      #ylim(min(dt_sub[[co2_col]], na.rm = TRUE), max(dt_sub[[co2_col]], na.rm = TRUE)) +  # Set y-axis limits for CO2
      ylab(paste0(param, " concentration")) +  # Label y-axis
      xlab("Time") +  # Label x-axis
      geom_vline(xintercept = cw_prime_seg$x0)  # Add vertical lines at change points
    
    # Create a ggplot for visualizing PAR over time
    p1 <- ggplot(data = dt_sub,  
                 aes(y = .data[[par_col]],  
                     x = as.numeric(.data[[date_time_col]]) - min(as.numeric(.data[[date_time_col]])))) +  
      geom_point(color = "forestgreen") +  # Add points for PAR data
      geom_line(color = "forestgreen") +  # Add lines connecting the points
      #  ylim(min(dt_sub[[par_col]], na.rm = TRUE), max(dt_sub[[par_col]], na.rm = TRUE)) +  # Set y-axis limits for PAR
      ylab("PAR") +  # Label y-axis
      xlab("Time") +  # Label x-axis
      labs(title = paste0(flux)) +  # Set title based on flux file_name
      geom_vline(xintercept = cw_prime_seg$x0)  # Add vertical lines at change points
    
    # Combine the two plots vertically
    p_c <- plot_grid(p1, p2, ncol=1, align="v", axis=1) 
    plot(p_c)  # Display the combined plot
    
    # Create a sequence of segment indices based on the number of rows in cw_prime_seg data frame
    segs <- c(1:nrow(cw_prime_seg))  
    
    # Initialize vectors to store various calculated metrics
    aic_linear_fit <- c()  # Stores AIC values for linear fits
    inter <- c()         # Stores intercept values from linear fits
    dcw_dt <- c()       # Stores the derivative (slope) of the linear fit
    r2_linear_fit <- c()  # Stores R-squared values for linear fits
    param_lm <- c()     # Stores parameters from linear model
    avg_temp <- c()  # Stores average temperature in Celsius for each segment
    avg_pressure <- c()   # Stores average pressure in KPa for each segment
    cav <- c()            # Stores average CO2 concentrations for each segment
    wav <- c()            # Stores average H2O concentrations for each segment
    lm_slope <- c()
    
    r2_linear_fit_all <- c()
    eff_sample_all <- 0
    # Initialize index for storing results and counters for effective sample sizes
    i = 1  
    eff_sample <- 0          # Counter for effective sample size based on linear fit criteria

    pred_plot <- data.table()
    
    # Loop over each segment to perform calculations
    for(s in segs){  
      
      end <- length(par)  # Get the total number of PAR values
      
      s1 <- cw_prime_seg$x0[s] + 1 # Starting index for current segment
      s2 <- cw_prime_seg$x1[s] + 1# Starting index for current segment
      
      # # Determine the ending index for the current segment
      # if(s == nrow(cw_prime_seg)){  
      #   s2 <- end  # If it's the last segment, use the last index
      # } else{
      #   s2 <- cw_prime_seg$x0[s+1]  # Otherwise, use the starting index of the next segment
      # }
      
      # Calculate the mean signal strength and PAR for the current segment
      mean_si_st <- mean(signal_strength[s1:s2])  
      mean_par <- mean(par[s1:s2])  
      sd_par <- sd(par[s1:s2])
      cv_par <- sd_par/mean_par
      
      # Select the concentration variable based on the parameter specified
      # Tag to identify the parameter being processed
      
      if ("co2" == param) {  
        cw_prime <- c_prime  # Use c_prime for CO2
        tag <- "c_prime"  
      } else if ("h2o" == param) {  
        cw_prime <- w_prime  # Use w_prime for H2O
        tag <- "w_prime"  
      }
      
      
      # Proceed only if both signal strength and PAR exceed the respective thresholds
      if((mean_si_st > signal_strength_tresh) && (mean_par > par_thresh)){  
        
        time_m <- time[s1:s2] - (time[s1]-1)
        
        # Fit a linear model to the current segment
        linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time_m))  
        
        lm_slope[i] <- linear.fit$coeff[2]
        
        # Extract R-squared value from the linear model
        rsq <- as.numeric(summary(linear.fit)$r.sq)  
        
        pred <- data.frame(time = time[s1:s2])
        pred$pred <- predict(linear.fit)*(1 - (mean(h2o[s1:s2]) / 1000))  
        
        pred_plot <- rbind(pred, pred_plot)
        
        p3 <- p2 + 
          geom_line(data = pred_plot, aes(x = time, y = pred), color = "black", linewidth = 1.1) +
          labs(title = paste0(flux))
        print(p3)
        # Calculate average temperature, pressure, and H2O concentration for the segment
        t_av <- mean(temp[s1:s2])  
        p_av <- mean(press[s1:s2])  
        w_av <- mean(h2o[s1:s2])  
        
        
        
        ### save R2 for all segments
        frac_sample_all <- (s2-s1)  # Calculate the fraction of samples in the current segment
        eff_sample_all <- eff_sample_all + (s2-s1)  # Update the effective sample size for linear fits
        
        r2_linear_fit_all[i] <- rsq
        
        # Only consider the segment if the R-squared value meets the threshold (0.7)
        if(rsq > 0.7){  
          frac_sample <- (s2-s1)  # Calculate the fraction of samples in the current segment
          eff_sample <- eff_sample + (s2-s1)  # Update the effective sample size for linear fits
          
          # Store metrics for linear model results
          aic_linear_fit[i] <- as.numeric(stats::AIC(linear.fit))*frac_sample  # Store AIC multiplied by sample fraction
          r2_linear_fit[i] <- as.numeric(summary(linear.fit)$r.sq)*frac_sample  # Store R-squared multiplied by sample fraction
          inter[i] <- as.numeric(linear.fit$coeff[1])  # Store intercept from linear fit
          dcw_dt[i] <- as.numeric(linear.fit$coeff[2])  # Store slope from linear fit
          avg_temp[i] <- mean(temp[s1:s2])*frac_sample  # Store average temperature multiplied by sample fraction
          avg_pressure[i] <- mean(press[s1:s2])*frac_sample  # Store average pressure multiplied by sample fraction
          cav[i] <- mean(co2[s1:s2])*frac_sample  # Store average CO2 concentration multiplied by sample fraction
          wav[i] <- mean(h2o[s1:s2])*frac_sample  # Store average H2O concentration multiplied by sample fraction
          
          # Calculate the linear model parameter based on the parameter type
          if ("co2" == param) {  
            param_lm[i] <- (vol * p_av * (1000) * dcw_dt[i])/(R * area * (t_av + 273.15))*frac_sample  
          } else if ("h2o" == param) {  
            param_lm[i] <- (vol * p_av * (1000) * dcw_dt[i])/(R * area * (t_av + 273.15))*frac_sample  
          }  
        }  
        
        # Print the current segment index
        print(s)  
        i <- i+1  # Increment the index for storing results
      }  
    }
    
    # Calculate average metrics based on effective sample sizes
    aic_linear_avg <- sum(aic_linear_fit, na.rm = TRUE)/eff_sample  # Average AIC for linear fits
    inter_avg <- sum(inter, na.rm = TRUE)/eff_sample  # Average intercept for linear fits
    dcw_dt_avg <- sum(dcw_dt, na.rm = TRUE)/eff_sample  # Average slope for linear fits
    r2_linear_fit_avg <- sum(r2_linear_fit, na.rm = TRUE)/eff_sample  # Average R-squared for linear fits
    param_lm_avg <- sum(param_lm, na.rm = TRUE)/eff_sample  # Average parameters for linear model
    avg_temp_avg <- if(eff_sample == 0){mean(temp)}else{sum(avg_temp, na.rm = TRUE)/eff_sample}  # Average temperature with handling for no effective samples
    avg_pressure_avg <- if(eff_sample == 0){mean(press)}else{sum(avg_pressure, na.rm = TRUE)/eff_sample}  # Average pressure with handling for no effective samples
    cav_avg <- if(eff_sample == 0){mean(co2)}else{sum(cav, na.rm = TRUE)/eff_sample}  # Average CO2 concentration with handling for no effective samples
    wav_avg <- if(eff_sample == 0){mean(h2o)}else{sum(wav, na.rm = TRUE)/eff_sample}  # Average H2O concentration with handling for no effective samples
    
    
    if(any(lm_slope > 0, na.rm = T) & any(lm_slope < 0, na.rm = T)) {
      flux_direction_flag <- paste0("Weird flux! Both positive and negative segments identified")
      print(flux_direction_flag)
    } else {
      flux_direction_flag <- paste0("Looks good!")
      print(flux_direction_flag)
    }
    
    
    
    # Calculate standard deviations for the parameters based on effective sample sizes
    sd_lm <- sd(param_lm)/eff_sample  # Standard deviation for linear model parameters
    
    aic_avg <- aic_linear_avg
    r2_avg <- r2_linear_fit_avg
    flux_value <- param_lm_avg
    seg_sd <- sd_lm

    ### select flux
    
    # Extract unique plot IDs from the subset data frame using the specified column name
    plot_id = unique(dt_sub[[plot_id_col]])  
    
    if(!is.na(day_night_col)){
      day_night = unique(dt_sub[[day_night_col]])  
    }else{day_night = NA}
    if(!is.na(redo_col)){
      redo = unique(dt_sub[[redo_col]])  
    }else{redo = NA}
    
    # Print a tibble summarizing various metrics related to the calculated flux
    print(data.table::data.table(
      file_name = flux,                          # File name associated with the flux calculations
      avg_temp = avg_temp_avg,   # Average temperature in Celsius
      avg_pressure = avg_pressure_avg,     # Average pressure in KPa
      flux_value = flux_value,                    # Value of the flux being calculated
      flux_type = flux_type,                      # Type of flux (e.g., CO2, H2O)
      plot_id = plot_id,                          # Unique plot identifier
      seg_sd = seg_sd,                            # Standard deviation of segments
      avg_co2 = cav_avg,                         # Average CO2 concentration
      avg_h2o = wav_avg,                         # Average H2O concentration
      r2 = r2_avg,                              # Average R-squared value for model fits
      aic = aic_avg,                            # Average Akaike Information Criterion (AIC) for model fits
      param = param,                            # Parameter associated with the flux calculation
      flux_direction_flag = flux_direction_flag, 
      redo = redo, 
      day_night = day_night, 
      par_flag = ifelse(mean(par, na.rm = T) < par_thresh, "PAR too low", NA)
    ))
    
    # Create a data table to hold the results of the flux calculations
    tmpRes <- data.table::data.table(
      file_name = flux,                          # File name
      avg_temp = avg_temp_avg,   # Average temperature
      avg_pressure = avg_pressure_avg,     # Average pressure
      flux_value = flux_value,                    # Flux value
      flux_type = flux_type,                      # Flux type
      plot_id = plot_id,                          # Unique plot ID
      seg_sd = seg_sd,                            # Segment standard deviation
      avg_co2 = cav_avg,                         # Average CO2 concentration
      avg_h2o = wav_avg,                         # Average H2O concentration
      r2 = r2_avg,                              # Average R-squared
      aic = aic_avg,                            # Average AIC
      param = param,                            # Parameter
      flux_direction_flag = flux_direction_flag,
      redo = redo, 
      day_night = day_night, 
      par_flag = ifelse(mean(par, na.rm = T) < par_thresh, "PAR too low", NA)
      
    ) %>%
      # Mutate to create additional flags for quality control
      mutate(
        seg_sd = ifelse(is.na(seg_sd), 0, seg_sd),  # Replace NA segment SDs with 0
        r2_flag = ifelse(r2 < .75 | is.na(r2), "Be careful, R2 is suspiciously low or NA", "Looks good!"),  # Flag for R-squared value
        seg_flag = ifelse(seg_sd > abs(flux_value/2), "Not good, segments are extremely variable. Suggest to discard", "Looks good!"),  # Flag for segment variability
        flux_flag = ifelse(r2_flag == "Looks good!" & seg_flag == "Looks good!" & flux_direction_flag == "Looks good!", "keep", "discard")  # Overall flag for flux quality
      )
    tmpRes
    # Append the new results to the overall results data table
    calculated_fluxes <- rbind(tmpRes, calculated_fluxes)  
  }
  # Return the combined data table with calculated fluxes
  return(calculated_fluxes)  
}

