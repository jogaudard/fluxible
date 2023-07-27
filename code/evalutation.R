# to provide different evalutation metrics and possibility to flag fluxe that should be replaced by NA or 0

eval.flux <- function(data,
                      noise = 10, # noise of the setup in ppm
                      r.squared_threshold = -100, #threshold to discard data based on r.squared of the linear fit at tz over the kept part
                      RMSE_threshold = 25, # threshold above which data are discarded
                      cor_threshold = 0.5, # delimits the window in which CO2 is considered not correlated with time
                      b_threshold = 1, # this value and its opposite define a window out of which data are being discarded
                      ambient_CO2 = 421,
                      error = 100 # error of the setup in ppm. fluxes starting outside of the window ambient_CO2 +/- error will be discarded
                      
){
  eval_metrics <- data %>% 
    group_by(fluxID) %>% 
    nest() %>% 
    rowwise() %>% 
    summarize(
      cor_coef = cor(data$CO2, data$time),
      RMSE = sqrt((1/length(data$time)) * sum((data$fit - data$CO2)^2)),
      norm_RMSE = RMSE / (max(data$CO2) - min(data$CO2)),
      start_error = case_when(
        data$CO2[1] < (ambient_CO2 - error) ~ "error",
        data$CO2[1] > (ambient_CO2 + error) ~ "error",
        TRUE ~ "ok"
    )
    ) %>% 
    ungroup()
    
}