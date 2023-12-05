# function to fit the CO2 concentration to a function of time, as described in Zhao 2018

flux_fitting_log <- function(conc_df,
                               weird_fluxesID = NA, # a vector of fluxes to discard because they are obviously wrong
                               t_window = 20, # enlarge focus window before and after tmin and tmax
                               Cz_window = 15, # window used to calculate Cz, at the beginning of cut window
                               b_window = 10, # window to estimate b. It is an interval after tz where it is assumed that C fits the data perfectly
                               a_window = 10, # window at the end of the flux to estimate a
                               length_flux = 160, # length of the total flux
                               roll_width = 15, # width of the rolling mean for CO2 when looking for tz, idaelly same as Cz_window
                               # c = 3 # coefficient to define the interval around estimates to optimize function
                               # noise = 10, # noise of the setup in ppm
                               # r.squared_threshold = -100, #threshold to discard data based on r.squared of the linear fit at tz over the kept part
                              #  RMSE_threshold = 25, # threshold above which data are discarded
                              #  cor_threshold = 0.5, # delimits the window in which CO2 is considered not correlated with time
                              #  b_threshold = 1, # this value and its opposite define a window out of which data are being discarded
                              #  ambient_CO2 = 421,
                              #  error = 100 # error of the setup in ppm. fluxes starting outside of the window ambient_CO2 +/- error will be discarded
                              start_cut = 0, # to cut at the start
                              end_cut = 0 # to cut at the end, if you notice on the graphs that the match was not precise enough
){ 
  
  # we will try to calculate all the parameters without a, and then insert a in the end
  

  
  conc_df <- conc_df %>% 
    group_by(fluxID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
      time = as.double(time),
      start = start + start_cut,
      end = end - end_cut,
      cut = case_when(
        datetime < start | datetime > end ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(cut)
    ) %>% 
    ungroup()
  
conc_df_cut <- conc_df %>%
   filter(
      cut == "keep"
    )

  Cm_df <- conc_df_cut %>% 
       group_by(fluxID) %>% 
    distinct(conc, .keep_all = TRUE) %>% 
    mutate(
      Cmax = max(conc),
      Cmin = min(conc),
      tmax = time[conc == Cmax],
      tmin = time[conc == Cmin]
    ) %>% 
    select(fluxID, Cmax, Cmin, tmax, tmin) %>% 
    ungroup() %>% 
    distinct(Cmax, Cmin, .keep_all = TRUE)
  
  Cm_slope <- conc_df_cut %>% 
    group_by(fluxID) %>% 
    do({model = lm(conc ~ time, data=.)    # create your model
    data.frame(broom::tidy(model),              # get coefficient info
               broom::glance(model))}) %>%          # get model info
    filter(term == "time") %>% 
    rename(slope_Cm = estimate) %>% 
    select(fluxID, slope_Cm) %>% 
    ungroup()
  
  Cm_df <- left_join(Cm_df, Cm_slope) %>% 
    mutate(
      Cm_est = case_when(
        slope_Cm < 0 ~ Cmin, #Cm is the maximum mixing point, which is when the limit of C(t) when t tends to infinite.
        slope_Cm > 0 ~ Cmax 
      ),
      tm = case_when(
        slope_Cm < 0 ~ tmin,
        slope_Cm > 0 ~ tmax
      )
    ) %>% 
    select(fluxID, Cm_est, tm, slope_Cm) %>% 
    ungroup()
  
  Cz_df <- conc_df_cut %>%
    group_by(fluxID) %>%
    filter(
      time <= Cz_window
    ) %>%
    do({model = lm(conc ~ time, data=.)    # create your model
    data.frame(broom::tidy(model),              # get coefficient info
               broom::glance(model))}) %>%          # get model info
    pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    rename(
      Cz = "(Intercept)",
      slope_Cz = time) %>%
    select(fluxID, Cz, slope_Cz) %>%
    ungroup()
  
  tz_df <- conc_df_cut %>% 
    left_join(Cz_df) %>% 
    group_by(fluxID) %>% 
    filter(
      # time > Cz_window
      time < length_flux / 2 # tz should be in the first half of the flux
    ) %>%
    mutate(
      conc_roll = zoo::rollmean(conc, k = roll_width, fill = NA, align = "right"),
      Cd = abs(conc_roll - Cz),
      minCd = min(Cd, na.rm = TRUE),
      tz_est = min(time[Cd == minCd], na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(fluxID, tz_est) %>% 
    distinct()
  
  # a_df <- CO2_df %>% # a is the slope at the end of the flux
  #   group_by(fluxID) %>% 
  #   filter(
  #     datetime >= end - t_window
  #   ) %>% 
  #   do({model = lm(CO2 ~ time, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))}) %>%          # get model info
  #   pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
  #   rename(
  #     a_est = time
  #   ) %>% 
  #   select(fluxID, a_est) %>% 
  #   ungroup()
  
  Cb_df <- conc_df_cut %>% 
    left_join(tz_df) %>% 
    group_by(fluxID) %>% 
    mutate(
      Cb = conc[time == tz_est - b_window]
    ) %>% 
    ungroup() %>% 
    select(fluxID, Cb) %>% 
    distinct()
  
  a_df <- conc_df_cut %>% 
    group_by(fluxID) %>% 
    mutate(
      ta = length_flux - a_window,
      Ca = conc[time == ta]
    ) %>% 
    ungroup() %>% 
    select(fluxID, ta, Ca) %>% 
    distinct()
  
  estimates_df <- left_join(Cm_df, Cz_df) %>% 
    left_join(tz_df) %>% 
    left_join(a_df) %>%
    left_join(Cb_df) %>% 
    mutate(
      b_est = case_when(
        Cb == Cm_est ~ 0, # special case or flat flux
        Cz == Cm_est ~ 0, # special case or flat flux
        TRUE ~         log(abs((Cb - Cm_est)/(Cz - Cm_est))) * (1/b_window), # problem with the log? negative value? let's try with absolute value
      ),
      a_est = case_when(
        ta == tz_est ~ 0, # tz_est = ta is a special case that is undefined
        TRUE ~ (Ca - Cm_est - (Cz - Cm_est) * exp(-b_est * (ta - tz_est))) / (ta - tz_est)
      )
      )

  
  
  
  myfn <- function(time, conc, par, Cz) {
    # sqrt((1/length(time)) * sum((par[1]+par[2]*(time-exp(par[4]))+(Cz-par[1])*exp(-(1/(1+exp(-par[3])))*(time-exp(par[4])))-CO2)^2))
    sqrt((1/length(time)) * sum((par[1]+par[2]*(time-exp(par[4]))+(Cz-par[1])*exp(-par[3]*(time-exp(par[4])))-conc)^2))
    # sqrt((1/length(time)) * sum((par[1]+par[2]*(time-exp(par[4]))+(Cz-par[1])*exp(-exp(par[3])*(time-exp(par[4])))-CO2)^2))
    
  }
  
  myfn1 <- function(time, conc, Cz, Cm, b, tz) {
    sqrt((1/length(time)) * sum((Cm+(Cz-Cm)*exp(-b*(time-tz))-conc)^2))
  }
  
  myfn2 <- function(time, conc, a, Cz, Cm, b, tz) {
    sqrt((1/length(time)) * sum((Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz))-conc)^2))
  }
  
  
  
  
  fitting_par <- conc_df_cut %>% 
    left_join(estimates_df) %>% 
    select(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time, conc) %>%
    group_by(fluxID, Cm_est, a_est, b_est, tz_est, Cz) %>%
    nest() %>% 
    rowwise() %>%
    summarize(
      # I would like to do something more resilient to avoid stopping everything if there is a problem with optim. Maybe tryCatch can be an idea
      results = list(optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, conc = data$conc, time = data$time, Cz = Cz)), #, lower=c(0, -Inf, -Inf, 0),  method="L-BFGS-B"
      Cm = results$par[1],
      a = results$par[2],
      # b = exp(results$par[3]),#/(abs(results$par[3])+1),
      b = results$par[3],
      # b = 1/(1+exp(-results$par[3])),
      #need to find the fit with the b closest to 0 (negative or positive)
      tz = exp(results$par[4]), #we force tz to be positive
      slope_tz = a + b * (Cm - Cz),
    ) %>% 
    ungroup()
  conc_fitting <- conc_df %>% 
    left_join(fitting_par) %>% 
    mutate(
      # time_corr = difftime(start_window, start, units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      # time_corr = as.double(time_corr),
      fit = Cm + a * (time - tz) + (Cz - Cm) * exp(- b * (time - tz)),
      fit_slope = slope_tz * (time) + Cz - slope_tz * tz,
      # fit = Cm_est + a_est * (time - tz_est - time_corr) + (Cz - Cm_est) * exp(- b_est * (time - tz_est - time_corr)),
      # fit_slope = (a_est + b_est * (Cm_est - Cz) ) * (time - time_corr) + Cz - slope_tz * tz_est,
      start_z = start + tz # this is for graph purpose, to have a vertical line showing where tz is for each flux
      
    )# %>% 
  # group_by(fluxID, Cm, a, b, tz, Cz) %>% 
  #   # nest() %>% 
  #   mutate(
  #     RMSE = myfn2(time = time, CO2 = CO2, a = a, b = b, Cm = Cm, Cz = Cz, tz = tz - time_corr)
  #   ) %>% 
  #   ungroup
  
  # r2_general <-function(preds,actual){ 
  #   return(1 - (sum((actual - preds)^2)/sum((actual - mean(actual))^2)))
  # }
  
  # model_fit <- CO2_fitting %>%
  #   group_by(fluxID) %>% 
  #   nest() %>% 
  #   rowwise() %>% 
  #   summarize(
  #     cor_coef = cor(data$CO2, data$time),
  #     r.squared_full = r2_general(data$fit, data$CO2),
  #     RMSE_full = sqrt((1/length(data$time)) * sum((data$fit - data$CO2)^2))
  #   )
  
  # model_fit <- CO2_fitting %>%
  #   # filter(
  #   #   cut == "keep"
  #   # ) %>%
  #   group_by(fluxID) %>%
  #   nest() %>%
  #   rowwise() %>%
  #   # select(fluxID, time, CO2, fit, fit_slope, Cm, a, b, Cz, tz, time_corr) %>%
  #   summarize(
  #     cor_coef = cor(data$CO2, data$time),
  #     # r.squared = r2_general(data$fit, data$CO2),
  #     RMSE = sqrt((1/length(data$time)) * sum((data$fit - data$CO2)^2)),
  #     # RMSE = myfn2(time = data$time, CO2 = data$CO2, a = data$a, b = data$b, Cm = data$Cm, Cz = data$Cz, tz = data$tz - data$time_corr),
  #     # norm_RMSE = RMSE / (max(data$CO2) - min(data$CO2)),
  #     # r.squared_slope = r2_general(data$fit_slope, data$CO2),
  #     start_error = case_when(
  #       data$CO2[1] < (ambient_CO2 - error) ~ "error",
  #       data$CO2[1] > (ambient_CO2 + error) ~ "error",
  #       TRUE ~ "ok"
  #     )
  #   ) %>%
  #   ungroup()
  
  # CO2_fitting <- CO2_fitting %>%
  #   left_join(model_fit)
  #   left_join(model_fit_cut)
  # 
  # now we need a set of rules to assess if the data should be kept, discarded, or replaced by 0
  # kept: good fit
  # discarded: bad fit, but with changes in CO2 concentration over time
  # replaced by 0: bad fit, and CO2 concentration does not change over time (just noise). How do we assess that?
  # Maybe we just set a threshold on the slope below which the slope is replaced by 0 if the fit is bad
  # This threshold can be calculated as the amplitude of the noise of the setup (to be fed into the function, depnding on the setup) divided by the lenght of the measurement
  
  # CO2_fitting <- CO2_fitting %>% 
  #   mutate(
  #     # threshold_slope = noise / as.double(difftime(end_window, start_window, units = "secs")),
  #     fit_quality = case_when(
  #       # b <= -b_threshold | 
  #         b >= b_threshold ~ "bad",
  #         type == "ER" & slope_tz < 0 ~ "bad",
  #       # RMSE > RMSE_threshold ~ "bad",
  #       # r.squared_slope < r.squared_threshold ~ "bad",
  #       # fluxID %in% weird_fluxesID ~ "bad",
  #       # start_error == "error" ~ "bad",
  #       TRUE ~ "ok"
  #     ),
  #     correlation = case_when(
  #       # cor_coef < cor_threshold & cor_coef > -cor_threshold ~ "no",
  #       abs(cor_coef) < cor_threshold ~ "no",
  #       TRUE ~ "yes"
  #     ),
  #     flag = case_when(
  #       fluxID %in% weird_fluxesID ~ "weird_flux",
  #       start_error == "error" ~ "start_error",
  #       fit_quality == "bad" & correlation == "yes" ~ "discard",
  #       fit_quality == "bad" & correlation == "no" ~ "zero",
  #       # fit_quality == "bad" & correlation == "no" & type == "NEE" ~ "zero",
  #       # fit_quality == "bad" & correlation == "no" & type == "ER" ~ "discard", # the idea being that we cannot have a respiration of 0, but I am not sure I agree with that. Soil could be super dry, frozen, containing very little organic matter...
  #       fit_quality == "ok" ~ "ok"
  #     )
    # )
  
  return(conc_fitting)
}
