# function to fit the CO2 concentration to a function of time, as described in Zhao 2018

# to do list
# check time thing in the beginning
# clean unused parts and stuff in comments
# add error messages for wrong arguments
# add warnings?
# add documentation
# make test on longer datasets
# make test with negative fluxes (the dataset used so far has only positive fluxes)


flux_fitting_log <- function(conc_df,
                              #  weird_fluxesID = NA, # a vector of fluxes to discard because they are obviously wrong, this shoudl be moved to the quality check function
                               t_window = 20, # enlarge focus window before and after tmin and tmax
                               Cz_window = 15, # window used to calculate Cz, at the beginning of cut window
                               b_window = 10, # window to estimate b. It is an interval after tz where it is assumed that C fits the data perfectly
                               a_window = 10, # window at the end of the flux to estimate a
                              #  length_flux = 160, # length of the total flux
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
  
   # we need to check that values provided to the function are what we expect and will not crash the function

  #  if(!is.double(t_window)) stop("t_window has to be a double")
  #  if(!is.double(Cz_window)) stop("Cz_window has to be a double")
  #  if(!is.double(b_window)) stop("b_window has to be a double")
  #  if(!is.double(a_window)) stop("a_window has to be a double")
  #  if(!is.double(length_flux)) stop("length_flux has to be a double")
  #  if(!is.double(roll_width)) stop("roll_width has to be a double")
  #  if(!is.double(start_cut)) stop("start_cut has to be a double")
  #  if(!is.double(end_cut)) stop("end_cut has to be a double")

# handling missing data
# should print a warning that there is not enough data
# take them out of the df and re add them in the end with NA for all new columns
# what is "not enough data for the function to work?"
# need to detect fluxes without enough data and take them out before to avoid crashing the function



# need also to check that all the parameters are making sense (cutting has to be shorter than the window for ex)
  
  # we will try to calculate all the parameters without a, and then insert a in the end
  
  conc_df <- conc_df %>% 
       dplyr::group_by(fluxID) %>% 
       dplyr::mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
      time = as.double(time),
      start = start + start_cut,
      end = end - end_cut,
      cut = dplyr::case_when(
        datetime < start | datetime > end ~ "cut",
        TRUE ~ "keep"
      ),
      cut = haven::as_factor(cut)
    ) %>% 
    dplyr::ungroup()
  
conc_df_cut <- conc_df %>%
   dplyr::filter(
      cut == "keep"
    )  %>%
       tidyr::drop_na(conc) %>% # drop NA in conc to avoid messing up the models used later, will have to print a warning for that
           dplyr::group_by(fluxID) %>%
           dplyr::mutate(
            time_cut = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
            time_cut = as.double(time_cut), # we need time_cut because we dropped the NA in conc
            # time_cut = time, # maybe it can just be the same, it doesn't have to start at 0
            length_window = max(time_cut), #to have length_flux for each flux, better than setting it up as a function argument
            # length_window = max(time_cut) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            # length_window = max(time) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            time_diff = time - time_cut
                 ) %>%
                    dplyr::ungroup()


  Cm_df <- conc_df_cut %>% 
       dplyr::group_by(fluxID) %>% 
    dplyr::distinct(conc, .keep_all = TRUE) %>% 
    dplyr::mutate(
      Cmax = max(conc),
      Cmin = min(conc),
      # tmax = time[conc == Cmax],
      # tmin = time[conc == Cmin]
      tmax = time_cut[conc == Cmax],
      tmin = time_cut[conc == Cmin]
    ) %>% 
    dplyr::select(fluxID, Cmax, Cmin, tmax, tmin) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(Cmax, Cmin, .keep_all = TRUE)
  
  Cm_slope <- conc_df_cut %>% 
    dplyr::group_by(fluxID) %>% 
    dplyr::do({model = lm(conc ~ time_cut, data=.)    # create your model
    data.frame(broom::tidy(model),              # get coefficient info
               broom::glance(model))}) %>%          # get model info
    dplyr::filter(term == "time_cut") %>% 
    # dplyr::do({model = lm(conc ~ time, data=.)    # create your model
    # data.frame(broom::tidy(model),              # get coefficient info
    #            broom::glance(model))}) %>%          # get model info
    # dplyr::filter(term == "time") %>% 
    dplyr::rename(slope_Cm = estimate) %>% 
    dplyr::select(fluxID, slope_Cm) %>% 
    dplyr::ungroup()
  
  Cm_df <- dplyr::left_join(Cm_df, Cm_slope) %>% 
    dplyr::mutate(
      Cm_est = dplyr::case_when(
        slope_Cm < 0 ~ Cmin, #Cm is the maximum mixing point, which is when the limit of C(t) when t tends to infinite.
        slope_Cm > 0 ~ Cmax 
      ),
      tm = dplyr::case_when(
        slope_Cm < 0 ~ tmin,
        slope_Cm > 0 ~ tmax
      )
    ) %>% 
    dplyr::select(fluxID, Cm_est, tm, slope_Cm) %>% 
    dplyr::ungroup()
  
  Cz_df <- conc_df_cut %>%
    dplyr::group_by(fluxID) %>%
    dplyr::filter(
      time_cut <= Cz_window
      # time <= Cz_window + start_cut
    ) %>%
    dplyr::do({model = lm(conc ~ time_cut, data=.)    # create your model
    data.frame(broom::tidy(model),              # get coefficient info
               broom::glance(model))}) %>%          # get model info
    tidyr::pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    dplyr::rename(
      Cz = "(Intercept)",
      slope_Cz = time_cut) %>%
    # dplyr::do({model = lm(conc ~ time, data=.)    # create your model
    # data.frame(broom::tidy(model),              # get coefficient info
    #            broom::glance(model))}) %>%          # get model info
    # tidyr::pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    # dplyr::rename(
    #   Cz = "(Intercept)",
    #   slope_Cz = time) %>%
    dplyr::select(fluxID, Cz, slope_Cz) %>%
    dplyr::ungroup()
  
  tz_df <- conc_df_cut %>% 
    dplyr::left_join(Cz_df) %>% 
    dplyr::group_by(fluxID) %>% 
    dplyr::filter(
      # time > Cz_window
      time_cut < length_window / 2 # tz should be in the first half of the flux
      # time_cut < (length_window / 2) + start_cut # tz should be in the first half of the flux
            # time < (length_window / 2) + start_cut # tz should be in the first half of the flux
    ) %>%
    dplyr::mutate(
      conc_roll = zoo::rollmean(conc, k = roll_width, fill = NA, align = "right"),
      Cd = abs(conc_roll - Cz),
      minCd = min(Cd, na.rm = TRUE),
      # tz_est = min(time[Cd == minCd], na.rm = TRUE)
      tz_est = min(time_cut[Cd == minCd], na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(fluxID, tz_est) %>% 
    dplyr::distinct()
  
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
    dplyr::left_join(tz_df) %>% 
    dplyr::group_by(fluxID) %>% 
    dplyr::mutate(
      Cb = conc[time_cut == tz_est - b_window]
      # Cb = dplyr::case_when(tz_est - b_window < 0 ~ conc[time == start_cut],
      #                       tz_est - b_window >= 0 ~ conc[time == tz_est - b_window]
      # )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(fluxID, Cb) %>% 
    dplyr::distinct()
  
  a_df <- conc_df_cut %>% 
    dplyr::group_by(fluxID) %>% 
    dplyr::mutate(
      ta = length_window - a_window,
      # ta = length_window - a_window + start_cut,
      Ca = conc[time_cut == ta]
      # Ca = conc[time == ta]
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(fluxID, ta, Ca) %>% 
    dplyr::distinct()
  
  estimates_df <- dplyr::left_join(Cm_df, Cz_df) %>% 
    dplyr::left_join(tz_df) %>% 
    dplyr::left_join(a_df) %>%
    dplyr::left_join(Cb_df) %>% 
    dplyr::mutate(
      b_est = dplyr::case_when(
        Cb == Cm_est ~ 0, # special case or flat flux
        Cz == Cm_est ~ 0, # special case or flat flux
        TRUE ~         log(abs((Cb - Cm_est)/(Cz - Cm_est))) * (1/b_window), # problem with the log? negative value? let's try with absolute value
      ),
      a_est = dplyr::case_when(
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
    dplyr::left_join(estimates_df) %>% 
    # dplyr::select(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time, conc) %>%
    dplyr::select(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time_cut, conc, time_diff) %>%
    dplyr::group_by(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time_diff) %>%
    tidyr::nest() %>% 
    dplyr::rowwise() %>%
    dplyr::summarize(
      # I would like to do something more resilient to avoid stopping everything if there is a problem with optim. Maybe tryCatch can be an idea
      results = list(optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, conc = data$conc, time = data$time_cut, Cz = Cz)), #, lower=c(0, -Inf, -Inf, 0),  method="L-BFGS-B"
      Cm = results$par[1],
      a = results$par[2],
      # b = exp(results$par[3]),#/(abs(results$par[3])+1),
      b = results$par[3],
      # b = 1/(1+exp(-results$par[3])),
      #need to find the fit with the b closest to 0 (negative or positive)
      tz = exp(results$par[4]), #we force tz to be positive
      slope_tz = a + b * (Cm - Cz),
    ) %>% 
    dplyr::ungroup() %>%
       dplyr::select(!results) # we do not need it anymore and it is confusing to have them as tibble


  conc_fitting <- conc_df %>% 
    dplyr::left_join(fitting_par) %>% 
    dplyr::group_by(fluxID) %>%
    dplyr::mutate(
      # time_corr = difftime(start_window, start, units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      # time_corr = as.double(time_corr),
      # fit = dplyr::case_when(
      #   is.na(time) ~ NA_real_,
      #   !is.na(time) ~ Cm + a * (time - tz) + (Cz - Cm) * exp(- b * (time - tz))
      # ), # problem here because tz is calculated compared to time_cut
      # fit_slope = dplyr::case_when(
      #   is.na(time) ~ NA_real_,
      #   !is.na(time) ~ slope_tz * (time) + Cz - slope_tz * tz
      # ),
      # need to correct tz for the time diff between time and time_cut
      # time_diff = time[1] - time_cut[1],
      fit = Cm + a * (time - tz - time_diff) + (Cz - Cm) * exp(- b * (time - tz - time_diff)),
      fit_slope = slope_tz * (time) + Cz - slope_tz * (tz + time_diff),
      # fit = Cm + a * (time - tz) + (Cz - Cm) * exp(- b * (time - tz)),
      # fit_slope = slope_tz * (time) + Cz - slope_tz * tz,
      # fit = Cm_est + a_est * (time - tz_est - time_corr) + (Cz - Cm_est) * exp(- b_est * (time - tz_est - time_corr)),
      # fit_slope = (a_est + b_est * (Cm_est - Cz) ) * (time - time_corr) + Cz - slope_tz * tz_est,
      start_z = start + tz # this is for graph purpose, to have a vertical line showing where tz is for each flux
      
    ) #%>% 
    #dplyr::select(!time_diff)
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
  
  warning_msg <- conc_df_cut %>%
     dplyr::select(fluxID, n_conc, start, end) %>%
     dplyr::distinct() %>%
        # dplyr::group_by(fluxID, n_conc, start, end) %>%
          #  dplyr::reframe(
            # length_flux = max(time)
            # length_flux = difftime(end, start, units = "sec")
          #  ) %>%
              # dplyr::ungroup() %>%
                 dplyr::mutate(
                  length_flux = difftime(end, start, units = "sec"),
                  # count = as.numeric(count),
                  warnings = paste("\n","fluxID", fluxID, ": slope was estimated on", n_conc, "points out of", length_flux, "seconds because data are missing"),
                  warnings = dplyr::case_when(
                    length_flux != n_conc ~ warnings
                  ),
                  warnings = as.character(warnings)
                 ) %>%
                 drop_na(warnings) %>%
              dplyr::pull(warnings)
                #  view(warning_df)

  warnings <- stringr::str_c(warning_msg)

  if(any(!is.na(warnings)))  warning(warnings)
  # print(warning_df)

  return(conc_fitting)
}
