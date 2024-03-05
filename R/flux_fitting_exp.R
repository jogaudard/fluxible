# function to fit the CO2 concentration to a function of time, as described in Zhao 2018

# to do list
# clean unused parts and stuff in comments
# add documentation
# make test on longer datasets
# make test with negative fluxes (the dataset used so far has only positive fluxes)

#' Fitting a model to the gas concentration curve and estimating the slope
#' @description Fits an exponential expression to the concentration evolution over time
#' @param conc_df dataframe of gas concentration over time
#' @param t_window enlarge focus window before and after tmin and tmax
#' @param Cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz where it is assumed that C fits the data perfectly
#' @param a_window window at the end of the flux to estimate a
#' @param roll_width width of the rolling mean for CO2 when looking for tz, idaelly same as Cz_window
#' @param start_cut to cut at the start
#' @param end_cut to cut at the end, if you notice on the graphs that the match was not precise enough
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxID_col column with ID of each flux
#' @return a dataframe with the slope at t zero (should be used for flux calculation), modelled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when ungroup filter distinct left_join rowwise summarize pull
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data
#' #' @examples 
#' data(co2_conc)
#' flux_fitting_exp(co2_conc)
#' @export


flux_fitting_exp <- function(conc_df,
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
                              end_cut = 0, # to cut at the end, if you notice on the graphs that the match was not precise enough
                              start_col = "start",
                              end_col = "end",
                              datetime_col = "datetime",
                              conc_col = "conc",
                              fluxID_col = "fluxID"
){ 
  
  #renaming columns
  conc_df <- conc_df |>
     rename(
      start = all_of((start_col)),
      end = all_of((end_col)),
      datetime = all_of((datetime_col)),
      conc = all_of((conc_col)),
      fluxID = all_of((fluxID_col))
     )


   # we need to check that values provided to the function are what we expect and will not crash the function

   if(!is.double(t_window)) stop("t_window has to be a double")
   if(!is.double(Cz_window)) stop("Cz_window has to be a double")
   if(!is.double(b_window)) stop("b_window has to be a double")
   if(!is.double(a_window)) stop("a_window has to be a double")
  #  if(!is.double(length_flux)) stop("length_flux has to be a double")
   if(!is.double(roll_width)) stop("roll_width has to be a double")
   if(!is.double(start_cut)) stop("start_cut has to be a double")
   if(!is.double(end_cut)) stop("end_cut has to be a double")


# need also to check that all the parameters are making sense (cutting has to be shorter than the window for ex)

# start_cut + end_cut has to be shorter than the flux length

length_flux_max <- conc_df |>
   mutate(
    length_flux = difftime(.data$end, .data$start, units = "sec"),
    length_flux = as.double(.data$length_flux)
   ) |>
      select("length_flux") |>
         max()

if((start_cut + end_cut) >= length_flux_max) {stop("You cannot cut more than the length of the measurements! ((start_cut + end_cut) >= length_flux_max)")}

  
  # we will try to calculate all the parameters without a, and then insert a in the end
  
  conc_df <- conc_df |> 
       group_by(.data$fluxID) |> 
       mutate(
      time = difftime(.data$datetime[1:length(.data$datetime)],.data$datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
      time = as.double(.data$time),
      start = .data$start + ((start_cut)),
      end = .data$end - ((end_cut)),
      cut = case_when(
        .data$datetime < .data$start | .data$datetime >= .data$end ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(.data$cut),
      n_conc = sum(!is.na(.data$conc)) # already done in match function but I want the functions to be independant
    ) |> 
    ungroup()
  
conc_df_cut <- conc_df |>
   filter(
      cut == "keep"
    )  |>
       drop_na("conc") |> # drop NA in conc to avoid messing up the models used later, will have to print a warning for that
           group_by(.data$fluxID) |>
           mutate(
            time_cut = difftime(.data$datetime[1:length(.data$datetime)],.data$datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
            time_cut = as.double(.data$time_cut), # we need time_cut because we dropped the NA in conc
            # time_cut = time, # maybe it can just be the same, it doesn't have to start at 0
            length_window = max(.data$time_cut), #to have length_flux for each flux, better than setting it up as a function argument, we use time_cut because we want the time where there are conc data
            length_flux = difftime(.data$end, .data$start, units = "sec"), # the length of the flux after cutting, does not mean there is data for all the seconds
            # length_flux = as.double(length_window),
            # length_window = max(time_cut) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            # length_window = max(time) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            time_diff = .data$time - .data$time_cut,
            n_conc_cut = sum(!is.na(.data$conc)) # nb of conc data after cutting, for warnings

                 ) |>
                    ungroup()


  Cm_temp <- conc_df_cut |> 
       group_by(.data$fluxID) |> 
    distinct(.data$conc, .keep_all = TRUE) |> 
    mutate(
      Cmax = max(.data$conc),
      Cmin = min(.data$conc),
      # tmax = time[conc == Cmax],
      # tmin = time[conc == Cmin]
      tmax = .data$time_cut[.data$conc == .data$Cmax],
      tmin = .data$time_cut[.data$conc == .data$Cmin]
    ) |> 
    select("fluxID", "Cmax", "Cmin", "tmax", "tmin") |> 
    ungroup() |> 
    distinct(.data$Cmax, .data$Cmin, .keep_all = TRUE)
  
  Cm_slope <- conc_df_cut |> 
    group_by(.data$fluxID) |> 
    nest()  |>
    mutate(
      model_Cm = map(.data$data, \(d)
      lm(conc ~ time_cut, data = d) |>
      broom::tidy() |>
      select("term", "estimate") |>
      pivot_wider(names_from = "term", values_from = "estimate")
      )
    ) |>
    unnest("model_Cm") |>
    rename(slope_Cm = "time_cut") |> 
    select("fluxID", "slope_Cm") |> 
    ungroup()
  
  Cm_df <- left_join(Cm_temp, Cm_slope) |> 
    mutate(
      Cm_est = case_when(
        .data$slope_Cm < 0 ~ .data$Cmin, #Cm is the maximum mixing point, which is when the limit of C(t) when t tends to infinite.
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
      # time <= Cz_window + start_cut
    ) |>
    nest()  |>
    mutate(
      model_Cz = map(.data$data, \(d)
      lm(conc ~ time_cut, data = d) |>
      broom::tidy() |>
      select("term", "estimate") |>
      pivot_wider(names_from = "term", values_from = "estimate")
      )
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
      # time > Cz_window
      .data$time_cut < .data$length_window / 2 # tz should be in the first half of the flux
      # time_cut < (length_window / 2) + start_cut # tz should be in the first half of the flux
            # time < (length_window / 2) + start_cut # tz should be in the first half of the flux
    ) |>
    mutate(
      conc_roll = zoo::rollmean(.data$conc, k = ((roll_width)), fill = NA, align = "right"),
      Cd = abs(.data$conc_roll - .data$Cz),
      minCd = min(.data$Cd, na.rm = TRUE),
      # tz_est = min(time[Cd == minCd], na.rm = TRUE)
      tz_est = min(.data$time_cut[.data$Cd == .data$minCd], na.rm = TRUE)
    ) |> 
    ungroup() |> 
    select("fluxID", "tz_est") |> 
    distinct()
  
  # a_df <- CO2_df |> # a is the slope at the end of the flux
  #   group_by(fluxID) |> 
  #   filter(
  #     datetime >= end - t_window
  #   ) |> 
  #   do({model = lm(CO2 ~ time, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))}) |>          # get model info
  #   pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") |> 
  #   rename(
  #     a_est = time
  #   ) |> 
  #   select(fluxID, a_est) |> 
  #   ungroup()
  
  Cb_df <- conc_df_cut |> 
    left_join(tz_df) |> 
    group_by(.data$fluxID) |> 
    mutate(
      Cb = .data$conc[.data$time_cut == .data$tz_est - ((b_window))]
      # Cb = case_when(tz_est - b_window < 0 ~ conc[time == start_cut],
      #                       tz_est - b_window >= 0 ~ conc[time == tz_est - b_window]
      # )
    ) |> 
    ungroup() |> 
    select("fluxID", "Cb") |> 
    distinct()
  
  a_df <- conc_df_cut |> 
    group_by(.data$fluxID) |> 
    mutate(
      ta = .data$length_window - ((a_window)),
      # ta = length_window - a_window + start_cut,
      Ca = .data$conc[.data$time_cut == .data$ta]
      # Ca = conc[time == ta]
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
        TRUE ~ log(abs((.data$Cb - .data$Cm_est)/(.data$Cz - .data$Cm_est))) * (1/((b_window))), # problem with the log? negative value? let's try with absolute value
      ),
      a_est = case_when(
        .data$ta == .data$tz_est ~ 0, # tz_est = ta is a special case that is undefined
        TRUE ~ (.data$Ca - .data$Cm_est - (.data$Cz - .data$Cm_est) * exp(-.data$b_est * (.data$ta - .data$tz_est))) / (.data$ta - .data$tz_est)
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
  
  
  
  
  fitting_par <- conc_df_cut |> 
    left_join(estimates_df) |> 
    # select(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time, conc) |>
    select("fluxID", "Cm_est", "a_est", "b_est", "tz_est", "Cz", "time_cut", "conc", "time_diff") |>
    group_by(.data$fluxID, .data$Cm_est, .data$a_est, .data$b_est, .data$tz_est, .data$Cz, .data$time_diff) |>
    nest() |> 
    rowwise() |>
    summarize(
      # I would like to do something more resilient to avoid stopping everything if there is a problem with optim. Maybe tryCatch can be an idea
      results = list(optim(par = c(.data$Cm_est, .data$a_est, .data$b_est, log(.data$tz_est)), fn = myfn, conc = data$conc, time = data$time_cut, Cz = .data$Cz)), #, lower=c(0, -Inf, -Inf, 0),  method="L-BFGS-B"
      Cm = .data$results$par[1],
      a = .data$results$par[2],
      # b = exp(results$par[3]),#/(abs(results$par[3])+1),
      b = .data$results$par[3],
      # b = 1/(1+exp(-results$par[3])),
      #need to find the fit with the b closest to 0 (negative or positive)
      tz = exp(.data$results$par[4]), #we force tz to be positive
      slope_tz = .data$a + .data$b * (.data$Cm - .data$Cz),
    ) |> 
    ungroup() |>
       select(!"results") # we do not need it anymore and it is confusing to have them as tibble


  conc_fitting <- conc_df |> 
    left_join(fitting_par) |> 
    group_by(.data$fluxID) |>
    mutate(
      # time_corr = difftime(start_window, start, units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      # time_corr = as.double(time_corr),
      # fit = case_when(
      #   is.na(time) ~ NA_real_,
      #   !is.na(time) ~ Cm + a * (time - tz) + (Cz - Cm) * exp(- b * (time - tz))
      # ), # problem here because tz is calculated compared to time_cut
      # fit_slope = case_when(
      #   is.na(time) ~ NA_real_,
      #   !is.na(time) ~ slope_tz * (time) + Cz - slope_tz * tz
      # ),
      # need to correct tz for the time diff between time and time_cut
      # time_diff = time[1] - time_cut[1],
      fit = .data$Cm + .data$a * (.data$time - .data$tz - .data$time_diff) + (.data$Cz - .data$Cm) * exp(- .data$b * (.data$time - .data$tz - .data$time_diff)),
      fit_slope = .data$slope_tz * (.data$time) + .data$Cz - .data$slope_tz * (.data$tz + .data$time_diff),
      # fit = Cm + a * (time - tz) + (Cz - Cm) * exp(- b * (time - tz)),
      # fit_slope = slope_tz * (time) + Cz - slope_tz * tz,
      # fit = Cm_est + a_est * (time - tz_est - time_corr) + (Cz - Cm_est) * exp(- b_est * (time - tz_est - time_corr)),
      # fit_slope = (a_est + b_est * (Cm_est - Cz) ) * (time - time_corr) + Cz - slope_tz * tz_est,
      start_z = .data$start + .data$tz # this is for graph purpose, to have a vertical line showing where tz is for each flux
      
    ) |> 
    ungroup()



    #select(!time_diff)
  # group_by(fluxID, Cm, a, b, tz, Cz) |> 
  #   # nest() |> 
  #   mutate(
  #     RMSE = myfn2(time = time, CO2 = CO2, a = a, b = b, Cm = Cm, Cz = Cz, tz = tz - time_corr)
  #   ) |> 
  #   ungroup
  
  # r2_general <-function(preds,actual){ 
  #   return(1 - (sum((actual - preds)^2)/sum((actual - mean(actual))^2)))
  # }
  
  # model_fit <- CO2_fitting |>
  #   group_by(fluxID) |> 
  #   nest() |> 
  #   rowwise() |> 
  #   summarize(
  #     cor_coef = cor(data$CO2, data$time),
  #     r.squared_full = r2_general(data$fit, data$CO2),
  #     RMSE_full = sqrt((1/length(data$time)) * sum((data$fit - data$CO2)^2))
  #   )
  
  # model_fit <- CO2_fitting |>
  #   # filter(
  #   #   cut == "keep"
  #   # ) |>
  #   group_by(fluxID) |>
  #   nest() |>
  #   rowwise() |>
  #   # select(fluxID, time, CO2, fit, fit_slope, Cm, a, b, Cz, tz, time_corr) |>
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
  #   ) |>
  #   ungroup()
  
  # CO2_fitting <- CO2_fitting |>
  #   left_join(model_fit)
  #   left_join(model_fit_cut)
  # 
  # now we need a set of rules to assess if the data should be kept, discarded, or replaced by 0
  # kept: good fit
  # discarded: bad fit, but with changes in CO2 concentration over time
  # replaced by 0: bad fit, and CO2 concentration does not change over time (just noise). How do we assess that?
  # Maybe we just set a threshold on the slope below which the slope is replaced by 0 if the fit is bad
  # This threshold can be calculated as the amplitude of the noise of the setup (to be fed into the function, depnding on the setup) divided by the lenght of the measurement
  
  # CO2_fitting <- CO2_fitting |> 
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
  
  warning_msg <- conc_df |>
    left_join(conc_df_cut) |> # we want n_conc after cutting
     select("fluxID", "n_conc", "n_conc_cut", "length_flux") |>
     distinct() |>
        # group_by(fluxID, n_conc, start, end) |>
          #  reframe(
            # length_flux = max(time)
            # length_flux = difftime(end, start, units = "sec")
          #  ) |>
              # ungroup() |>
                 mutate(
                  # length_flux = difftime(end, start, units = "sec"),
                  # count = as.numeric(count),
                  low_data = paste("\n","fluxID", .data$fluxID, ": slope was estimated on", .data$n_conc_cut, "points out of", .data$length_flux, "seconds because data are missing"),
                  no_data = paste("\n","fluxID", .data$fluxID, ": slope could not be estimated because there are no data in the conc column"),
                  warnings = case_when(
                    .data$n_conc == 0 ~ .data$no_data,
                    .data$n_conc_cut != .data$length_flux ~ .data$low_data
                  ),
                  warnings = as.character(.data$warnings)
                 ) |>
                 drop_na(warnings) |>
              pull(.data$warnings)
                #  view(warning_df)

  warnings <- str_c(warning_msg)

  if(any(!is.na(warnings)))  warning(warnings)
  # print(warning_df)

  attr(conc_fitting, "fit_type") <- "exponential"

  conc_fitting
}
