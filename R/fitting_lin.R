#' linear fit to gas concentration over time
#' @description fits a linear model to the gas concentration over time
#' @param conc_df dataframe of gas concentration over time
#' @param start_cut to cut at the start
#' @param end_cut to cut at the end, if you notice on the graphs that the match was not precise enough
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxID_col column with ID of each flux
#' @return a df with the modelled gas concentration, slope, intercept, std error, r square and p value of the linear model
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when ungroup filter do left_join distinct pull
#' @importFrom tidyr drop_na pivot_wider fill 
#' @importFrom haven as_factor
# #' @importFrom purrr ::
#' @importFrom stringr str_c
# replace do with a nest mutate with map

flux_fitting_lin <- function(conc_df,
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
      start = all_of(start_col),
      end = all_of(end_col),
      datetime = all_of(datetime_col),
      conc = all_of(conc_col),
      fluxID = all_of(fluxID_col)
     )

    if(!is.double(start_cut)) stop("start_cut has to be a double")
   if(!is.double(end_cut)) stop("end_cut has to be a double")

   length_flux_max <- conc_df |>
   mutate(
    length_flux = difftime(end, start, units = "sec"),
    length_flux = as.double(length_flux)
   ) |>
      select(length_flux) |>
         max()

if((start_cut + end_cut) >= length_flux_max) {stop("You cannot cut more than the length of the measurements! ((start_cut + end_cut) >= length_flux_max)")}

conc_df <- conc_df |> 
       group_by(fluxID) |> 
       mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
      time = as.double(time),
      start = start + start_cut,
      end = end - end_cut,
      cut = case_when(
        datetime < start | datetime >= end ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(cut),
      n_conc = sum(!is.na(conc)) # already done in match function but I want the functions to be independant
    ) |> 
    ungroup()
  
conc_df_cut <- conc_df |>
   filter(
      cut == "keep"
    )  |>
       drop_na(conc) |> # drop NA in conc to avoid messing up the models used later, will have to print a warning for that
           group_by(fluxID) |>
           mutate(
            time_cut = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), # I am not sure what happens here if some rows are missing
            time_cut = as.double(time_cut), # we need time_cut because we dropped the NA in conc
            # time_cut = time, # maybe it can just be the same, it doesn't have to start at 0
            length_window = max(time_cut), #to have length_flux for each flux, better than setting it up as a function argument, we use time_cut because we want the time where there are conc data
            length_flux = difftime(end, start, units = "sec"), # the length of the flux after cutting, does not mean there is data for all the seconds
            # length_flux = as.double(length_window),
            # length_window = max(time_cut) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            # length_window = max(time) - start_cut #to have length_flux for each flux, better than setting it up as a function argument
            time_diff = time - time_cut,
            n_conc_cut = sum(!is.na(conc)) # nb of conc data after cutting, for warnings

                 ) |>
                    ungroup()

    fitting_par <- conc_df_cut |> 
    group_by(fluxID) |> 
    do({model = lm(conc ~ time_cut, data=.)    # create your model
    data.frame(broom::tidy(model),              # get coefficient info
               broom::glance(model))}) |>          # get model info
               select(fluxID, term, estimate, std.error, p.value, r.squared, adj.r.squared) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    rename(
        intercept = "(Intercept)",
        slope = time_cut
        ) |>
    fill(intercept, .direction = "down") |>
    drop_na(slope) |> #there must be a less dirty way to do this!
       ungroup()

    conc_fitting <- conc_df |> 
    left_join(fitting_par) |> 
    # group_by(fluxID) |>
    mutate(
            fit = intercept + slope * (time - start_cut)
    )

    warning_msg <- conc_df |>
    left_join(conc_df_cut) |> # we want n_conc after cutting
     select(fluxID, n_conc, n_conc_cut, length_flux) |>
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
                  low_data = paste("\n","fluxID", fluxID, ": slope was estimated on", n_conc_cut, "points out of", length_flux, "seconds because data are missing"),
                  no_data = paste("\n","fluxID", fluxID, ": slope could not be estimated because there are no data in the conc column"),
                  warnings = case_when(
                    n_conc == 0 ~ no_data,
                    n_conc_cut != length_flux ~ low_data
                  ),
                  warnings = as.character(warnings)
                 ) |>
                 drop_na(warnings) |>
              pull(warnings)
                #  view(warning_df)

  warnings <- str_c(warning_msg)

  if(any(!is.na(warnings)))  warning(warnings)

  return(conc_fitting)
}
