# just a script to produce plots to include in the poster
# we can use the lia data used in the readme
library(fluxible)
library(progress)
library(tidyverse)

conc_liahovden <- flux_match(
  co2_liahovden,
  record_liahovden
)
slopes_exp_liahovden <- flux_fitting(
  conc_liahovden,
  fit_type = "exponential"
)
slopes_exp_liahovden <- flux_quality(
  slopes_exp_liahovden,
  fit_type = "expo",
  slope_col = "f_slope"
  )

# special function to make the plots for the poster (bigger and co)
flux_plot_exp_poster <- function(slopes_df,
                      color_discard = "#D55E00",
                      color_cut = "#D55E00",
                      color_ok = "#009E73",
                      color_zero = "#CC79A7",
                      linewidth = 1,
                      size_point = 1,
                      f_date_breaks = "1 min",
                      f_minor_breaks = "10 sec",
                      f_date_labels = "%e/%m \n %H:%M",
                      f_ylim_upper = 800,
                      f_ylim_lower = 400,
                      f_plotname = "plot_quality",
                      facet_wrap_args = list(
                        ncol = 2,
                        nrow = 2,
                        scales = "free"
                      ),
                      y_text_position = 500,
                      cut_arg = "cut",
                      no_data_flag = "no_data") {
  

  slopes_df <- slopes_df |>
    filter(
      .data$f_quality_flag != ((no_data_flag))
    )




  param_df <- flux_param_exp(
    ((slopes_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- flux_plot_flag(((slopes_df)),
    ((param_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- slopes_df |>
    rename(
      fit = "f_fit",
      slope = "f_fit_slope"
    ) |>
    pivot_longer(
      cols = c("fit", "slope"),
      names_to = "linetype",
      values_to = "fit"
    )



  f_plot <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_quality_flag),
      size = ((size_point)),
      na.rm = TRUE
    ) +
    geom_text(
      aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

  message("Plotting in progress")

  f_plot <- f_plot +
    geom_line(
      aes(y = .data$fit, linetype = .data$linetype),
      linewidth = ((linewidth)),
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    scale_color_manual(values = c(
      "cut" = ((color_cut)),
      "ok" = ((color_ok)),
      "discard" = ((color_discard)),
      "zero" = ((color_zero)),
      "start_error" = ((color_discard)),
      "weird_flux" = ((color_discard)),
      "force_ok" = ((color_ok))
    )) +
    scale_linetype_manual(values = c(
      "fit" = "longdash",
      "slope" = "dashed"
    )) +
    scale_x_datetime(
      date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
      date_labels = ((f_date_labels))
    ) +
    ylim(((f_ylim_lower)), ((f_ylim_upper))) +
    do.call(facet_wrap_paginate,
      args = c(facets = ~f_fluxID, ((facet_wrap_args)))
    ) +
    labs(
      title = "Fluxes quality assessment",
      subtitle = "Exponential model",
      x = "Datetime",
      y = "Concentration",
      colour = "Quality flags",
      linetype = "Fits"
    ) +
    guides(color = guide_legend(override.aes = list(linetype = 0))) +
    theme_classic()


    # ggsave("poster_plot.png", width = 255, height = 170, units = c("mm"), dpi = 300,)

    f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
    pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = n_pages(f_plot)
    )
    pb$tick(0)
    Sys.sleep(3)
    for (i in 1:n_pages(f_plot)) {
      pb$tick()
      Sys.sleep(0.1)
      print(f_plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_fluxID,
            page = i,
            ((facet_wrap_args))
          )
        ))
    }
    quietly(dev.off())
    
}



slopes_exp_liahovden |>
  # dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
  #   mutate(
  #       f_fluxID = case_when(
  #           f_fluxID == 28 ~ "A",
  #           f_fluxID == 51 ~ "B",
  #           f_fluxID == 100 ~ "C"
  #       ),
  #       f_fluxID = factor(f_fluxID, levels = c("A", "B", "C"))
  #   ) |>
    # view()
    flux_plot_exp_poster(
      linewidth = 0.8,
      size_point = 1,
      f_plotname = "poster_plot",
      f_ylim_lower = 375,
      f_ylim_upper = 525,
      y_text_position = 470
      )

co2_joasete <- read_csv(
  "data-raw/PFTC6_CO2_joasete_2022.csv",
  col_types = "ccdddddd",
  na = c("#N/A", "Over", "Invalid")
)

co2_joasete <- co2_joasete %>%
  rename(
    datetime = "Date/Time",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    conc = "CO2 (ppm)",
    PAR = "PAR (umolsm2)"
  ) %>%
  mutate(
    datetime = dmy_hms(datetime)
  ) %>%
  select(
    datetime, temp_air, temp_soil, conc, PAR
  ) # we keep just the variables we need

record_joasete <- read_csv(
  "data-raw/PFTC6_cflux_field-record_joasete.csv"
)

record_joasete <- record_joasete %>%
  select(turfID, type, starting_time, date) %>%
  mutate(
    starting_time = formatC(
      starting_time,
      width = 6, format = "d", flag = "0"
    ), # to make sure all the time is 6 digits
    starting_time = gsub(
      "(\\d{2})(?=\\d{2})", "\\1:", starting_time,
      perl = TRUE
    ), # to add the : in the time
    date = ymd(date),
    start = ymd_hms(
      paste(date, starting_time)
    ), # pasting date and time together to make datetime
  ) %>%
  select(!c(starting_time, date))


conc <- flux_match(
  co2_joasete,
  record_joasete
)

slopes <- flux_fitting(
  conc,
  fit_type = "exp",
  end_cut = 0
)

slopes_flag <- flux_quality(
  slopes
)

slopes_flag |>
  # dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
  #   mutate(
  #       f_fluxID = case_when(
  #           f_fluxID == 28 ~ "A",
  #           f_fluxID == 51 ~ "B",
  #           f_fluxID == 100 ~ "C"
  #       ),
  #       f_fluxID = factor(f_fluxID, levels = c("A", "B", "C"))
  #   ) |>
  flux_plot_exp_poster(
      linewidth = 0.8,
      size_point = 1,
      f_plotname = "poster_plot",
      f_ylim_lower = 375,
      f_ylim_upper = 700,
      y_text_position = 470
      )

# try with INCLINE data

library(dataDownloader)
library(fluxible)
library(tidyverse)
library(fs)



# download and read data -----------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_CO2_2022.zip",
         path = "data-raw",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2022.csv",
         path = "data-raw",
         remote_path = "RawData/C-Flux")

# get_file(node = "zhk3m",
#          file = "INCLINE_metadata.csv",
#          path = "data/C-Flux/summer_2022/raw_data",
#          remote_path = "RawData")

# Unzip files
zipfile <- "data-raw/INCLINE_CO2_2022.zip"
if(file.exists(zipfile)){
  outdir <- "data-raw/INCLINE"
  unzip(zipfile, exdir = outdir)
}

#importing fluxes data
location <- "data-raw/INCLINE" #location of datafiles

raw_CO2_INCLINE_2022 <- dir_ls(location, regexp = "*CO2_campaign*")  |>
  map_dfr(read_csv,  na = c("#N/A", "Over")) |>
  rename( #rename the column to get something more practical without space
    CO2 = "CO2 (ppm)",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    PAR = "PAR (umolsm2)",
    datetime = "Date/Time"
  ) %>%  
  mutate(
    datetime = dmy_hms(datetime)
  ) %>%
  select(datetime, CO2, PAR, temp_air, temp_soil)

record <- read_csv("data-raw/INCLINE_field-record_2022.csv", na = c(""), col_types = "fffccfc") %>% 
  drop_na(starting_time) %>%  #delete row without starting time (meaning no measurement was done)
  mutate(
    starting_time = case_when(
      campaign != 1 ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # campaing 1 was written as hh:mm:ss and others as hhmmss
      campaign == 1 ~ starting_time
      ),
    start = paste(date, starting_time),
    start = ymd_hms(start)
  )

str(raw_CO2_INCLINE_2022)
str(record)

# match fluxes and CO2 concentration --------------------------------------

CO2_INCLINE_2022 <- flux_match(
  raw_CO2_INCLINE_2022,
  record,
  startcrop = 0,
  measurement_length = 180,
  conc_col = "CO2"
) |>
filter(f_fluxID %in% c(408, 255)) |>
# filter(f_fluxID == 408) |>
select(f_datetime, f_conc, f_fluxID, f_start, f_end)


# str(CO2_INCLINE_2022)

slopes <- flux_fitting(CO2_INCLINE_2022, fit_type = "exp")

slopes_flag2022 <- flux_quality(
  slopes
)

slopes_flag2022 |>
  # dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
  #   mutate(
  #       f_fluxID = case_when(
  #           f_fluxID == 28 ~ "A",
  #           f_fluxID == 51 ~ "B",
  #           f_fluxID == 100 ~ "C"
  #       ),
  #       f_fluxID = factor(f_fluxID, levels = c("A", "B", "C"))
  #   ) |>
  flux_plot_exp_poster(
      linewidth = 0.8,
      size_point = 1,
      f_plotname = "poster_plot",
      f_ylim_lower = 375,
      f_ylim_upper = 525,
      y_text_position = 470
      )

# INCLINE 2020

get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data-raw/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2020.csv",
         path = "data-raw",
         remote_path = "RawData/C-Flux")

# Unzip files
zipFile <- "data-raw/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data-raw/summer_2020"
  unzip(zipFile, exdir = outDir)
}

location <- "data-raw/summer_2020/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    date = dmy(Date), #convert date in POSIXct
    datetime = as_datetime(paste(date, Time)),  #paste date and time in one column
    CO2 = CO2 + 80 # gas analyzer is off and that will be difficult to explain in the poster, but it does not affect the slope anyway
  ) %>%
  select(datetime,CO2)

incline <- read_csv("data-raw/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcfc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    date = dmy(date),
    start = ymd_hms(paste(date, starting_time)) #converting the date as posixct, pasting date and starting time together
  )

conc2020 <- flux_match(
  fluxes,
  incline,
  measurement_length = 180,
  conc_col = "CO2"
) |>
  filter(
    f_fluxID == 37
    # f_fluxID %in% c(
      # 37,
      # 688,
      # 471,
      # 20,
      # 467,
      # 698,
      # 13,
      # 21,
      # 19,
      # 22,
      # 23,
      # 24
    # )
    ) |>
  select(
    f_datetime, f_conc, f_fluxID, f_start, f_end
  )

 str(conc2020)

slope2020 <- flux_fitting(
  conc2020,
  fit_type = "exp"
)

slope2020_flag <- flux_quality(
  slope2020,
  error = 200
)

conc_poster <- bind_rows(slope2020_flag, slopes_flag2022) |>
  select(f_datetime, f_conc, f_fluxID, f_quality_flag, f_start, f_RMSE, f_cor_coef, f_b, f_cut, f_fit, f_fit_slope)

conc_poster |>
  # dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
    mutate(
        f_fluxID = case_when(
            f_fluxID == 37 ~ "A",
            f_fluxID == 255 ~ "B",
            f_fluxID == 408 ~ "C"
        ),
        f_fluxID = factor(f_fluxID, levels = c("A", "B", "C"))
    ) |>
  flux_plot_exp_poster(
      linewidth = 0.8,
      size_point = 1,
      f_plotname = "poster_plot",
      f_ylim_lower = 350,
      f_ylim_upper = 500,
      y_text_position = 470
      )

# just to keep the data

usethis::use_data(conc_poster, overwrite = TRUE)
