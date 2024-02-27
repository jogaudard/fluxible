library(tidyverse)
library(dataDownloader)
library(timetk)

#fetching the data
get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "data-raw",
         remote_path = "raw_data/v. c_flux_raw_data")

# making the dataset we want
record <- read_csv("data-raw/PFTC6_cflux_field-record_joasete.csv", col_types = "ffdDccc")

record_short <- record %>%
   select(turfID, type, starting_time, date) %>%
      mutate(
         starting_time = formatC(starting_time, width = 6, format = "d", flag = "0"), #to make sure all the time is 6 digits
         starting_time = gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
         date = ymd(date),
         start = ymd_hms(paste(date, starting_time)), #pasting date and time together to make datetime
      ) %>%
         select(!c(starting_time, date)) %>%
            filter( # we will just make it shorter and keep a couple of fluxes around midnight
         timetk::between_time(start, "2022-07-28 23:40:00", "2022-07-29 00:10:00")
         )

usethis::use_data(record_short, overwrite = TRUE)
