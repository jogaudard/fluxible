# matching works

    Code
      dplyr::distinct(dplyr::select(flux_match(co2_df_short, record_short, datetime,
        start, conc, measurement_length = 180), f_fluxid, f_n_conc, f_ratio,
      f_flag_match, f_length))
    Output
      # A tibble: 6 x 5
        f_fluxid f_n_conc f_ratio f_flag_match f_length
        <fct>       <int>   <dbl> <chr>           <dbl>
      1 1             180       1 <NA>              180
      2 2             180       1 <NA>              180
      3 3             180       1 <NA>              180
      4 4             180       1 <NA>              180
      5 5             180       1 <NA>              180
      6 6             180       1 <NA>              180

# time_diff works

    Code
      flux_match(co2_df_short_180, record_short, datetime, start, conc,
        measurement_length = 220, time_diff = 180)
    Output
      # A tibble: 1,299 x 15
         datetime            temp_air temp_soil  conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:25    NA         NA    439. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:26     7.21      10.9  440.  2.29 156 AN2C 156 ER   
       3 2022-07-28 23:43:27    NA         NA    441. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:28    NA         NA    441. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:29    NA         NA    442. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:30    NA         NA    443. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:31    NA         NA    443. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:32    NA         NA    444. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:33    NA         NA    446. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:34    NA         NA    446. NA    156 AN2C 156 ER   
      # i 1,289 more rows
      # i 8 more variables: start <dttm>, f_start <dttm>, f_fluxid <fct>,
      #   f_end <dttm>, f_n_conc <int>, f_length <dbl>, f_ratio <dbl>,
      #   f_flag_match <chr>

# renaming variables works

    Code
      flux_match(co2_df_short, record_short, date_time, starting, CO2_conc,
        measurement_length = 220)
    Output
      # A tibble: 1,299 x 15
         date_time           temp_air temp_soil CO2_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>    <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:25    NA         NA       439. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:26     7.21      10.9     440.  2.29 156 AN2C 156 ER   
       3 2022-07-28 23:43:27    NA         NA       441. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:28    NA         NA       441. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:29    NA         NA       442. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:30    NA         NA       443. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:31    NA         NA       443. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:32    NA         NA       444. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:33    NA         NA       446. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:34    NA         NA       446. NA    156 AN2C 156 ER   
      # i 1,289 more rows
      # i 8 more variables: starting <dttm>, f_start <dttm>, f_fluxid <fct>,
      #   f_end <dttm>, f_n_conc <int>, f_length <dbl>, f_ratio <dbl>,
      #   f_flag_match <chr>

# flags on nb of data

    Code
      suppressWarnings(flux_match(co2_df_missing, record_short, datetime, start, conc,
        measurement_length = 220))
    Output
      # A tibble: 686 x 15
         datetime            temp_air temp_soil  conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:25       NA        NA   NA     NA 156 AN2C 156 ER   
       2 2022-07-28 23:45:37       NA        NA  514.    NA 156 AN2C 156 ER   
       3 2022-07-28 23:45:38       NA        NA  513.    NA 156 AN2C 156 ER   
       4 2022-07-28 23:45:39       NA        NA  513.    NA 156 AN2C 156 ER   
       5 2022-07-28 23:45:40       NA        NA  514.    NA 156 AN2C 156 ER   
       6 2022-07-28 23:45:41       NA        NA  514.    NA 156 AN2C 156 ER   
       7 2022-07-28 23:45:42       NA        NA  515.    NA 156 AN2C 156 ER   
       8 2022-07-28 23:45:43       NA        NA  515.    NA 156 AN2C 156 ER   
       9 2022-07-28 23:45:44       NA        NA  515.    NA 156 AN2C 156 ER   
      10 2022-07-28 23:45:45       NA        NA  515.    NA 156 AN2C 156 ER   
      # i 676 more rows
      # i 8 more variables: start <dttm>, f_start <dttm>, f_fluxid <fct>,
      #   f_end <dttm>, f_n_conc <int>, f_length <dbl>, f_ratio <dbl>,
      #   f_flag_match <chr>

# matching works with end col

    Code
      dplyr::distinct(dplyr::select(flux_match(co2_df_short, record_short_end,
        datetime, start, conc, end, fixed_length = FALSE), f_fluxid, f_ratio,
      f_flag_match, f_length))
    Output
      # A tibble: 6 x 4
        f_fluxid f_ratio f_flag_match f_length
        <fct>      <dbl> <chr>           <dbl>
      1 1              1 <NA>              120
      2 2              1 <NA>              180
      3 3              1 <NA>              120
      4 4              1 <NA>              180
      5 5              1 <NA>              120
      6 6              1 <NA>              180

