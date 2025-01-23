# works for exponential fitting

    Code
      flux_quality(slopes0, fit_type = "expo", slope_col = "f_slope")
    Message
      
       Total number of measurements: 6
      
       ok 	 2 	 33 %
       zero 	 4 	 67 %
       discard 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
    Output
      # A tibble: 1,251 x 39
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 32 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <dbl>,
      #   n_conc <dbl>, ratio <dbl>, flag <lgl>, f_time <dbl>, f_cut <chr>,
      #   Cm_est <dbl>, a_est <dbl>, b_est <dbl>, tz_est <dbl>, f_Cz <dbl>,
      #   time_diff <dbl>, f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>,
      #   f_slope <dbl>, f_fit <dbl>, f_fit_slope <dbl>, f_start_z <dttm>,
      #   f_n_conc <int>, f_ratio <dbl>, f_flag_ratio <chr>, f_start_error <chr>, ...

# works for linear fitting

    Code
      flux_quality(slopes30lin, )
    Message
      
       Total number of measurements: 6
      
       discard 	 1 	 17 %
       ok 	 5 	 83 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
    Output
      # A tibble: 1,251 x 27
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 20 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   n_conc <dbl>, ratio <dbl>, flag <chr>, f_time <dbl>, f_cut <fct>,
      #   f_pvalue <dbl>, f_rsquared <dbl>, f_adj_rsquared <dbl>, f_intercept <dbl>,
      #   f_slope <dbl>, f_fit <dbl>, f_n_conc <int>, f_ratio <dbl>,
      #   f_flag_ratio <chr>, f_start_error <chr>, f_quality_flag <chr>,
      #   f_slope_corr <dbl>

# works for quadratic fitting

    Code
      flux_quality(slopes30qua, fit_type = "quadratic")
    Message
      
       Total number of measurements: 6
      
       ok 	 6 	 100 %
       discard 	 0 	 0 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
    Output
      # A tibble: 1,251 x 30
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 23 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   n_conc <int>, ratio <dbl>, flag <chr>, f_time <dbl>, f_cut <fct>,
      #   f_rsquared <dbl>, f_adj_rsquared <dbl>, f_pvalue <dbl>, f_intercept <dbl>,
      #   f_param1 <dbl>, f_param2 <dbl>, f_slope <dbl>, f_fit <dbl>,
      #   f_fit_slope <dbl>, f_n_conc <int>, f_ratio <dbl>, f_flag_ratio <chr>,
      #   f_start_error <chr>, f_quality_flag <chr>, f_slope_corr <dbl>

# segmentation tool

    Code
      dplyr::distinct(select(flux_quality(slopes_pftc7), f_fluxID, f_mean_slope,
      f_mean_slope_corr, f_quality_flag, f_sd_slope))
    Message
      
       Total number of measurements: 13
      
       discard 	 3 	 23 %
       ok 	 10 	 77 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
    Output
      # A tibble: 13 x 5
         f_fluxID             f_mean_slope f_mean_slope_corr f_quality_flag f_sd_slope
         <fct>                       <dbl>             <dbl> <chr>               <dbl>
       1 5_2800_east_5_day_p~      -0.0863           -0.0863 ok                NA     
       2 5_2800_east_5_day_r~      -0.0882           -0.0882 ok                NA     
       3 5_2800_east_4_day_p~      NA                NA      discard           NA     
       4 5_2800_east_3_day_p~       0.0386            0.0386 ok                 0.0637
       5 5_2800_east_2_day_p~       0.0418            0.0418 ok                 0.0203
       6 5_2800_east_1_day_p~       0.0608            0.0608 ok                NA     
       7 5_2800_west_1_day_p~      -0.0973           -0.0973 ok                 0.0375
       8 5_2800_west_2_day_p~      NA                NA      discard           NA     
       9 5_2800_west_2_day_r~      -0.0582           -0.0582 ok                NA     
      10 5_2800_west_3_day_p~      NA                NA      discard           NA     
      11 5_2800_west_3_day_r~      -0.0432           -0.0432 ok                 0.0408
      12 5_2800_west_4_day_r~       0.0451            0.0451 ok                 0.0980
      13 5_2800_west_5_day_p~      -0.0267           -0.0267 ok                 0.0721

# segmentation tool without par

    Code
      dplyr::distinct(select(flux_quality(slopes_pftc7_nopar), f_fluxID, f_mean_slope,
      f_mean_slope_corr, f_quality_flag, f_sd_slope))
    Message
      
       Total number of measurements: 13
      
       discard 	 1 	 8 %
       ok 	 12 	 92 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
    Output
      # A tibble: 13 x 5
         f_fluxID             f_mean_slope f_mean_slope_corr f_quality_flag f_sd_slope
         <fct>                       <dbl>             <dbl> <chr>               <dbl>
       1 5_2800_east_5_day_p~       0.0542            0.0542 ok                 0.192 
       2 5_2800_east_5_day_r~      -0.0882           -0.0882 ok                NA     
       3 5_2800_east_4_day_p~      NA                NA      discard           NA     
       4 5_2800_east_3_day_p~       0.0386            0.0386 ok                 0.0637
       5 5_2800_east_2_day_p~       0.0418            0.0418 ok                 0.0203
       6 5_2800_east_1_day_p~       0.0608            0.0608 ok                NA     
       7 5_2800_west_1_day_p~      -0.0973           -0.0973 ok                 0.0375
       8 5_2800_west_2_day_p~       0.0912            0.0912 ok                 0.0773
       9 5_2800_west_2_day_r~       0.0721            0.0721 ok                 0.101 
      10 5_2800_west_3_day_p~       0.0762            0.0762 ok                 0.0567
      11 5_2800_west_3_day_r~      -0.0432           -0.0432 ok                 0.0408
      12 5_2800_west_4_day_r~       0.0451            0.0451 ok                 0.0980
      13 5_2800_west_5_day_p~      -0.0267           -0.0267 ok                 0.0721

