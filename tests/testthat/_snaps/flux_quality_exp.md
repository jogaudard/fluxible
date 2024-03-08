# quality works on exp fit

    Code
      flux_quality_exp(slopes0, fluxID_col = "fluxID", conc_col = "conc", b_col = "b",
        time_col = "time", fit_col = "fit", slope_col = "slope_tz", cut_col = "cut")
    Message <rlang_message>
      Joining with `by = join_by(f_fluxID, f_cut)`
      Joining with `by = join_by(f_fluxID)`
    Output
      # A tibble: 1,251 x 36
         datetime            temp_air temp_soil f_conc   PAR turfID       type 
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
      # i 29 more variables: start <dttm>, end <dttm>, f_fluxID <dbl>, n_conc <dbl>,
      #   ratio <dbl>, flag <lgl>, f_time <dbl>, f_cut <chr>, Cm_est <dbl>,
      #   a_est <dbl>, b_est <dbl>, tz_est <dbl>, Cz <dbl>, time_diff <dbl>,
      #   Cm <dbl>, a <dbl>, f_b <dbl>, tz <dbl>, f_slope_tz <dbl>, f_fit <dbl>,
      #   fit_slope <dbl>, start_z <dttm>, f_cor_coef <dbl>, f_RMSE <dbl>,
      #   f_start_error <chr>, f_fit_quality <chr>, f_correlation <chr>, ...

