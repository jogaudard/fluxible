# fitting works with 0 second end cut

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "exp"), f_fluxID, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exp()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxID f_slope
        <fct>      <dbl>
      1 1          1.56 
      2 2          0.853
      3 3          0.303
      4 4          1.13 
      5 5          1.46 
      6 6          0.426

# fitting works with 30 second end cut

    Code
      flux_fitting(co2_conc, end_cut = 30, fit_type = "exp")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Output
      # A tibble: 1,251 x 29
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
      # i 22 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   n_conc <int>, ratio <dbl>, flag <chr>, f_time <dbl>, f_cut <fct>,
      #   Cm_est <dbl>, a_est <dbl>, b_est <dbl>, tz_est <dbl>, f_Cz <dbl>,
      #   time_diff <dbl>, f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>,
      #   f_slope <dbl>, f_fit <dbl>, f_fit_slope <dbl>, f_start_z <dttm>

# fitting works with 60 second end cut

    Code
      flux_fitting(co2_conc, end_cut = 60, fit_type = "exp")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Output
      # A tibble: 1,251 x 29
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
      # i 22 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   n_conc <int>, ratio <dbl>, flag <chr>, f_time <dbl>, f_cut <fct>,
      #   Cm_est <dbl>, a_est <dbl>, b_est <dbl>, tz_est <dbl>, f_Cz <dbl>,
      #   time_diff <dbl>, f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>,
      #   f_slope <dbl>, f_fit <dbl>, f_fit_slope <dbl>, f_start_z <dttm>

# renaming works

    Code
      flux_fitting(co2_conc_names, datetime_col = "date_time", end_col = "finish",
        conc_col = "co2", fit_type = "exp")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exp()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 1,251 x 29
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
      # i 22 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   n_conc <int>, ratio <dbl>, flag <chr>, f_time <dbl>, f_cut <fct>,
      #   Cm_est <dbl>, a_est <dbl>, b_est <dbl>, tz_est <dbl>, f_Cz <dbl>,
      #   time_diff <dbl>, f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>,
      #   f_slope <dbl>, f_fit <dbl>, f_fit_slope <dbl>, f_start_z <dttm>

