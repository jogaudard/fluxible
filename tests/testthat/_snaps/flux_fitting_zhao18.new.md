# fitting works with 0 second end cut

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "exponential"),
      f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
    Condition
      Warning:
      There were 6 warnings in `summarize()`.
      The first warning was:
      i In argument: `results = list(...)`.
      i In row 1.
      Caused by warning in `minpack.lm::nls.lm()`:
      ! lmdif: info = 0. Improper input parameters.
      i Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
    Message
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid  f_slope
        <fct>       <dbl>
      1 1        -0.0603 
      2 2        -0.00852
      3 3         0.265  
      4 4        -0.0139 
      5 5        -0.141  
      6 6         0.356  

# fitting works with 30 second end cut

    Code
      flux_fitting(co2_conc, conc, datetime, end_cut = 30, fit_type = "exponential")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
    Condition
      Warning:
      There were 6 warnings in `summarize()`.
      The first warning was:
      i In argument: `results = list(...)`.
      i In row 1.
      Caused by warning in `minpack.lm::nls.lm()`:
      ! lmdif: info = 0. Improper input parameters.
      i Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
    Message
      Calculating fits and slopes...
      Done.
    Output
      # A tibble: 1,251 x 23
         datetime            temp_air temp_soil  conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA    447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9  447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA    448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA    449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA    449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA    450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA    451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA    451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA    453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA    453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 16 more variables: f_start <dttm>, f_end <dttm>, f_fluxid <fct>,
      #   f_ratio <dbl>, f_flag_match <chr>, f_time <dbl>, f_cut <fct>, f_Cz <dbl>,
      #   f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>, f_slope <dbl>, f_fit <dbl>,
      #   f_fit_slope <dbl>, f_start_z <dttm>

# fitting works with 60 second end cut

    Code
      flux_fitting(co2_conc, conc, datetime, end_cut = 60, fit_type = "exponential")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
    Condition
      Warning:
      There were 6 warnings in `summarize()`.
      The first warning was:
      i In argument: `results = list(...)`.
      i In row 1.
      Caused by warning in `minpack.lm::nls.lm()`:
      ! lmdif: info = 0. Improper input parameters.
      i Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
    Message
      Calculating fits and slopes...
      Done.
    Output
      # A tibble: 1,251 x 23
         datetime            temp_air temp_soil  conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA    447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9  447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA    448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA    449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA    449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA    450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA    451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA    451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA    453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA    453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 16 more variables: f_start <dttm>, f_end <dttm>, f_fluxid <fct>,
      #   f_ratio <dbl>, f_flag_match <chr>, f_time <dbl>, f_cut <fct>, f_Cz <dbl>,
      #   f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>, f_slope <dbl>, f_fit <dbl>,
      #   f_fit_slope <dbl>, f_start_z <dttm>

# renaming works

    Code
      flux_fitting(co2_conc_names, co2, date_time, f_start, finish, fit_type = "exponential")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
    Condition
      Warning:
      There were 6 warnings in `summarize()`.
      The first warning was:
      i In argument: `results = list(...)`.
      i In row 1.
      Caused by warning in `minpack.lm::nls.lm()`:
      ! lmdif: info = 0. Improper input parameters.
      i Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
    Message
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 1,251 x 23
         date_time           temp_air temp_soil   co2   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA    447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9  447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA    448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA    449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA    449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA    450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA    451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA    451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA    453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA    453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 16 more variables: f_start <dttm>, finish <dttm>, f_fluxid <fct>,
      #   f_ratio <dbl>, f_flag_match <chr>, f_time <dbl>, f_cut <fct>, f_Cz <dbl>,
      #   f_Cm <dbl>, f_a <dbl>, f_b <dbl>, f_tz <dbl>, f_slope <dbl>, f_fit <dbl>,
      #   f_fit_slope <dbl>, f_start_z <dttm>

