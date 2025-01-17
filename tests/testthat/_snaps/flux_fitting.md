# works for exponential fitting

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "expo"), f_fluxID, f_slope))
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

# works for linear fitting

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "lin"), f_fluxID, f_slope))
    Condition
      Warning in `flux_fitting_lin()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxID f_slope
        <fct>      <dbl>
      1 1         0.113 
      2 2         0.110 
      3 3         0.115 
      4 4         0.0431
      5 5        -0.105 
      6 6         0.117 

# works for quadratic fitting

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "qua"), f_fluxID, f_slope))
    Condition
      Warning in `flux_fitting_quadratic()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds because data are missing
       fluxID 6 : slope was estimated on 206 points out of 210 seconds because data are missing
    Output
      # A tibble: 6 x 2
        f_fluxID f_slope
        <fct>      <dbl>
      1 1          1.60 
      2 2          0.978
      3 3          0.251
      4 4          1.24 
      5 5          0.876
      6 6          0.474

# works for exponential fitting with cut

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "expo", start_cut = 20),
      f_fluxID, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exp()`:
      
       fluxID 5 : slope was estimated on 185 points out of 190 seconds
       fluxID 6 : slope was estimated on 186 points out of 190 seconds
    Output
      # A tibble: 6 x 2
        f_fluxID f_slope
        <fct>      <dbl>
      1 1          1.46 
      2 2          1.01 
      3 3          0.241
      4 4          1.32 
      5 5          1.08 
      6 6          0.337

# works for linear fitting with cut

    Code
      distinct(select(flux_fitting(co2_conc, fit_type = "line", start_cut = 20),
      f_fluxID, f_slope))
    Condition
      Warning in `flux_fitting_lin()`:
      
       fluxID 5 : slope was estimated on 185 points out of 190 seconds
       fluxID 6 : slope was estimated on 186 points out of 190 seconds
    Output
      # A tibble: 6 x 2
        f_fluxID f_slope
        <fct>      <dbl>
      1 1         0.0214
      2 2         0.0596
      3 3         0.0978
      4 4        -0.0327
      5 5        -0.195 
      6 6         0.0877

# removing duplicated datetime

    Code
      flux_fitting(rep_data, fit_type = "exp")
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

# segmentation tool snapshot

    Code
      flux_fitting(conc_df = pftc7_short, fit_type = "segments", start_col = "start_time",
        end_col = "f_end", start_cut = 0, end_cut = 0, conc_col = "co2_conc",
        par_col = "par", datetime_col = "date_time", h2o_col = "h2o_conc",
        signal_strength_col = "signal_strength", fluxid_col = "file_name",
        h2o_correction = TRUE, min_seg_length = 30)
    Message
      Cutting measurements...
      Starting segmentation...
      
      Segmenting flux 1 out of 13 [==>--------------------------------------] (  8%)
      
      Segmenting flux 2 out of 13 [=====>-----------------------------------] ( 15%)
      
      Segmenting flux 3 out of 13 [========>--------------------------------] ( 23%)
      
      Segmenting flux 4 out of 13 [============>----------------------------] ( 31%)
      
      Segmenting flux 5 out of 13 [===============>-------------------------] ( 38%)
      
      Segmenting flux 6 out of 13 [==================>----------------------] ( 46%)
      
      Segmenting flux 7 out of 13 [=====================>-------------------] ( 54%)
      
      Segmenting flux 8 out of 13 [========================>----------------] ( 62%)
      
      Segmenting flux 9 out of 13 [===========================>-------------] ( 69%)
      
      Segmenting flux 10 out of 13 [==============================>---------] ( 77%)
      
      Segmenting flux 11 out of 13 [=================================>------] ( 85%)
      
      Segmenting flux 12 out of 13 [====================================>---] ( 92%)
      
      Segmenting flux 13 out of 13 [========================================] (100%)
                                                                                    
      
    Output
      # A tibble: 1,665 x 33
         f_conc h2o_conc temperature_c pressure_kpa signal_strength
          <dbl>    <dbl>         <dbl>        <dbl>           <dbl>
       1   426.     14.3          18.7         73.1            99.2
       2   426.     14.5          18.8         73.1            99.3
       3   426.     14.7          18.8         73.1            99.2
       4   426.     14.8          18.8         73.1            99.2
       5   426.     14.9          18.9         73.1            99.2
       6   426.     14.9          18.9         73.1            99.2
       7   426.     15.1          18.9         73.1            99.2
       8   426.     15.2          19.0         73.1            99.2
       9   426.     15.3          19.0         73.1            99.2
      10   426.     15.4          19.0         73.1            99.2
      # i 1,655 more rows
      # i 28 more variables: f_datetime <dttm>, f_start <dttm>, f_fluxID <fct>,
      #   site <dbl>, elevation <dbl>, aspect <chr>, plot <dbl>, day_night <chr>,
      #   measurement <chr>, redo <lgl>, plot_id <chr>, par <dbl>, f_end <dttm>,
      #   f_time <dbl>, n_conc <int>, f_flag_fit <chr>, f_cut <fct>,
      #   corrected_for_water_vapor <chr>, f_time_cut <dbl>, f_fit <dbl>,
      #   f_slope <dbl>, f_rsquared <dbl>, f_adj_rsquared <dbl>, f_pvalue <dbl>, ...

