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
      dplyr::distinct(dplyr::select(flux_fitting(conc_df = pftc7_short, fit_type = "segments",
        start_col = "start_time", end_col = "f_end", start_cut = 0, end_cut = 0,
        conc_col = "co2_conc", par_col = "par", datetime_col = "date_time", h2o_col = "h2o_conc",
        signal_strength_col = "signal_strength", fluxid_col = "file_name",
        h2o_correction = TRUE, min_seg_length = 30), f_fluxID, f_slope, f_rsquared,
      f_adj_rsquared, f_pvalue, f_segment_length))
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
      # A tibble: 51 x 6
         f_fluxID         f_slope f_rsquared f_adj_rsquared  f_pvalue f_segment_length
         <fct>              <dbl>      <dbl>          <dbl>     <dbl>            <dbl>
       1 5_2800_east_5_d~  0.0767     0.688          0.677   1.49e- 8               30
       2 5_2800_east_5_d~  0.186      0.988          0.988   3.28e-46               48
       3 5_2800_east_5_d~ -0.0863     0.960          0.959   9.07e-32               45
       4 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       5 5_2800_east_5_d~ -0.0882     0.973          0.973   2.78e-32               41
       6 5_2800_east_5_d~  0.0548     0.619          0.606   1.55e- 7               31
       7 5_2800_east_5_d~ -0.0158     0.520          0.510   3.43e- 9               50
       8 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       9 5_2800_east_4_d~ -0.0367     0.0774         0.0708  8.34e- 4              141
      10 5_2800_east_4_d~  1.99       0.0806         0.0551  8.40e- 2               38
      # i 41 more rows

# fitting segment works without par

    Code
      dplyr::distinct(dplyr::select(flux_fitting(conc_df = test_data, fit_type = "segments",
        start_col = "start_time", end_col = "f_end", start_cut = 0, end_cut = 0,
        conc_col = "co2_conc", datetime_col = "date_time", h2o_col = "h2o_conc",
        signal_strength_col = "signal_strength", fluxid_col = "file_name",
        h2o_correction = TRUE, min_seg_length = 30), f_fluxID, f_slope, f_rsquared,
      f_adj_rsquared, f_pvalue, f_segment_length))
    Message
      f_par column added
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
      # A tibble: 51 x 6
         f_fluxID         f_slope f_rsquared f_adj_rsquared  f_pvalue f_segment_length
         <fct>              <dbl>      <dbl>          <dbl>     <dbl>            <dbl>
       1 5_2800_east_5_d~  0.0767     0.688          0.677   1.49e- 8               30
       2 5_2800_east_5_d~  0.186      0.988          0.988   3.28e-46               48
       3 5_2800_east_5_d~ -0.0863     0.960          0.959   9.07e-32               45
       4 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       5 5_2800_east_5_d~ -0.0882     0.973          0.973   2.78e-32               41
       6 5_2800_east_5_d~  0.0548     0.619          0.606   1.55e- 7               31
       7 5_2800_east_5_d~ -0.0158     0.520          0.510   3.43e- 9               50
       8 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       9 5_2800_east_4_d~ -0.0367     0.0774         0.0708  8.34e- 4              141
      10 5_2800_east_4_d~  1.99       0.0806         0.0551  8.40e- 2               38
      # i 41 more rows

# fitting segment works without signal strength

    Code
      dplyr::distinct(dplyr::select(flux_fitting(conc_df = test_data, fit_type = "segments",
        start_col = "start_time", end_col = "f_end", start_cut = 0, end_cut = 0,
        conc_col = "co2_conc", par_col = "par", datetime_col = "date_time", h2o_col = "h2o_conc",
        fluxid_col = "file_name", h2o_correction = TRUE, min_seg_length = 30),
      f_fluxID, f_slope, f_rsquared, f_adj_rsquared, f_pvalue, f_segment_length))
    Message
      f_signal_strength column added
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
      # A tibble: 51 x 6
         f_fluxID         f_slope f_rsquared f_adj_rsquared  f_pvalue f_segment_length
         <fct>              <dbl>      <dbl>          <dbl>     <dbl>            <dbl>
       1 5_2800_east_5_d~  0.0767     0.688          0.677   1.49e- 8               30
       2 5_2800_east_5_d~  0.186      0.988          0.988   3.28e-46               48
       3 5_2800_east_5_d~ -0.0863     0.960          0.959   9.07e-32               45
       4 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       5 5_2800_east_5_d~ -0.0882     0.973          0.973   2.78e-32               41
       6 5_2800_east_5_d~  0.0548     0.619          0.606   1.55e- 7               31
       7 5_2800_east_5_d~ -0.0158     0.520          0.510   3.43e- 9               50
       8 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       9 5_2800_east_4_d~ -0.0367     0.0774         0.0708  8.34e- 4              141
      10 5_2800_east_4_d~  1.99       0.0806         0.0551  8.40e- 2               38
      # i 41 more rows

# fitting segment works without h2o concentration

    Code
      dplyr::distinct(dplyr::select(flux_fitting(conc_df = test_data, fit_type = "segments",
        start_col = "start_time", end_col = "f_end", start_cut = 0, end_cut = 0,
        conc_col = "co2_conc", par_col = "par", datetime_col = "date_time",
        signal_strength_col = "signal_strength", fluxid_col = "file_name",
        h2o_correction = FALSE, min_seg_length = 30), f_fluxID, f_slope, f_rsquared,
      f_adj_rsquared, f_pvalue, f_segment_length))
    Message
      f_h2o_conc column added
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
      # A tibble: 52 x 6
         f_fluxID         f_slope f_rsquared f_adj_rsquared  f_pvalue f_segment_length
         <fct>              <dbl>      <dbl>          <dbl>     <dbl>            <dbl>
       1 5_2800_east_5_d~  0.0325     0.272          0.246   3.11e- 3               30
       2 5_2800_east_5_d~  0.160      0.988          0.988   1.16e-45               48
       3 5_2800_east_5_d~ -0.105      0.972          0.972   4.11e-35               45
       4 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       5 5_2800_east_5_d~ -0.129      0.986          0.985   1.18e-37               41
       6 5_2800_east_5_d~  0.0237     0.237          0.211   5.44e- 3               31
       7 5_2800_east_5_d~ -0.0417     0.885          0.882   3.62e-24               50
       8 5_2800_east_5_d~ NA         NA             NA      NA                      NA
       9 5_2800_east_4_d~ -0.0448     0.111          0.105   6.12e- 5              139
      10 5_2800_east_4_d~  1.93       0.0900         0.0660  6.00e- 2               40
      # i 42 more rows

