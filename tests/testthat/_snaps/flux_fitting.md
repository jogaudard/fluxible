# works for exponential fitting

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "exp_zhao18"),
      f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          1.56 
      2 2          0.853
      3 3          0.303
      4 4          1.13 
      5 5          1.46 
      6 6          0.426

# works for linear fitting

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "lin"),
      f_fluxid, f_slope))
    Condition
      Warning in `flux_fitting_lin()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1         0.113 
      2 2         0.110 
      3 3         0.115 
      4 4         0.0431
      5 5        -0.105 
      6 6         0.117 

# works for quadratic fitting

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "qua"),
      f_fluxid, f_slope))
    Condition
      Warning in `flux_fitting_quadratic()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          1.60 
      2 2          0.978
      3 3          0.251
      4 4          1.24 
      5 5          0.876
      6 6          0.474

# works for exponential fitting with cut

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "exp_zhao18",
        start_cut = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 5 : slope was estimated on 185 points out of 190 seconds
       fluxID 6 : slope was estimated on 186 points out of 190 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          1.46 
      2 2          1.01 
      3 3          0.241
      4 4          1.32 
      5 5          1.08 
      6 6          0.337

# works for linear fitting with cut

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "line",
        start_cut = 20), f_fluxid, f_slope))
    Condition
      Warning in `flux_fitting_lin()`:
      
       fluxID 5 : slope was estimated on 185 points out of 190 seconds
       fluxID 6 : slope was estimated on 186 points out of 190 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1         0.0214
      2 2         0.0596
      3 3         0.0978
      4 4        -0.0327
      5 5        -0.195 
      6 6         0.0877

# removing duplicated datetime

    Code
      flux_fitting(rep_data, conc, datetime, fit_type = "exponential")
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
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

# works for exp_tz fitting

    Code
      distinct(select(flux_fitting(co2_conc, conc, datetime, fit_type = "exp_tz"),
      f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exptz()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid  f_slope
        <fct>       <dbl>
      1 1        2253.   
      2 2         966.   
      3 3           0.376
      4 4        1613.   
      5 5           1.18 
      6 6           0.493

# works for exp_zhao18 with missing data

    Code
      distinct(select(flux_fitting(co2_conc_missing, conc, datetime, fit_type = "exp_zhao18",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 1 : slope was estimated on 28 points out of 150 seconds
       fluxID 2 : slope was estimated on 61 points out of 150 seconds
       fluxID 3 : slope was estimated on 42 points out of 150 seconds
       fluxID 6 dropped (no data in the conc column)
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          0.376
      2 2          0.462
      3 3         -6.33 
      4 4          0.686
      5 5          0.751
      6 6         NA    

# works for exp_zhao18 with mid missing data

    Code
      distinct(select(flux_fitting(co2_conc_mid_missing, conc, datetime, fit_type = "exp_zhao18",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          0.841
      2 2          0.579
      3 3          0.472
      4 4          0.620
      5 5          0.751
      6 6          0.475

# works for exp_tz with mid missing data

    Code
      distinct(select(flux_fitting(co2_conc_mid_missing, conc, datetime, fit_type = "exp_tz",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exptz()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          0.555
      2 2          0.387
      3 3          0.280
      4 4          0.498
      5 5          0.579
      6 6          0.198

# works for quadratic with mid missing data

    Code
      distinct(select(flux_fitting(co2_conc_mid_missing, conc, datetime, fit_type = "quadratic",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Condition
      Warning in `flux_fitting_quadratic()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          0.645
      2 2          0.394
      3 3          0.258
      4 4          0.525
      5 5          0.672
      6 6          0.314

# exp_tz: optim produces non-finite values

    Code
      distinct(select(flux_fitting(test_data, conc, datetime, fit_type = "exp_tz",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exptz()`:
      
       fluxID 1 : slope was estimated on 28 points out of 150 seconds
       fluxID 2 : slope was estimated on 61 points out of 150 seconds
       fluxID 3 : slope was estimated on 42 points out of 150 seconds
       fluxID 4 : slope is NA, most likely optim() supplied non-finite value.
              Check your data or use a different model.
       fluxID 6 dropped (no data in the conc column)
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1         0.367 
      2 2         0.317 
      3 3         0.0834
      4 4        NA     
      5 5         0.579 
      6 6        NA     

# exp_zhao18: optim produces non-finite values

    Code
      distinct(select(flux_fitting(test_data, conc, datetime, fit_type = "exp_zhao18",
        end_cut = 60, t_zero = 20), f_fluxid, f_slope))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 1 : slope was estimated on 28 points out of 150 seconds
       fluxID 2 : slope was estimated on 61 points out of 150 seconds
       fluxID 3 : slope was estimated on 42 points out of 150 seconds
       fluxID 4 : slope is NA, most likely optim() supplied non-finite value.
              Check your data or use a different model.
       fluxID 6 dropped (no data in the conc column)
    Output
      # A tibble: 6 x 2
        f_fluxid f_slope
        <fct>      <dbl>
      1 1          0.376
      2 2          0.462
      3 3         -6.33 
      4 4         NA    
      5 5          0.751
      6 6         NA    

