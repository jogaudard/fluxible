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
      1 1         1.68  
      2 2         0.226 
      3 3         0.156 
      4 4         0.183 
      5 5         0.0178
      6 6         0.216 

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

