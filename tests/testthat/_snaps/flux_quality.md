# works for exponential fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes0, f_conc, fit_type = "expo"),
      f_fluxID, f_quality_flag, f_RMSE, f_cor_coef))
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
      # A tibble: 6 x 4
        f_fluxID f_quality_flag f_RMSE f_cor_coef
           <dbl> <chr>           <dbl>      <dbl>
      1        1 ok              23.5       0.211
      2        2 ok              14.0       0.336
      3        3 ok               2.51      0.949
      4        4 ok              15.0       0.112
      5        5 ok              12.0      -0.315
      6        6 ok               6.19      0.640

# works for linear fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30lin, f_conc, fit_type = "li"),
      f_fluxID, f_quality_flag, f_pvalue, f_rsquared))
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
      # A tibble: 6 x 4
        f_fluxID f_quality_flag  f_pvalue f_rsquared
        <fct>    <chr>              <dbl>      <dbl>
      1 1        ok             1.23e-166    0.986  
      2 2        ok             1.43e-207    0.995  
      3 3        ok             3.06e- 96    0.913  
      4 4        ok             1.04e-108    0.937  
      5 5        discard        2.84e-  1    0.00646
      6 6        ok             1.39e-114    0.946  

# works for quadratic fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30qua, f_conc, fit_type = "quadratic"),
      f_fluxID, f_quality_flag, f_pvalue, f_rsquared))
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
      # A tibble: 6 x 4
        f_fluxID f_quality_flag  f_pvalue f_rsquared
        <fct>    <chr>              <dbl>      <dbl>
      1 1        ok             9.51e-297      1.00 
      2 2        ok             1.08e-292      0.999
      3 3        ok             2.44e-173      0.989
      4 4        ok             8.52e-217      0.996
      5 5        ok             9.68e- 55      0.755
      6 6        ok             5.13e-191      0.993

