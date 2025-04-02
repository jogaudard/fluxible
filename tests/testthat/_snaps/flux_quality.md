# works for exponential fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes0, conc), f_fluxid,
      f_quality_flag, f_RMSE, f_cor_coef, f_ratio, f_gfactor))
    Message
      
       Total number of measurements: 6
      
       discard 	 3 	 50 %
       ok 	 3 	 50 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
    Output
      # A tibble: 6 x 6
        f_fluxid f_quality_flag f_RMSE f_cor_coef f_ratio f_gfactor
        <fct>    <chr>           <dbl>      <dbl>   <dbl>     <dbl>
      1 1        discard         23.5       0.211   1         13.8 
      2 2        ok              14.0       0.336   1          7.74
      3 3        ok               2.51      0.949   1          2.63
      4 4        discard         15.0       0.112   1         26.2 
      5 5        discard         12.0      -0.315   0.976    -14.0 
      6 6        ok               6.19      0.640   0.981      3.65

# works for linear fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30lin, conc), f_fluxid,
      f_quality_flag, f_pvalue, f_rsquared))
    Message
      
       Total number of measurements: 6
      
       ok 	 5 	 83 %
       discard 	 1 	 17 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
    Output
      # A tibble: 6 x 4
        f_fluxid f_quality_flag  f_pvalue f_rsquared
        <fct>    <chr>              <dbl>      <dbl>
      1 1        ok             1.23e-166    0.986  
      2 2        ok             1.43e-207    0.995  
      3 3        ok             3.06e- 96    0.913  
      4 4        ok             1.04e-108    0.937  
      5 5        discard        2.84e-  1    0.00646
      6 6        ok             1.39e-114    0.946  

# works for quadratic fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30qua, conc), f_fluxid,
      f_quality_flag, f_pvalue, f_rsquared, f_gfactor))
    Message
      
       Total number of measurements: 6
      
       ok 	 5 	 83 %
       discard 	 1 	 17 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
    Output
      # A tibble: 6 x 5
        f_fluxid f_quality_flag  f_pvalue f_rsquared f_gfactor
        <fct>    <chr>              <dbl>      <dbl>     <dbl>
      1 1        ok             9.51e-297      1.00       1.45
      2 2        ok             1.08e-292      0.999      1.26
      3 3        ok             2.44e-173      0.989      2.11
      4 4        ok             8.52e-217      0.996      1.97
      5 5        discard        9.68e- 55      0.755    -40.5 
      6 6        ok             5.13e-191      0.993      1.86

# kappamax with HM model

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopeshm, conc, kappamax = TRUE),
      f_fluxid, f_quality_flag, f_slope_corr, f_model))
    Message
      
       Total number of measurements: 1
      
       ok 	 1 	 100 %
       discard 	 0 	 0 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
    Output
      # A tibble: 2 x 4
        f_fluxid f_quality_flag f_slope_corr f_model
        <fct>    <chr>                 <dbl> <chr>  
      1 2        ok                    0.409 exp_hm 
      2 2        discard              NA     exp_hm 

