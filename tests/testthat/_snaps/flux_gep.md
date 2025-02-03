# GEP calculation

    Code
      flux_gep(co2_fluxes, flux, type, f_start, PAR, id_cols = "turfID", cols_keep = c(
        "temp_soil"))
    Condition
      Warning in `flux_gep()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 11
        f_start               PAR type   flux f_fluxID f_slope_tz temp_air_ave
        <dttm>              <dbl> <chr> <dbl> <fct>         <dbl>        <dbl>
      1 2022-07-28 23:47:22  2.11 GEP    33.8 <NA>         NA            NA   
      2 2022-07-28 23:59:32  1.84 GEP   -20.5 <NA>         NA            NA   
      3 2022-07-29 00:06:35  1.78 GEP    NA   <NA>         NA            NA   
      4 2022-07-28 23:43:35  1.95 ER     95.6 1             1.56          7.31
      5 2022-07-28 23:47:22  2.11 NEE    52.4 2             0.853         7.38
      6 2022-07-28 23:52:10  2.04 ER     18.6 3             0.303         7.46
      7 2022-07-28 23:59:32  1.84 NEE    69.4 4             1.13          7.77
      8 2022-07-29 00:03:10  1.66 ER     89.9 5             1.46          7.71
      9 2022-07-29 00:06:35  1.78 NEE    26.2 6             0.426         7.75
      # i 4 more variables: temp_soil <dbl>, turfID <fct>, temp_fahr <dbl>,
      #   temp_kelvin <dbl>

# GEP calculation works with several id cols

    Code
      flux_gep(fluxes, flux, type, datetime, par, id_cols = c("turfid", "campaign"))
    Output
      # A tibble: 9 x 6
        datetime              par type   flux campaign turfid
        <chr>               <dbl> <chr> <dbl>    <dbl> <chr> 
      1 2024-02-11 10:00:00   300 GEP      -2        1 A     
      2 2024-02-11 10:00:20   250 GEP      -5        2 A     
      3 2024-02-11 10:00:40   320 GEP      -2        3 B     
      4 2024-02-11 10:00:00   300 NEE       3        1 A     
      5 2024-02-11 10:00:10     2 ER        5        1 A     
      6 2024-02-11 10:00:20   250 NEE       2        2 A     
      7 2024-02-11 10:00:30     5 ER        7        2 A     
      8 2024-02-11 10:00:40   320 NEE       9        3 B     
      9 2024-02-11 10:00:50     1 ER       11        3 B     

# missing NEE and several id cols

    Code
      flux_gep(fluxes, flux, type, datetime, par, id_cols = c("turfid", "campaign"))
    Condition
      Warning in `flux_gep()`:
      
       NEE missing for measurement turfid: C, campaign: 3
       NEE missing for measurement turfid: D, campaign: 4
       NEE missing for measurement turfid: A, campaign: 4
    Output
      # A tibble: 12 x 6
         datetime              par type   flux campaign turfid
         <chr>               <dbl> <chr> <dbl>    <dbl> <chr> 
       1 2024-02-11 10:00:00   300 GEP      -2        1 A     
       2 2024-02-11 10:00:20   250 GEP      -5        2 A     
       3 2024-02-11 10:00:40   320 GEP      -2        3 B     
       4 2024-02-11 10:00:00   300 NEE       3        1 A     
       5 2024-02-11 10:00:10     2 ER        5        1 A     
       6 2024-02-11 10:00:20   250 NEE       2        2 A     
       7 2024-02-11 10:00:30     5 ER        7        2 A     
       8 2024-02-11 10:00:40   320 NEE       9        3 B     
       9 2024-02-11 10:00:50     1 ER       11        3 B     
      10 2024-02-11 10:01:00     0 ER       10        3 C     
      11 2024-02-11 10:01:10     3 ER       13        4 D     
      12 2024-02-11 10:01:20     4 ER        8        4 A     

