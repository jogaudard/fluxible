# GEP calculation

    Code
      flux_gep(co2_fluxes, type, f_start, PAR, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil"))
    Condition
      Warning in `flux_gep()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 6
        f_start               PAR type  f_flux temp_soil turfID      
        <dttm>              <dbl> <chr>  <dbl>     <dbl> <fct>       
      1 2022-07-28 23:47:22  2.11 GEP     33.8      10.7 74 WN2C 155 
      2 2022-07-28 23:59:32  1.84 GEP    -20.5      10.8 109 AN3C 109
      3 2022-07-29 00:06:35  1.78 GEP     NA        12.2 29 WN3C 106 
      4 2022-07-28 23:43:35  1.95 ER      95.6      10.8 156 AN2C 156
      5 2022-07-28 23:47:22  2.11 NEE     52.4      10.7 74 WN2C 155 
      6 2022-07-28 23:52:10  2.04 ER      18.6      10.7 74 WN2C 155 
      7 2022-07-28 23:59:32  1.84 NEE     69.4      10.8 109 AN3C 109
      8 2022-07-29 00:03:10  1.66 ER      89.9      10.6 109 AN3C 109
      9 2022-07-29 00:06:35  1.78 NEE     26.2      12.2 29 WN3C 106 

# GEP calculation works with several id cols

    Code
      flux_gep(fluxes, type, datetime, par, flux, id_cols = c("turfid", "campaign"))
    Output
      # A tibble: 9 x 6
        datetime              par type   flux turfid campaign
        <chr>               <dbl> <chr> <dbl> <chr>     <dbl>
      1 2024-02-11 10:00:00   300 GEP      -2 A             1
      2 2024-02-11 10:00:20   250 GEP      -5 A             2
      3 2024-02-11 10:00:40   320 GEP      -2 B             3
      4 2024-02-11 10:00:00   300 NEE       3 A             1
      5 2024-02-11 10:00:10     2 ER        5 A             1
      6 2024-02-11 10:00:20   250 NEE       2 A             2
      7 2024-02-11 10:00:30     5 ER        7 A             2
      8 2024-02-11 10:00:40   320 NEE       9 B             3
      9 2024-02-11 10:00:50     1 ER       11 B             3

# missing NEE and several id cols

    Code
      flux_gep(fluxes, type, datetime, par, flux, id_cols = c("turfid", "campaign"))
    Condition
      Warning in `flux_gep()`:
      
       NEE missing for measurement turfid: C, campaign: 3
       NEE missing for measurement turfid: D, campaign: 4
       NEE missing for measurement turfid: A, campaign: 4
    Output
      # A tibble: 12 x 6
         datetime              par type   flux turfid campaign
         <chr>               <dbl> <chr> <dbl> <chr>     <dbl>
       1 2024-02-11 10:00:00   300 GEP      -2 A             1
       2 2024-02-11 10:00:20   250 GEP      -5 A             2
       3 2024-02-11 10:00:40   320 GEP      -2 B             3
       4 2024-02-11 10:00:00   300 NEE       3 A             1
       5 2024-02-11 10:00:10     2 ER        5 A             1
       6 2024-02-11 10:00:20   250 NEE       2 A             2
       7 2024-02-11 10:00:30     5 ER        7 A             2
       8 2024-02-11 10:00:40   320 NEE       9 B             3
       9 2024-02-11 10:00:50     1 ER       11 B             3
      10 2024-02-11 10:01:00     0 ER       10 C             3
      11 2024-02-11 10:01:10     3 ER       13 D             4
      12 2024-02-11 10:01:20     4 ER        8 A             4

