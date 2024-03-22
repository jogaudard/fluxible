# averaging works

    Code
      output
    Output
      # A tibble: 6 x 6
        f_fluxID   PAR temp_soil f_slope temp_air_ave  flux
           <dbl> <dbl>     <dbl>   <dbl>        <dbl> <dbl>
      1        1  1.95      10.8   1.56          7.31  95.6
      2        2  2.11      10.7   0.853         7.38  52.4
      3        3  2.04      10.7   0.303         7.46  18.6
      4        4  1.84      10.8   1.13          7.77  69.4
      5        5  1.66      10.6   1.46          7.71  89.9
      6        6  1.78      12.2   0.426         7.75  26.2

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_fahr",
        temp_air_unit = "fahrenheit")
    Output
      # A tibble: 6 x 4
        f_fluxID f_slope temp_air_ave  flux
           <dbl>   <dbl>        <dbl> <dbl>
      1        1   1.56          45.2  95.6
      2        2   0.853         45.3  52.4
      3        3   0.303         45.4  18.6
      4        4   1.13          46.0  69.4
      5        5   1.46          45.9  89.9
      6        6   0.426         45.9  26.2

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_kelvin",
        temp_air_unit = "kelvin")
    Output
      # A tibble: 6 x 4
        f_fluxID f_slope temp_air_ave  flux
           <dbl>   <dbl>        <dbl> <dbl>
      1        1   1.56          280.  95.6
      2        2   0.853         281.  52.4
      3        3   0.303         281.  18.6
      4        4   1.13          281.  69.4
      5        5   1.46          281.  89.9
      6        6   0.426         281.  26.2

