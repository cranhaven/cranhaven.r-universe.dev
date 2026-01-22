# distribution_fit() works

    Code
      normalise(dt, index = norm_quantile(.fit))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      temporal: `rolling_window()` -> .agg
      distribution_fit: `distfit_gamma()` -> .fit
      normalise: `norm_quantile()` -> index
    Output
      
      Data: 
      # A tibble: 358 x 14
         id     month       ym  prcp  tmax   tmin  tavg  long   lat name   .agg   .fit
         <chr>  <dbl>    <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>  <dbl>
       1 ASN00~    12 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~  8382 0.700 
       2 ASN00~     1 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~  8608 0.724 
       3 ASN00~     2 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~  7976 0.608 
       4 ASN00~     3 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~  7926 0.595 
       5 ASN00~     4 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~  6376 0.258 
       6 ASN00~     5 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~  5786 0.178 
       7 ASN00~     6 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~  5634 0.152 
       8 ASN00~     7 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~  5596 0.139 
       9 ASN00~     8 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~  5276 0.0967
      10 ASN00~     9 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~  5088 0.0837
      # i 348 more rows
      # i 2 more variables: .fit_obj <list>, index <dbl>

# on errors

    Code
      distribution_fit(tenterifeld, .fit = dist_gamma(.agg, method = "lmoms"))
    Condition
      Error:
      ! object 'tenterifeld' not found

---

    Code
      distribution_fit(init(hdi), index = rescale_zscore(life_exp))
    Condition
      Error in `check_dist_fit_obj()`:
      ! A distribution fit object is required as input. Create it using `dist_*()`

