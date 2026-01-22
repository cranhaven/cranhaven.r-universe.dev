# temporal_agg() works

    Code
      temporal_aggregate(init(tenterfield, id = id, time = ym), .agg = temporal_rolling_window(
        prcp, scale = 12))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      temporal: `rolling_window()` -> .agg
    Output
      
      Data: 
      # A tibble: 358 x 10
         id                ym  prcp  tmax   tmin  tavg  long   lat name           .agg
         <chr>          <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>         <dbl>
       1 ASN00056032 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tenterfield ~  8382
       2 ASN00056032 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tenterfield ~  8608
       3 ASN00056032 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tenterfield ~  7976
       4 ASN00056032 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tenterfield ~  7926
       5 ASN00056032 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tenterfield ~  6376
       6 ASN00056032 1991 May   630  21.3  7.52  14.4   152. -29.0 tenterfield ~  5786
       7 ASN00056032 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tenterfield ~  5634
       8 ASN00056032 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tenterfield ~  5596
       9 ASN00056032 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tenterfield ~  5276
      10 ASN00056032 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tenterfield ~  5088
      # i 348 more rows

---

    Code
      temporal_aggregate(init(tenterfield, id = id, time = ym),
      temporal_rolling_window(prcp, scale = c(12, 24)))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      temporal: `rolling_window()` -> rolling_window_12
      temporal: `rolling_window()` -> rolling_window_24
    Output
      
      Data: 
      # A tibble: 358 x 11
         id            ym  prcp  tmax   tmin  tavg  long   lat name  rolling_window_12
         <chr>      <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>             <dbl>
       1 ASN000~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~              8382
       2 ASN000~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~              8608
       3 ASN000~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~              7976
       4 ASN000~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~              7926
       5 ASN000~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~              6376
       6 ASN000~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~              5786
       7 ASN000~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~              5634
       8 ASN000~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~              5596
       9 ASN000~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~              5276
      10 ASN000~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~              5088
      # i 348 more rows
      # i 1 more variable: rolling_window_24 <dbl>

# check on temporal order and id

    Code
      temporal_aggregate(init(tenterfield, time = ym), .agg = temporal_rolling_window(
        prcp, scale = 12))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      temporal: `rolling_window()` -> .agg
    Output
      
      Data: 
      # A tibble: 358 x 10
         id                ym  prcp  tmax   tmin  tavg  long   lat name           .agg
         <chr>          <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>         <dbl>
       1 ASN00056032 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tenterfield ~  8382
       2 ASN00056032 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tenterfield ~  8608
       3 ASN00056032 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tenterfield ~  7976
       4 ASN00056032 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tenterfield ~  7926
       5 ASN00056032 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tenterfield ~  6376
       6 ASN00056032 1991 May   630  21.3  7.52  14.4   152. -29.0 tenterfield ~  5786
       7 ASN00056032 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tenterfield ~  5634
       8 ASN00056032 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tenterfield ~  5596
       9 ASN00056032 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tenterfield ~  5276
      10 ASN00056032 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tenterfield ~  5088
      # i 348 more rows

---

    Code
      temporal_aggregate(init(tenterfield, id = id), .agg = temporal_rolling_window(
        prcp, scale = 12))
    Condition
      Error in `check_temporal_index()`:
      ! No temporal index is found in the input data. Please supply through `init()`

---

    Code
      temporal_aggregate(init(dplyr::slice_sample(tenterfield, n = 369), id = id,
      time = ym), .agg = temporal_rolling_window(prcp, scale = 12))
    Condition
      Error in `check_temporal_index()`:
      ! The temporal index is not ordered. Please check the input data.

# on errors

    Code
      temporal_aggregate(tenterfield, temporal_rolling_window(prcp, scale = 12))
    Condition
      Error in `check_idx_tbl()`:
      ! A index table object is required as input. Created it using `init()`.

---

    Code
      temporal_aggregate(init(tenterfield), index = rescale_zscore(prcp))
    Condition
      Error in `FUN()`:
      ! A temporal aggregation object is required as input. Create it using `temporal_*()`

