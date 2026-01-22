# trans_thornthwaite() works

    Code
      variable_trans(init(tenterfield), pet = trans_thornthwaite(tavg, lat = -29))
    Output
      [1] "Checking for missing values (`NA`): all the data must be complete. Input type is vector. Assuming the data are monthly time series starting in January, all regular (non-leap) years."
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_thornthwaite()` -> pet
    Output
      
      Data: 
      # A tibble: 369 x 10
         id                ym  prcp  tmax  tmin  tavg  long   lat name             pet
         <chr>          <mth> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>          <dbl>
       1 ASN00056032 1990 Jan   882  27.0 15.2  21.1   152. -29.0 tenterfield (~ 113. 
       2 ASN00056032 1990 Feb  1260  26.1 16.0  21.0   152. -29.0 tenterfield (~  97.0
       3 ASN00056032 1990 Mar   254  23.8 13.4  18.6   152. -29.0 tenterfield (~  83.9
       4 ASN00056032 1990 Apr  1594  20.4 12.5  16.5   152. -29.0 tenterfield (~  62.8
       5 ASN00056032 1990 May  1220  19.1  6.66 12.9   152. -29.0 tenterfield (~  42.1
       6 ASN00056032 1990 Jun   394  14.6  3.19  8.88  152. -29.0 tenterfield (~  22.5
       7 ASN00056032 1990 Jul   618  15.5  1.95  8.75  152. -29.0 tenterfield (~  23.1
       8 ASN00056032 1990 Aug   334  14.3  2.49  8.41  152. -29.0 tenterfield (~  23.1
       9 ASN00056032 1990 Sep   266  18.7  5.4  12.1   152. -29.0 tenterfield (~  41.3
      10 ASN00056032 1990 Oct   362  23.3  7.6  15.4   152. -29.0 tenterfield (~  66.1
      # i 359 more rows

---

    Code
      variable_trans(init(tenterfield), pet = trans_thornthwaite(tavg, lat = lat))
    Output
      [1] "Checking for missing values (`NA`): all the data must be complete. Input type is vector. Assuming the data are monthly time series starting in January, all regular (non-leap) years."
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_thornthwaite()` -> pet
    Output
      
      Data: 
      # A tibble: 369 x 10
         id                ym  prcp  tmax  tmin  tavg  long   lat name             pet
         <chr>          <mth> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>          <dbl>
       1 ASN00056032 1990 Jan   882  27.0 15.2  21.1   152. -29.0 tenterfield (~ 113. 
       2 ASN00056032 1990 Feb  1260  26.1 16.0  21.0   152. -29.0 tenterfield (~  97.0
       3 ASN00056032 1990 Mar   254  23.8 13.4  18.6   152. -29.0 tenterfield (~  83.9
       4 ASN00056032 1990 Apr  1594  20.4 12.5  16.5   152. -29.0 tenterfield (~  62.8
       5 ASN00056032 1990 May  1220  19.1  6.66 12.9   152. -29.0 tenterfield (~  42.1
       6 ASN00056032 1990 Jun   394  14.6  3.19  8.88  152. -29.0 tenterfield (~  22.5
       7 ASN00056032 1990 Jul   618  15.5  1.95  8.75  152. -29.0 tenterfield (~  23.1
       8 ASN00056032 1990 Aug   334  14.3  2.49  8.41  152. -29.0 tenterfield (~  23.1
       9 ASN00056032 1990 Sep   266  18.7  5.4  12.1   152. -29.0 tenterfield (~  41.2
      10 ASN00056032 1990 Oct   362  23.3  7.6  15.4   152. -29.0 tenterfield (~  66.1
      # i 359 more rows

# idx_spi() works

    Code
      res
    Output
      # A tibble: 358 x 13
         .dist id           ym  prcp  tmax   tmin  tavg  long   lat name  month .scale
         <chr> <chr>     <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>  <dbl>
       1 gamma ASN00~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~    12     12
       2 gamma ASN00~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~     1     12
       3 gamma ASN00~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~     2     12
       4 gamma ASN00~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~     3     12
       5 gamma ASN00~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~     4     12
       6 gamma ASN00~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~     5     12
       7 gamma ASN00~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~     6     12
       8 gamma ASN00~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~     7     12
       9 gamma ASN00~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~     8     12
      10 gamma ASN00~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~     9     12
      # i 348 more rows
      # i 1 more variable: .value <dbl>

---

    Code
      res
    Output
      # A tibble: 704 x 13
         .dist id           ym  prcp  tmax   tmin  tavg  long   lat name  month .scale
         <chr> <chr>     <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>  <dbl>
       1 gamma ASN00~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~    12     12
       2 gamma ASN00~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~     1     12
       3 gamma ASN00~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~     2     12
       4 gamma ASN00~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~     3     12
       5 gamma ASN00~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~     4     12
       6 gamma ASN00~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~     5     12
       7 gamma ASN00~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~     6     12
       8 gamma ASN00~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~     7     12
       9 gamma ASN00~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~     8     12
      10 gamma ASN00~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~     9     12
      # i 694 more rows
      # i 1 more variable: .value <dbl>

# idx_spei() works

    Code
      res
    Output
      # A tibble: 704 x 13
         .dist id           ym  prcp  tmax   tmin  tavg  long   lat name  month .scale
         <chr> <chr>     <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>  <dbl>
       1 glo   ASN00~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~    12     12
       2 glo   ASN00~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~     1     12
       3 glo   ASN00~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~     2     12
       4 glo   ASN00~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~     3     12
       5 glo   ASN00~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~     4     12
       6 glo   ASN00~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~     5     12
       7 glo   ASN00~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~     6     12
       8 glo   ASN00~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~     7     12
       9 glo   ASN00~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~     8     12
      10 glo   ASN00~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~     9     12
      # i 694 more rows
      # i 1 more variable: .value <dbl>

# idx_rdi() works

    Code
      res
    Output
      # A tibble: 358 x 12
         id                ym  prcp  tmax   tmin  tavg  long   lat name   month .scale
         <chr>          <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>  <dbl>  <dbl>
       1 ASN00056032 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tente~    12     12
       2 ASN00056032 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tente~     1     12
       3 ASN00056032 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tente~     2     12
       4 ASN00056032 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tente~     3     12
       5 ASN00056032 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tente~     4     12
       6 ASN00056032 1991 May   630  21.3  7.52  14.4   152. -29.0 tente~     5     12
       7 ASN00056032 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tente~     6     12
       8 ASN00056032 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tente~     7     12
       9 ASN00056032 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tente~     8     12
      10 ASN00056032 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tente~     9     12
      # i 348 more rows
      # i 1 more variable: .value <dbl>

# idx_edi() works

    Code
      res
    Output
      # A tibble: 358 x 12
         id                ym  prcp  tmax   tmin  tavg  long   lat name   month .scale
         <chr>          <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>  <dbl>  <dbl>
       1 ASN00056032 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tente~    12     12
       2 ASN00056032 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tente~     1     12
       3 ASN00056032 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tente~     2     12
       4 ASN00056032 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tente~     3     12
       5 ASN00056032 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tente~     4     12
       6 ASN00056032 1991 May   630  21.3  7.52  14.4   152. -29.0 tente~     5     12
       7 ASN00056032 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tente~     6     12
       8 ASN00056032 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tente~     7     12
       9 ASN00056032 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tente~     8     12
      10 ASN00056032 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tente~     9     12
      # i 348 more rows
      # i 1 more variable: .value <dbl>

