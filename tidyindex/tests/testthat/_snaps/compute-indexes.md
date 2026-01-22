# compute_indexes() works

    Code
      res
    Output
      # A tibble: 1,074 x 14
         .idx  .dist id            ym  prcp  tmax   tmin  tavg  long   lat name  month
         <chr> <chr> <chr>      <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>
       1 spi   gamma ASN000~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~    12
       2 spi   gamma ASN000~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~     1
       3 spi   gamma ASN000~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~     2
       4 spi   gamma ASN000~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~     3
       5 spi   gamma ASN000~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~     4
       6 spi   gamma ASN000~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~     5
       7 spi   gamma ASN000~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~     6
       8 spi   gamma ASN000~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~     7
       9 spi   gamma ASN000~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~     8
      10 spi   gamma ASN000~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~     9
      # i 1,064 more rows
      # i 2 more variables: .scale <dbl>, .value <dbl>

---

    Code
      res2
    Output
      # A tibble: 1,420 x 14
         .idx  .dist id            ym  prcp  tmax   tmin  tavg  long   lat name  month
         <chr> <chr> <chr>      <mth> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>
       1 spi   gamma ASN000~ 1990 Dec   640  30.4 14.7   22.6   152. -29.0 tent~    12
       2 spi   gamma ASN000~ 1991 Jan  1108  27.5 15.9   21.7   152. -29.0 tent~     1
       3 spi   gamma ASN000~ 1991 Feb   628  28.0 15.5   21.8   152. -29.0 tent~     2
       4 spi   gamma ASN000~ 1991 Mar   204  26.2 11.8   19.0   152. -29.0 tent~     3
       5 spi   gamma ASN000~ 1991 Apr    44  24.2  6.57  15.4   152. -29.0 tent~     4
       6 spi   gamma ASN000~ 1991 May   630  21.3  7.52  14.4   152. -29.0 tent~     5
       7 spi   gamma ASN000~ 1991 Jun   242  19.6  3.65  11.6   152. -29.0 tent~     6
       8 spi   gamma ASN000~ 1991 Jul   580  15.3  0.519  7.91  152. -29.0 tent~     7
       9 spi   gamma ASN000~ 1991 Aug    14  17.8  1.67   9.76  152. -29.0 tent~     8
      10 spi   gamma ASN000~ 1991 Sep    78  21.1  3.07  12.1   152. -29.0 tent~     9
      # i 1,410 more rows
      # i 2 more variables: .scale <dbl>, .value <dbl>

