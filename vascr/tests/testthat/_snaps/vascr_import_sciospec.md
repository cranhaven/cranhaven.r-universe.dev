# Check col exists

    Code
      import_sciospec_single(cur_file)
    Output
           frequency[Hz]   Re[Ohm]    Im[Ohm]                channel
                   <num>     <num>      <num>                 <char>
        1:      100.0008 20260.602 -19550.561 Channel: ECISadapter 1
        2:      109.7796 18171.346 -15598.238 Channel: ECISadapter 1
        3:      120.4899 17838.764 -16082.875 Channel: ECISadapter 1
        4:      132.2478 17475.324 -14845.407 Channel: ECISadapter 1
        5:      145.0535 17380.291 -13290.462 Channel: ECISadapter 1
       ---                                                          
       97:   756463.1875  2310.753  -2470.794 Channel: ECISadapter 1
       98:   830217.3750  2204.136  -2373.112 Channel: ECISadapter 1
       99:   911162.6250  2116.315  -2280.082 Channel: ECISadapter 1
      100:   999999.8125  2023.540  -2195.740 Channel: ECISadapter 1
      101:     4000.0305 13646.221  -1729.712 Channel: ECISadapter 1
                                     time
                                   <char>
        1: 31-Mar.-2025 10:12:36:478 p.m.
        2: 31-Mar.-2025 10:12:36:478 p.m.
        3: 31-Mar.-2025 10:12:36:478 p.m.
        4: 31-Mar.-2025 10:12:36:478 p.m.
        5: 31-Mar.-2025 10:12:36:478 p.m.
       ---                               
       97: 31-Mar.-2025 10:12:36:478 p.m.
       98: 31-Mar.-2025 10:12:36:478 p.m.
       99: 31-Mar.-2025 10:12:36:478 p.m.
      100: 31-Mar.-2025 10:12:36:478 p.m.
      101: 31-Mar.-2025 10:12:36:478 p.m.

---

    Code
      suppressMessages(import_sciospec(data_path))
    Output
      # A tibble: 16,160 x 11
         channel      Frequency Unit    Value Well  Instrument  Time Experiment Sample
         <chr>            <dbl> <chr>   <dbl> <chr> <chr>      <dbl> <lgl>      <chr> 
       1 Channel: EC~      100. R      20261. D02   sciospec       0 NA         D02   
       2 Channel: EC~      100. I     -19551. D02   sciospec       0 NA         D02   
       3 Channel: EC~      110. R      18171. D02   sciospec       0 NA         D02   
       4 Channel: EC~      110. I     -15598. D02   sciospec       0 NA         D02   
       5 Channel: EC~      120. R      17839. D02   sciospec       0 NA         D02   
       6 Channel: EC~      120. I     -16083. D02   sciospec       0 NA         D02   
       7 Channel: EC~      132. R      17475. D02   sciospec       0 NA         D02   
       8 Channel: EC~      132. I     -14845. D02   sciospec       0 NA         D02   
       9 Channel: EC~      145. R      17380. D02   sciospec       0 NA         D02   
      10 Channel: EC~      145. I     -13290. D02   sciospec       0 NA         D02   
      # i 16,150 more rows
      # i 2 more variables: SampleID <dbl>, Excluded <chr>

---

    Code
      suppressMessages(import_sciospec(data_path, shear = TRUE))
    Output
      # A tibble: 16,160 x 11
         channel      Frequency Unit    Value Well  Instrument  Time Experiment Sample
         <chr>            <dbl> <chr>   <dbl> <chr> <chr>      <dbl> <lgl>      <chr> 
       1 Channel: EC~      100. R      20261. F01   sciospec       0 NA         F01   
       2 Channel: EC~      100. I     -19551. F01   sciospec       0 NA         F01   
       3 Channel: EC~      110. R      18171. F01   sciospec       0 NA         F01   
       4 Channel: EC~      110. I     -15598. F01   sciospec       0 NA         F01   
       5 Channel: EC~      120. R      17839. F01   sciospec       0 NA         F01   
       6 Channel: EC~      120. I     -16083. F01   sciospec       0 NA         F01   
       7 Channel: EC~      132. R      17475. F01   sciospec       0 NA         F01   
       8 Channel: EC~      132. I     -14845. F01   sciospec       0 NA         F01   
       9 Channel: EC~      145. R      17380. F01   sciospec       0 NA         F01   
      10 Channel: EC~      145. I     -13290. F01   sciospec       0 NA         F01   
      # i 16,150 more rows
      # i 2 more variables: SampleID <dbl>, Excluded <chr>

