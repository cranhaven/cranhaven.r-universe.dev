# ccf functions work

    Code
      vascr_cc(data.df)
    Output
      # A tibble: 8 x 10
      # Rowwise:  Experiment
        Well.x SampleID.x Sample.x      Experiment values.x Well.y SampleID.y Sample.y
        <chr>       <int> <fct>         <fct>      <list>   <chr>       <int> <fct>   
      1 A02             1 35,000_cells~ 1 : Exper~ <dbl>    A02             1 35,000_~
      2 A02             1 35,000_cells~ 1 : Exper~ <dbl>    A03             1 35,000_~
      3 A02             1 35,000_cells~ 1 : Exper~ <dbl>    D01             4 20,000_~
      4 A03             1 35,000_cells~ 1 : Exper~ <dbl>    A02             1 35,000_~
      5 A03             1 35,000_cells~ 1 : Exper~ <dbl>    A03             1 35,000_~
      6 A03             1 35,000_cells~ 1 : Exper~ <dbl>    D01             4 20,000_~
      7 D01             4 20,000_cells~ 1 : Exper~ <dbl>    D01             4 20,000_~
      8 D06             4 20,000_cells~ 2 : Exper~ <dbl>    D06             4 20,000_~
      # i 2 more variables: values.y <list>, cc <dbl>

---

    Code
      cc_data
    Output
      # A tibble: 135 x 10
      # Rowwise:  Experiment
         Well.x SampleID.x Sample.x     Experiment values.x Well.y SampleID.y Sample.y
         <chr>       <int> <chr>        <fct>      <list>   <chr>       <int> <chr>   
       1 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
       2 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A02             1 35,000_~
       3 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A03             1 35,000_~
       4 A01             1 20,000_cell~ 1 : Exper~ <dbl>    D01             4 35,000_~
       5 A01             1 20,000_cell~ 1 : Exper~ <dbl>    D02             4 35,000_~
       6 A01             1 20,000_cell~ 1 : Exper~ <dbl>    D03             4 35,000_~
       7 A01             1 5,000_cells~ 1 : Exper~ <dbl>    G01             7 35,000_~
       8 A01             1 5,000_cells~ 1 : Exper~ <dbl>    G02             7 35,000_~
       9 A01             1 5,000_cells~ 1 : Exper~ <dbl>    G03             7 35,000_~
      10 A02             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
      # i 125 more rows
      # i 2 more variables: values.y <list>, cc <dbl>

---

    Code
      cc_data
    Output
      # A tibble: 162 x 10
      # Rowwise:  Experiment
         Well.x SampleID.x Sample.x     Experiment values.x Well.y SampleID.y Sample.y
         <chr>       <int> <chr>        <fct>      <list>   <chr>       <int> <chr>   
       1 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
       2 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A02             1 35,000_~
       3 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A03             1 35,000_~
       4 A01             1 35,000_cell~ 1 : Exper~ <dbl>    D01             4 20,000_~
       5 A01             1 20,000_cell~ 1 : Exper~ <dbl>    D02             4 35,000_~
       6 A01             1 35,000_cell~ 1 : Exper~ <dbl>    D03             4 20,000_~
       7 A01             1 5,000_cells~ 1 : Exper~ <dbl>    G01             7 35,000_~
       8 A01             1 35,000_cell~ 1 : Exper~ <dbl>    G02             7 5,000_c~
       9 A01             1 5,000_cells~ 1 : Exper~ <dbl>    G03             7 35,000_~
      10 A02             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
      # i 152 more rows
      # i 2 more variables: values.y <list>, cc <dbl>

---

    Code
      cc_data
    Output
      # A tibble: 162 x 10
      # Rowwise:  Experiment
         Well.x SampleID.x Sample.x     Experiment values.x Well.y SampleID.y Sample.y
         <chr>       <int> <fct>        <fct>      <list>   <chr>       <int> <fct>   
       1 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
       2 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A02             1 35,000_~
       3 A01             1 35,000_cell~ 1 : Exper~ <dbl>    A03             1 35,000_~
       4 A01             1 35,000_cell~ 1 : Exper~ <dbl>    D01             4 20,000_~
       5 A01             1 35,000_cell~ 1 : Exper~ <dbl>    D02             4 20,000_~
       6 A01             1 35,000_cell~ 1 : Exper~ <dbl>    D03             4 20,000_~
       7 A01             1 35,000_cell~ 1 : Exper~ <dbl>    G01             7 5,000_c~
       8 A01             1 35,000_cell~ 1 : Exper~ <dbl>    G02             7 5,000_c~
       9 A01             1 35,000_cell~ 1 : Exper~ <dbl>    G03             7 5,000_c~
      10 A02             1 35,000_cell~ 1 : Exper~ <dbl>    A01             1 35,000_~
      # i 152 more rows
      # i 2 more variables: values.y <list>, cc <dbl>

---

    Code
      vascr_summarise_cc(cc_data, "experiments")
    Output
      # A tibble: 18 x 7
      # Groups:   Sample.x, Sample.y, SampleID.x, SampleID.y [6]
         Sample.x                Sample.y SampleID.x SampleID.y Experiment    cc     n
         <fct>                   <fct>         <int>      <int> <fct>      <dbl> <int>
       1 35,000_cells + HCMEC D~ 35,000_~          1          1 1 : Exper~ 0.998     9
       2 35,000_cells + HCMEC D~ 35,000_~          1          1 2 : Exper~ 0.996     9
       3 35,000_cells + HCMEC D~ 35,000_~          1          1 3 : Exper~ 0.994     9
       4 35,000_cells + HCMEC D~ 20,000_~          1          4 1 : Exper~ 0.998     9
       5 35,000_cells + HCMEC D~ 20,000_~          1          4 2 : Exper~ 0.996     9
       6 35,000_cells + HCMEC D~ 20,000_~          1          4 3 : Exper~ 0.992     9
       7 35,000_cells + HCMEC D~ 5,000_c~          1          7 1 : Exper~ 0.997     9
       8 35,000_cells + HCMEC D~ 5,000_c~          1          7 2 : Exper~ 0.996     9
       9 35,000_cells + HCMEC D~ 5,000_c~          1          7 3 : Exper~ 0.989     9
      10 20,000_cells + HCMEC D~ 20,000_~          4          4 1 : Exper~ 0.999     9
      11 20,000_cells + HCMEC D~ 20,000_~          4          4 2 : Exper~ 0.998     9
      12 20,000_cells + HCMEC D~ 20,000_~          4          4 3 : Exper~ 0.999     9
      13 20,000_cells + HCMEC D~ 5,000_c~          4          7 1 : Exper~ 0.998     9
      14 20,000_cells + HCMEC D~ 5,000_c~          4          7 2 : Exper~ 0.994     9
      15 20,000_cells + HCMEC D~ 5,000_c~          4          7 3 : Exper~ 0.998     9
      16 5,000_cells + HCMEC D3~ 5,000_c~          7          7 1 : Exper~ 0.997     9
      17 5,000_cells + HCMEC D3~ 5,000_c~          7          7 2 : Exper~ 0.998     9
      18 5,000_cells + HCMEC D3~ 5,000_c~          7          7 3 : Exper~ 0.997     9

---

    Code
      vascr_summarise_cc(cc_data, "summary")
    Output
      # A tibble: 6 x 6
      # Groups:   Sample.x [3]
        Sample.x                     Sample.y                 ccsem    cc totaln title
        <fct>                        <fct>                    <dbl> <dbl>  <int> <chr>
      1 35,000_cells + HCMEC D3_line 35,000_cells + HCMEC ~ 7.35e-4 0.996     27 "35,~
      2 35,000_cells + HCMEC D3_line 20,000_cells + HCMEC ~ 1.09e-3 0.995     27 "35,~
      3 35,000_cells + HCMEC D3_line 5,000_cells + HCMEC D~ 1.41e-3 0.994     27 "35,~
      4 20,000_cells + HCMEC D3_line 20,000_cells + HCMEC ~ 3.21e-4 0.999     27 "20,~
      5 20,000_cells + HCMEC D3_line 5,000_cells + HCMEC D~ 8.71e-4 0.997     27 "20,~
      6 5,000_cells + HCMEC D3_line  5,000_cells + HCMEC D~ 1.77e-4 0.998     27 "5,0~

