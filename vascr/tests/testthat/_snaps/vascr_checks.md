# Check duplicate

    Code
      vascr_check_duplicate(map_4, "Row")
    Message
      ! Row [ A ]   defined more than once 
      
      # A tibble: 2 x 5
        Row    Freq SampleID Column Sample                       
        <chr> <int>    <dbl> <chr>  <chr>                        
      1 A         2        1 1 2 3  10 nM Treatment 1 + 1nm water
      2 A         2        3 1 2 3  10 nM Treatment 2 + 1nm water
    Output
      NULL

---

    Code
      vascr_check_duplicate(map_5, "Sample")
    Message
      ! Sample [ 10 nM Treatment 1 + 1nm water ]   defined more than once 
      
      # A tibble: 2 x 5
        Sample                         Freq SampleID Row   Column
        <chr>                         <int>    <dbl> <chr> <chr> 
      1 10 nM Treatment 1 + 1nm water     2        1 A     1 2 3 
      2 10 nM Treatment 1 + 1nm water     2        3 C     1 2 3 
    Output
      NULL

---

    Code
      vascr_check_duplicate(map_5, "Row")
    Output
      NULL

# Check col exists

    Code
      vascr_check_col_exists(map_4, "Row")
    Output
      [1] TRUE

---

    Code
      vascr_check_col_exists(map_4, "Not_A_Col")
    Message
      ! Not_A_Col not found in dataframe. Please check
    Output
      [1] FALSE

# Checking of resampling works

    Code
      suppressMessages({
        w16 = system.file("extdata/instruments/ecis_16_testplate.abp", package = "vascr")
        d16 = vascr_import("ECIS", raw = w16, experiment = "W16")
      })

---

    Code
      vascr_check_resampled(d16)
    Output
      [1] FALSE

# Replicate consistency checks work

    Code
      vascr_check_replicates(growth.df)
    Output
      # A tibble: 8 x 4
        Sample                       SampleID count experiments                       
        <chr>                           <int> <int> <chr>                             
      1 0_cells + HCMEC D3_line             8     3 1 : Experiment 1 | 2 : Experiment~
      2 10,000_cells + HCMEC D3_line        6     3 1 : Experiment 1 | 2 : Experiment~
      3 15,000_cells + HCMEC D3_line        5     3 1 : Experiment 1 | 2 : Experiment~
      4 20,000_cells + HCMEC D3_line        4     3 1 : Experiment 1 | 2 : Experiment~
      5 25,000_cells + HCMEC D3_line        3     3 1 : Experiment 1 | 2 : Experiment~
      6 30,000_cells + HCMEC D3_line        2     3 1 : Experiment 1 | 2 : Experiment~
      7 35,000_cells + HCMEC D3_line        1     3 1 : Experiment 1 | 2 : Experiment~
      8 5,000_cells + HCMEC D3_line         7     3 1 : Experiment 1 | 2 : Experiment~

---

    Code
      vascr_check_replicates(data.df)
    Output
      # A tibble: 3 x 4
        Sample                       SampleID count experiments                       
        <fct>                           <int> <int> <chr>                             
      1 25,000_cells + HCMEC D3_line        3     2 1 : Experiment 1 | 2 : Experiment2
      2 35,000_cells + HCMEC D3_line        1     3 1 : Experiment 1 | 2 : Experiment~
      3 30,000_cells + HCMEC D3_line        2     3 1 : Experiment 1 | 2 : Experiment~

