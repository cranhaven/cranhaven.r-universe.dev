# Can import and ECIS file

    Code
      ecis_import(raw = w16)
    Message
      i Starting import
      i Importing raw data
      i Reading file
      i Extracting data
      i Lengthening the dataset
    Condition
      Warning:
      Expected 2 pieces. Additional pieces discarded in 2 rows [1, 2].
    Message
      i Generating other physical quantaties
      i Cleaning up
      v Import complete
    Output
      # A tibble: 880 x 8
             Time Well  Frequency Unit   Value Instrument Excluded Sample
            <dbl> <chr>     <dbl> <fct>  <dbl> <chr>      <chr>    <chr> 
       1 0.000688 A01        1000 R      1508. ECIS       no       NA    
       2 0.000688 A01         125 R     22828. ECIS       no       NA    
       3 0.000688 A01       16000 R      1006. ECIS       no       NA    
       4 0.000688 A01        2000 R      1186. ECIS       no       NA    
       5 0.000688 A01         250 R      6776. ECIS       no       NA    
       6 0.000688 A01       32000 R       995. ECIS       no       NA    
       7 0.000688 A01        4000 R      1076. ECIS       no       NA    
       8 0.000688 A01         500 R      2556. ECIS       no       NA    
       9 0.000688 A01          62 R     82275. ECIS       no       NA    
      10 0.000688 A01       64000 R       996. ECIS       no       NA    
      # i 870 more rows

---

    Code
      ecis_import(model = empty)
    Message
      i Starting import
      i Importing model data
      i Reading file into R
      i Extracting useful data
      ! No data imported, check the modeled data you are trying to import is correctly specified and an intact file
      i Renaming units
      i Naming dataset
      i Creating long dataframe
      i Finishing up
      v Import complete
    Output
      # A tibble: 0 x 8
      # i 8 variables: Time <???>, Unit <???>, Well <???>, Value <???>,
      #   Frequency <???>, Instrument <???>, Excluded <chr>, Sample <chr>

---

    Code
      ecis_import(raw, modeled, experimentname = "TEST")
    Message
      i Starting import
      i Importing raw data
      i Reading file
      i Extracting data
      i Lengthening the dataset
      i Generating other physical quantaties
      i Cleaning up
      i Importing model data
      i Reading file into R
      i Extracting useful data
      i Renaming units
      i Naming dataset
      i Creating long dataframe
      i Finishing up
      v Import complete
    Output
      # A tibble: 14,400 x 9
          Time Well  Frequency Unit  Value Instrument Experiment Excluded Sample
         <dbl> <chr>     <dbl> <fct> <dbl> <chr>      <chr>      <chr>    <chr> 
       1     0 A01        1000 R      394. ECIS       TEST       no       NA    
       2     0 A01       16000 R      223. ECIS       TEST       no       NA    
       3     0 A01        2000 R      316. ECIS       TEST       no       NA    
       4     0 A01         250 R      824. ECIS       TEST       no       NA    
       5     0 A01       32000 R      212. ECIS       TEST       no       NA    
       6     0 A01        4000 R      267. ECIS       TEST       no       NA    
       7     0 A01         500 R      533. ECIS       TEST       no       NA    
       8     0 A01       64000 R      205. ECIS       TEST       no       NA    
       9     0 A01        8000 R      240. ECIS       TEST       no       NA    
      10     0 B01        1000 R      388. ECIS       TEST       no       NA    
      # i 14,390 more rows

# Can import cellZScope file

    Code
      cellzscope_import(raw, model, "test")
    Output
      # A tibble: 88,288 x 8
          Time Unit               Well  Value Experiment Frequency Instrument Sample  
         <dbl> <chr>              <chr> <dbl> <chr>          <dbl> <chr>      <chr>   
       1  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A01    NA   test               0 cellZscope " 80,00~
       2  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A02    46.8 test               0 cellZscope " 80,00~
       3  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A04    42.9 test               0 cellZscope " 20,00~
       4  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A05    NA   test               0 cellZscope " 20,00~
       5  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B01    NA   test               0 cellZscope " 80,00~
       6  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B02    NA   test               0 cellZscope " 80,00~
       7  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B04    NA   test               0 cellZscope " 20,00~
       8  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B05    NA   test               0 cellZscope " 20,00~
       9  1.3  CPE_A(sⁿ⁻¹·µF/cm²) A01    46.1 test               0 cellZscope " 80,00~
      10  1.3  CPE_A(sⁿ⁻¹·µF/cm²) A02    51.8 test               0 cellZscope " 80,00~
      # i 88,278 more rows

---

    Code
      cellzscope_import(raw, model)
    Output
      # A tibble: 88,288 x 8
          Time Unit               Well  Value Experiment   Frequency Instrument Sample
         <dbl> <chr>              <chr> <dbl> <chr>            <dbl> <chr>      <chr> 
       1  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A01    NA   zscopemodel~         0 cellZscope " 80,~
       2  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A02    46.8 zscopemodel~         0 cellZscope " 80,~
       3  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A04    42.9 zscopemodel~         0 cellZscope " 20,~
       4  0.03 CPE_A(sⁿ⁻¹·µF/cm²) A05    NA   zscopemodel~         0 cellZscope " 20,~
       5  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B01    NA   zscopemodel~         0 cellZscope " 80,~
       6  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B02    NA   zscopemodel~         0 cellZscope " 80,~
       7  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B04    NA   zscopemodel~         0 cellZscope " 20,~
       8  0.03 CPE_A(sⁿ⁻¹·µF/cm²) B05    NA   zscopemodel~         0 cellZscope " 20,~
       9  1.3  CPE_A(sⁿ⁻¹·µF/cm²) A01    46.1 zscopemodel~         0 cellZscope " 80,~
      10  1.3  CPE_A(sⁿ⁻¹·µF/cm²) A02    51.8 zscopemodel~         0 cellZscope " 80,~
      # i 88,278 more rows

# Can import xCELLigence file

    Code
      import_xcelligence(rawdata = rawdata, "TEST7")
    Output
      # A tibble: 6,336 x 10
          Time Unit  Value Well  Sample      Frequency Experiment Instrument  SampleID
         <dbl> <chr> <dbl> <chr> <chr>           <dbl> <chr>      <chr>          <int>
       1  0    CI     1    A01   HCMVEC80000     10000 TEST7      xCELLigence        1
       2  0    CI     1    A02   HCMVEC80000     10000 TEST7      xCELLigence        1
       3  0    CI     1    A03   HCMVEC80000     10000 TEST7      xCELLigence        1
       4  1.54 CI     3.59 A01   HCMVEC80000     10000 TEST7      xCELLigence        1
       5  1.54 CI     3.56 A02   HCMVEC80000     10000 TEST7      xCELLigence        1
       6  1.54 CI     3.72 A03   HCMVEC80000     10000 TEST7      xCELLigence        1
       7  3.21 CI     3.51 A01   HCMVEC80000     10000 TEST7      xCELLigence        1
       8  3.21 CI     3.48 A02   HCMVEC80000     10000 TEST7      xCELLigence        1
       9  3.21 CI     3.48 A03   HCMVEC80000     10000 TEST7      xCELLigence        1
      10  4.87 CI     3.39 A01   HCMVEC80000     10000 TEST7      xCELLigence        1
      # i 6,326 more rows
      # i 1 more variable: Excluded <chr>

---

    Code
      suppressMessages(import_xcelligence(rawdata = rawdata))
    Output
      # A tibble: 6,336 x 10
          Time Unit  Value Well  Sample      Frequency Experiment  Instrument SampleID
         <dbl> <chr> <dbl> <chr> <chr>           <dbl> <chr>       <chr>         <int>
       1  0    CI     1    A01   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       2  0    CI     1    A02   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       3  0    CI     1    A03   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       4  1.54 CI     3.59 A01   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       5  1.54 CI     3.56 A02   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       6  1.54 CI     3.72 A03   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       7  3.21 CI     3.51 A01   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       8  3.21 CI     3.48 A02   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
       9  3.21 CI     3.48 A03   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
      10  4.87 CI     3.39 A01   HCMVEC80000     10000 TEMPMDBFOR~ xCELLigen~        1
      # i 6,326 more rows
      # i 1 more variable: Excluded <chr>

---

    ERROR - file could not be duplicated to be opened. Ensure you have write capabilities in the new folder, and the file TEMPMDBFORIMPORT.mdb does not exist. Also check that the plt file is not open when you run this command

