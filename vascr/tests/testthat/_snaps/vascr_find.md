# Find varous things

    Code
      vascr_find_normalised(standard)
    Output
      [1] FALSE

---

    Code
      vascr_find_normalised(normal)
    Output
      [1] 88.88889

# Force median

    Code
      vascr:::vascr_force_median(c(1, 3, 5, 6), "up")
    Output
      [1] 5

---

    Code
      vascr:::vascr_force_median(c(1, 3, 5, 6), "down")
    Output
      [1] 3

# Vascr match

    Code
      vascr_match("Re", vector)
    Message
      ! [Re] corrected to [Rb]. Please check the argeuments for your functions are correctly typed.
    Output
      [1] "Rb"

---

    Code
      vascr_match("Rb", vector)
    Output
      [1] "Rb"

---

    Code
      vascr_match(c("Rb", "Cm"), vector)
    Output
      [1] "Rb" "Cm"

# vascr_find_single_time

    Code
      vascr_find_single_time(small_growth.df, NULL)
    Output
       [1]   0.00000  22.22222  44.44444  66.66667  88.88889 111.11111 133.33333
       [8] 155.55556 177.77778 200.00000

---

    Code
      vascr_find_single_time(small_growth.df, c(1, 3))
    Message
      ! Vascr_find_single_time deals with only one time in one call. Use find times if more parsing is needed.
    Output
      [1] "NA"

---

    Code
      vascr_find_single_time(c(1, 2, 3), c(1, 3))
    Message
      ! Vascr_find_single_time deals with only one time in one call. Use find times if more parsing is needed.
    Output
      [1] "NA"

---

    Code
      vascr_find_single_time(small_growth.df, c(4.876))
    Message
      ! [ 4.876 ]  corrected to  [ 0 ]. Please check the variables used.
    Output
      [1] 0

# vascr_find_well

    Code
      vascr_find_well(small_growth.df, NULL)
    Output
       [1] "A01" "A02" "A03" "A04" "A05" "A06" "A07" "A08" "A09" "B01" "B02" "B03"
      [13] "B04" "B05" "B06" "B07" "B08" "B09" "C01" "C02" "C03" "C04" "C05" "C06"
      [25] "C07" "C08" "C09" "D01" "D02" "D03" "D04" "D05" "D06" "D07" "D08" "D09"
      [37] "E01" "E02" "E03" "E04" "E05" "E06" "E07" "E08" "E09" "F01" "F02" "F03"
      [49] "F04" "F05" "F06" "F07" "F08" "F09" "G01" "G02" "G03" "G04" "G05" "G06"
      [61] "G07" "G08" "G09" "H07" "H08" "H09" "H01" "H02" "H03" "H04" "H05" "H06"

---

    Code
      vascr_find_well(small_growth.df, "A01")
    Output
      [1] "A01"

---

    Code
      vascr_find_well(small_growth.df, "A1")
    Output
      [1] "A01"

---

    Code
      vascr_find_well(small_growth.df, "W39")
    Message
      ! Well NA is not a valid well name, please check your input data
      ! [NA] corrected to [A01]. Please check the argeuments for your functions are correctly typed.
    Output
      [1] "A01"

# vascr_find_time

    Code
      vascr_find_time(small_growth.df, NULL)
    Output
       [1]   0.00000  22.22222  44.44444  66.66667  88.88889 111.11111 133.33333
       [8] 155.55556 177.77778 200.00000

---

    Code
      vascr_find_time(small_growth.df, list(1, 3, 5))
    Message
      ! [ 1 ]  corrected to  [ 0 ]. Please check the variables used.
      ! [ 3 ]  corrected to  [ 0 ]. Please check the variables used.
      ! [ 5 ]  corrected to  [ 0 ]. Please check the variables used.
    Output
      [1] 0 0 0

---

    Code
      vascr_find_time(small_growth.df, Inf)
    Output
       [1]   0.00000  22.22222  44.44444  66.66667  88.88889 111.11111 133.33333
       [8] 155.55556 177.77778 200.00000

---

    Code
      vascr_find_time(small_growth.df, c(10, 20))
    Output
      numeric(0)

---

    Code
      vascr_find_time(small_growth.df, 5)
    Message
      ! [ 5 ]  corrected to  [ 0 ]. Please check the variables used.
    Output
      [1] 0

---

    Code
      vascr_find_time(small_growth.df, NA)
    Output
      [1] 111.1111

# vascr_find_frequency

    Code
      vascr_find_frequency(small_growth.df, 4382)
    Message
      ! Frequency corrected from 4382 to 4000
    Output
      [1] 4000

---

    Code
      vascr_find_frequency(small_growth.df, 4000)
    Output
      [1] 4000

---

    Code
      vascr_find_frequency(small_growth.df, NULL)
    Output
       [1]     0   250   500  1000  2000  4000  8000 16000 32000 64000

---

    Code
      vascr_find_frequency(small_growth.df, NA)
    Output
      [1] 4000

---

    Code
      vascr_find_frequency(small_growth.df, Inf)
    Output
      [1] Inf

---

    Code
      vascr_find_frequency(small_growth.df, "raw")
    Output
      [1]   250   500  1000  2000  4000  8000 16000 32000 64000

---

    Code
      vascr_find_frequency(small_growth.df, "model")
    Output
      [1] 0

---

    Code
      suppressMessages(vascr_find_frequency(small_growth.df, frequency = c(100, 200)))
    Output
      [1]   0 250

# vascr_instrument_list

    Code
      vascr_instrument_list()
    Output
      [1] "ECIS"        "xCELLigence" "cellZscope" 

# vascr_units_table

    Code
      vascr_units_table()
    Output
      # A tibble: 22 x 4
         Unit  Content                     Modeled Instrument
         <chr> <chr>                       <lgl>   <chr>     
       1 Alpha ohm<sup>1/2</sup> cm        TRUE    ECIS      
       2 Cm    Cm (&#956;F/cm<sup>2</sup>) TRUE    ECIS      
       3 Drift Drift (%)                   TRUE    ECIS      
       4 Rb    Rb (ohm cm<sup>2</sup>)     TRUE    ECIS      
       5 RMSE  Model Fit RMSE              TRUE    ECIS      
       6 C     Capacitance (nF, 0 Hz)      FALSE   ECIS      
       7 C     Capacitance (nF, 0 Hz)      FALSE   cellZscope
       8 P     Phase (radians, 0 Hz)       FALSE   ECIS      
       9 P     Phase (radians, 0 Hz)       FALSE   cellZscope
      10 R     Resistance (ohm, 0 Hz)      FALSE   ECIS      
      # i 12 more rows

# vascr_find_instrument

    Code
      vascr_find_instrument(small_growth.df, "Rb")
    Message
      ! [Rb] corrected to [ECIS]. Please check the argeuments for your functions are correctly typed.
    Output
      [1] "ECIS"

---

    Code
      vascr_find_instrument(small_growth.df, NULL)
    Output
      [1] "ECIS"

---

    Code
      vascr_find_instrument(small_growth.df, "cellZscope")
    Message
      ! cellZscope data is not present in the dataset. Use with care
      ! No selected instruments present in dataset. Use with care.
    Output
      NULL

---

    Code
      vascr_find_instrument(small_growth.df, c("cellZscope", "ECIS"))
    Message
      ! cellZscope data is not present in the dataset. Use with care
    Output
      [1] "ECIS"

---

    Code
      vascr_find_instrument(small_growth.df, c("cellZscope", "xCELLigence"))
    Message
      ! cellZscope data is not present in the dataset. Use with care
      ! xCELLigence data is not present in the dataset. Use with care
      ! No selected instruments present in dataset. Use with care.
    Output
      NULL

# vascr_find_unit

    Code
      vascr_find_unit(small_growth.df, "raw")
    Output
      [1] "C" "P" "R" "X" "Z"

---

    Code
      vascr_find_unit(small_growth.df, "modeled")
    Output
      [1] "Alpha" "Cm"    "Drift" "Rb"    "RMSE" 

---

    Code
      vascr_find_unit(small_growth.df, "all")
    Output
       [1] "Alpha" "Cm"    "Drift" "Rb"    "RMSE"  "C"     "P"     "R"     "X"    
      [10] "Z"    

---

    Code
      vascr_find_unit(small_growth.df, "Cm")
    Output
      [1] "Cm"

---

    Code
      vascr_find_unit(small_growth.df, NULL)
    Output
       [1] "Alpha" "Cm"    "Drift" "RMSE"  "Rb"    "C"     "P"     "R"     "X"    
      [10] "Z"    

---

    Code
      vascr_find_unit(small_growth.df, unit = c("Ci", "Rb"))
    Message
      ! [Ci] corrected to [Cm]. Please check the argeuments for your functions are correctly typed.
    Output
      [1] "Cm" "Rb"

---

    Code
      vascr_find_unit(small_growth.df, NA)
    Output
      [1] "R"

---

    Code
      vascr_find_unit(small_growth.df %>% mutate(Instrument = "cellZscope"), NA)
    Output
      [1] "TER"

---

    Code
      vascr_find_unit(small_growth.df %>% mutate(Instrument = "xCELLigence"), NA)
    Output
      [1] "CI"

# vascr_find_experiment

    Code
      vascr_find_experiment(small_growth.df, 1)
    Output
      [1] "1 : Experiment 1"

---

    Code
      vascr_find_experiment(small_growth.df, "1 : Experiment 1")
    Output
      [1] "1 : Experiment 1"

---

    Code
      vascr_find_experiment(small_growth.df, NULL)
    Output
      [1] 1 : Experiment 1 2 : Experiment2  3 : Experiment3 
      Levels: 1 : Experiment 1 2 : Experiment2 3 : Experiment3

# vascr_titles render

    Code
      vascr_titles(unit, frequency)
    Output
      Capacitance (nF, 1000 Hz)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Resistance (ohm, 1000 Hz)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Phase (radians, 1000 Hz)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Capacative Reactance (ohm, 1000 Hz)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Impedance (ohm, 1000 Hz)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Rb (ohm cm<sup>2</sup>)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Cm (&#956;F/cm<sup>2</sup>)

---

    Code
      vascr_titles(unit, frequency)
    Output
      ohm<sup>1/2</sup> cm

---

    Code
      vascr_titles(unit, frequency)
    Output
      Model Fit RMSE

---

    Code
      vascr_titles(unit, frequency)
    Output
      Drift (%)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Cell Index

---

    Code
      vascr_titles(unit, frequency)
    Output
      CPE_A (s<sup>n-1</sup>&#956;F/cm<sup>2</sup>)

---

    Code
      vascr_titles(unit, frequency)
    Output
      CPE_n

---

    Code
      vascr_titles(unit, frequency)
    Output
      TER (&#937; cm<sup>2</sup>)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Ccl (&#956;F/cm<sup>2</sup>)

---

    Code
      vascr_titles(unit, frequency)
    Output
      Rmed (ohm)

# vascr_titles

    Code
      vascr_titles("random text, not changed")
    Output
      [1] "random text, not changed"

---

    Code
      vascr_titles_vector(c("Rb", "R", "Cm"))
    Output
      [1] "Rb (ohm cm<sup>2</sup>)"     "Resistance (ohm, 0 Hz)"     
      [3] "Cm (&#956;F/cm<sup>2</sup>)"

---

    Code
      vascr_instrument_units("ECIS")
    Output
       [1] "Alpha" "Cm"    "Drift" "Rb"    "RMSE"  "C"     "P"     "R"     "X"    
      [10] "Z"    

---

    Code
      vascr_instrument_units("xCELLigence")
    Output
      [1] "Z"  "CI"

---

    Code
      vascr_instrument_units("cellZscope")
    Output
       [1] "CPE_A" "CPE_n" "TER"   "Ccl"   "Rmed"  "C"     "P"     "R"     "X"    
      [10] "Z"    

---

    Code
      vascr_instrument_from_unit("Rb")
    Output
      [1] "ECIS"

---

    Code
      vascr_instrument_from_unit("CI")
    Output
      [1] "xCELLigence"

---

    Code
      vascr_instrument_from_unit("TER")
    Output
      [1] "cellZscope"

# test if data is summarised

    Code
      vascr_find_level(small_growth.df)
    Output
      [1] "wells"

---

    Code
      vascr_find_level(vascr_summarise(small_growth.df, level = "experiments"))
    Output
      [1] "experiments"

---

    Code
      vascr_find_level(vascr_summarise(small_growth.df, level = "summary"))
    Output
      [1] "summary"

# test vascr file validation

    Code
      vascr_validate_file(test_file_path, "abp")
    Output
      [1] TRUE

---

    Code
      vascr_validate_file(test_file_path, extension = c("abp", "r"))
    Output
      [1] TRUE

# test well standardisation

    Code
      vascr_standardise_wells("A01")
    Output
      [1] "A01"

---

    Code
      vascr_standardise_wells("A 1")
    Output
      [1] "A01"

---

    Code
      vascr_standardise_wells("tortoise")
    Message
      ! Well NA is not a valid well name, please check your input data
    Output
      [1] "NA"

---

    Code
      vascr_standardise_wells(small_growth.df$Well) %>% head()
    Output
      [1] "A01" "A01" "A01" "A01" "A01" "A01"

# 96 well names are correct

    Code
      vascr_96_well_names()
    Output
       [1] "A01" "B01" "C01" "D01" "E01" "F01" "G01" "H01" "A02" "B02" "C02" "D02"
      [13] "E02" "F02" "G02" "H02" "A03" "B03" "C03" "D03" "E03" "F03" "G03" "H03"
      [25] "A04" "B04" "C04" "D04" "E04" "F04" "G04" "H04" "A05" "B05" "C05" "D05"
      [37] "E05" "F05" "G05" "H05" "A06" "B06" "C06" "D06" "E06" "F06" "G06" "H06"
      [49] "A07" "B07" "C07" "D07" "E07" "F07" "G07" "H07" "A08" "B08" "C08" "D08"
      [61] "E08" "F08" "G08" "H08" "A09" "B09" "C09" "D09" "E09" "F09" "G09" "H09"
      [73] "A10" "B10" "C10" "D10" "E10" "F10" "G10" "H10" "A11" "B11" "C11" "D11"
      [85] "E11" "F11" "G11" "H11" "A12" "B12" "C12" "D12" "E12" "F12" "G12" "H12"
      [97] "NC" 

# vascr_gg_hue

    Code
      vascr_gg_color_hue(5)
    Output
      [1] "#F8766D" "#9BA800" "#00C08F" "#00A7FF" "#F862DE"

# vascr_colnames_works

    Code
      vascr_cols()
    Output
       [1] "Time"       "Unit"       "Value"      "Well"       "Sample"    
       [6] "Frequency"  "Experiment" "Instrument" "SampleID"   "Excluded"  

---

    Code
      vascr_cols(small_growth.df, set = "exploded")
    Output
      [1] "cells" "line" 

---

    Code
      vascr_cols(small_growth.df, set = "core")
    Output
       [1] "Time"       "Unit"       "Value"      "Well"       "Sample"    
       [6] "Frequency"  "Experiment" "Instrument" "SampleID"   "Excluded"  

---

    Code
      vascr_cols(small_growth.df, set = "not_a_set")
    Message
      ! Inappropriate set selected, please use another
    Output
      NULL

# Printing vascr names works

    Code
      vascr_samples(small_growth.df)
    Output
      # A tibble: 8 x 3
        SampleID Sample                       Experiment                              
           <int> <chr>                        <chr>                                   
      1        8 0_cells + HCMEC D3_line      1 : Experiment 1 H01 H02 H03 | 2 : Expe~
      2        6 10,000_cells + HCMEC D3_line 1 : Experiment 1 F01 F02 F03 | 2 : Expe~
      3        5 15,000_cells + HCMEC D3_line 1 : Experiment 1 E01 E02 E03 | 2 : Expe~
      4        4 20,000_cells + HCMEC D3_line 1 : Experiment 1 D01 D02 D03 | 2 : Expe~
      5        3 25,000_cells + HCMEC D3_line 1 : Experiment 1 C01 C02 C03 | 2 : Expe~
      6        2 30,000_cells + HCMEC D3_line 1 : Experiment 1 B01 B02 B03 | 2 : Expe~
      7        1 35,000_cells + HCMEC D3_line 1 : Experiment 1 A01 A02 A03 | 2 : Expe~
      8        7 5,000_cells + HCMEC D3_line  1 : Experiment 1 G01 G02 G03 | 2 : Expe~

# Find metadata works

    Code
      vascr_find_metadata(small_growth.df)
    Message
      
      Timepoints

# find col works

    Code
      vascr_find_col(small_growth.df, "HCMEC/D3")
    Message
      ! [HCMEC/D3] corrected to [SampleID]. Please check the argeuments for your functions are correctly typed.
    Output
      [1] "SampleID"

---

    Code
      vascr_find_col(small_growth.df, "line")
    Output
      [1] "line"

# SampleID from sample works

    Code
      vascr_find_sampleid_from_sample(small_growth.df, "5,000_cells + HCMEC D3_line")
    Output
      [1] 7

# Find sample works

    Code
      vascr_find_sample(small_growth.df, NA)
    Output
      [1] "35,000_cells + HCMEC D3_line"

