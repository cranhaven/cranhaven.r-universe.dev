# export works

    Code
      re_import
    Output
      # A tibble: 2 x 25
         Time 0_cells + HCMEC D3_line.~1 0_cells + HCMEC D3_l~2 0_cells + HCMEC D3_l~3
        <dbl>                      <dbl>                  <dbl>                  <dbl>
      1     5                       311.                   324.                   328.
      2    10                       310.                   322.                   326.
      # i abbreviated names: 1: `0_cells + HCMEC D3_line...2`,
      #   2: `0_cells + HCMEC D3_line...3`, 3: `0_cells + HCMEC D3_line...4`
      # i 21 more variables: `10 000_cells + HCMEC D3_line...5` <dbl>,
      #   `10 000_cells + HCMEC D3_line...6` <dbl>,
      #   `10 000_cells + HCMEC D3_line...7` <dbl>,
      #   `15 000_cells + HCMEC D3_line...8` <dbl>,
      #   `15 000_cells + HCMEC D3_line...9` <dbl>, ...
    Code
      colnames(re_import)
    Output
       [1] "Time"                              "0_cells + HCMEC D3_line...2"      
       [3] "0_cells + HCMEC D3_line...3"       "0_cells + HCMEC D3_line...4"      
       [5] "10 000_cells + HCMEC D3_line...5"  "10 000_cells + HCMEC D3_line...6" 
       [7] "10 000_cells + HCMEC D3_line...7"  "15 000_cells + HCMEC D3_line...8" 
       [9] "15 000_cells + HCMEC D3_line...9"  "15 000_cells + HCMEC D3_line...10"
      [11] "20 000_cells + HCMEC D3_line...11" "20 000_cells + HCMEC D3_line...12"
      [13] "20 000_cells + HCMEC D3_line...13" "25 000_cells + HCMEC D3_line...14"
      [15] "25 000_cells + HCMEC D3_line...15" "25 000_cells + HCMEC D3_line...16"
      [17] "30 000_cells + HCMEC D3_line...17" "30 000_cells + HCMEC D3_line...18"
      [19] "30 000_cells + HCMEC D3_line...19" "35 000_cells + HCMEC D3_line...20"
      [21] "35 000_cells + HCMEC D3_line...21" "35 000_cells + HCMEC D3_line...22"
      [23] "5 000_cells + HCMEC D3_line...23"  "5 000_cells + HCMEC D3_line...24" 
      [25] "5 000_cells + HCMEC D3_line...25" 

---

    Code
      filepath = tempfile("test_export", fileext = ".xlsx")
      suppressMessages(vascr_export_prism(small_growth, filepath, level = "wells"))
      re_import = readxl::read_xlsx(filepath, 2)
    Message
      New names:
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...2`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...3`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...4`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...5`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...6`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...7`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...8`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...9`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...10`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...11`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...12`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...13`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...14`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...15`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...16`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...17`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...18`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...19`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...20`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...21`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...22`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...23`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...24`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...25`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...26`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...27`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...28`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...29`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...30`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...31`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...32`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...33`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...34`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...35`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...36`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...37`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...38`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...39`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...40`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...41`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...42`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...43`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...44`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...45`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...46`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...47`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...48`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...49`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...50`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...51`
      * `35 000_cells + HCMEC D3_line` -> `35 000_cells + HCMEC D3_line...52`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...53`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...54`
      * `30 000_cells + HCMEC D3_line` -> `30 000_cells + HCMEC D3_line...55`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...56`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...57`
      * `25 000_cells + HCMEC D3_line` -> `25 000_cells + HCMEC D3_line...58`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...59`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...60`
      * `20 000_cells + HCMEC D3_line` -> `20 000_cells + HCMEC D3_line...61`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...62`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...63`
      * `15 000_cells + HCMEC D3_line` -> `15 000_cells + HCMEC D3_line...64`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...65`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...66`
      * `10 000_cells + HCMEC D3_line` -> `10 000_cells + HCMEC D3_line...67`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...68`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...69`
      * `5 000_cells + HCMEC D3_line` -> `5 000_cells + HCMEC D3_line...70`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...71`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...72`
      * `0_cells + HCMEC D3_line` -> `0_cells + HCMEC D3_line...73`
    Code
      re_import
    Output
      # A tibble: 2 x 73
         Time 35 000_cells + HCMEC D3_~1 35 000_cells + HCMEC~2 35 000_cells + HCMEC~3
        <dbl>                      <dbl>                  <dbl>                  <dbl>
      1     5                       413.                   434.                   420.
      2    10                       470.                   519.                   469.
      # i abbreviated names: 1: `35 000_cells + HCMEC D3_line...2`,
      #   2: `35 000_cells + HCMEC D3_line...3`,
      #   3: `35 000_cells + HCMEC D3_line...4`
      # i 69 more variables: `30 000_cells + HCMEC D3_line...5` <dbl>,
      #   `30 000_cells + HCMEC D3_line...6` <dbl>,
      #   `30 000_cells + HCMEC D3_line...7` <dbl>,
      #   `25 000_cells + HCMEC D3_line...8` <dbl>, ...
    Code
      colnames(re_import)
    Output
       [1] "Time"                              "35 000_cells + HCMEC D3_line...2" 
       [3] "35 000_cells + HCMEC D3_line...3"  "35 000_cells + HCMEC D3_line...4" 
       [5] "30 000_cells + HCMEC D3_line...5"  "30 000_cells + HCMEC D3_line...6" 
       [7] "30 000_cells + HCMEC D3_line...7"  "25 000_cells + HCMEC D3_line...8" 
       [9] "25 000_cells + HCMEC D3_line...9"  "25 000_cells + HCMEC D3_line...10"
      [11] "20 000_cells + HCMEC D3_line...11" "20 000_cells + HCMEC D3_line...12"
      [13] "20 000_cells + HCMEC D3_line...13" "15 000_cells + HCMEC D3_line...14"
      [15] "15 000_cells + HCMEC D3_line...15" "15 000_cells + HCMEC D3_line...16"
      [17] "10 000_cells + HCMEC D3_line...17" "10 000_cells + HCMEC D3_line...18"
      [19] "10 000_cells + HCMEC D3_line...19" "5 000_cells + HCMEC D3_line...20" 
      [21] "5 000_cells + HCMEC D3_line...21"  "5 000_cells + HCMEC D3_line...22" 
      [23] "0_cells + HCMEC D3_line...23"      "0_cells + HCMEC D3_line...24"     
      [25] "0_cells + HCMEC D3_line...25"      "35 000_cells + HCMEC D3_line...26"
      [27] "35 000_cells + HCMEC D3_line...27" "35 000_cells + HCMEC D3_line...28"
      [29] "30 000_cells + HCMEC D3_line...29" "30 000_cells + HCMEC D3_line...30"
      [31] "30 000_cells + HCMEC D3_line...31" "25 000_cells + HCMEC D3_line...32"
      [33] "25 000_cells + HCMEC D3_line...33" "25 000_cells + HCMEC D3_line...34"
      [35] "20 000_cells + HCMEC D3_line...35" "20 000_cells + HCMEC D3_line...36"
      [37] "20 000_cells + HCMEC D3_line...37" "15 000_cells + HCMEC D3_line...38"
      [39] "15 000_cells + HCMEC D3_line...39" "15 000_cells + HCMEC D3_line...40"
      [41] "10 000_cells + HCMEC D3_line...41" "10 000_cells + HCMEC D3_line...42"
      [43] "10 000_cells + HCMEC D3_line...43" "5 000_cells + HCMEC D3_line...44" 
      [45] "5 000_cells + HCMEC D3_line...45"  "5 000_cells + HCMEC D3_line...46" 
      [47] "0_cells + HCMEC D3_line...47"      "0_cells + HCMEC D3_line...48"     
      [49] "0_cells + HCMEC D3_line...49"      "35 000_cells + HCMEC D3_line...50"
      [51] "35 000_cells + HCMEC D3_line...51" "35 000_cells + HCMEC D3_line...52"
      [53] "30 000_cells + HCMEC D3_line...53" "30 000_cells + HCMEC D3_line...54"
      [55] "30 000_cells + HCMEC D3_line...55" "25 000_cells + HCMEC D3_line...56"
      [57] "25 000_cells + HCMEC D3_line...57" "25 000_cells + HCMEC D3_line...58"
      [59] "20 000_cells + HCMEC D3_line...59" "20 000_cells + HCMEC D3_line...60"
      [61] "20 000_cells + HCMEC D3_line...61" "15 000_cells + HCMEC D3_line...62"
      [63] "15 000_cells + HCMEC D3_line...63" "15 000_cells + HCMEC D3_line...64"
      [65] "10 000_cells + HCMEC D3_line...65" "10 000_cells + HCMEC D3_line...66"
      [67] "10 000_cells + HCMEC D3_line...67" "5 000_cells + HCMEC D3_line...68" 
      [69] "5 000_cells + HCMEC D3_line...69"  "5 000_cells + HCMEC D3_line...70" 
      [71] "0_cells + HCMEC D3_line...71"      "0_cells + HCMEC D3_line...72"     
      [73] "0_cells + HCMEC D3_line...73"     
    Code
      remove(filepath)

