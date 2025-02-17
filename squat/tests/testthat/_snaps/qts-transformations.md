# the function qts2dts() works

    Code
      qts2dts(vespa64$igp[[1]], vespa64$igp[[2]])
    Output
      # A tibble: 101 x 2
          time distance
         <int>    <dbl>
       1     0  0.0120 
       2     1  0.0106 
       3     2  0.00983
       4     3  0.00936
       5     4  0.00794
       6     5  0.00714
       7     6  0.00731
       8     7  0.00775
       9     8  0.00808
      10     9  0.00824
      # i 91 more rows

# the function qts2nts() works

    Code
      qts2nts(vespa64$igp[[1]], disable_normalization = FALSE)
    Output
      # A tibble: 101 x 2
          time  norm
         <int> <dbl>
       1     0 0.214
       2     1 0.203
       3     2 0.191
       4     3 0.178
       5     4 0.167
       6     5 0.157
       7     6 0.147
       8     7 0.140
       9     8 0.132
      10     9 0.125
      # i 91 more rows

---

    Code
      qts2nts(vespa64$igp[[1]], disable_normalization = TRUE)
    Output
      # A tibble: 101 x 2
          time  norm
         <int> <dbl>
       1     0 0.214
       2     1 0.203
       3     2 0.191
       4     3 0.178
       5     4 0.167
       6     5 0.157
       7     6 0.147
       8     7 0.140
       9     8 0.132
      10     9 0.125
      # i 91 more rows

# the function qts2ats() works

    Code
      qts2ats(vespa64$igp[[1]], disable_normalization = FALSE)
    Output
      # A tibble: 101 x 2
          time  angle
         <int>  <dbl>
       1     0 0     
       2     1 0.0113
       3     2 0.0235
       4     3 0.0366
       5     4 0.0480
       6     5 0.0583
       7     6 0.0673
       8     7 0.0752
       9     8 0.0828
      10     9 0.0908
      # i 91 more rows

---

    Code
      qts2ats(vespa64$igp[[1]], disable_normalization = TRUE)
    Output
      # A tibble: 101 x 2
          time  angle
         <int>  <dbl>
       1     0 0     
       2     1 0.0113
       3     2 0.0235
       4     3 0.0366
       5     4 0.0480
       6     5 0.0583
       7     6 0.0673
       8     7 0.0752
       9     8 0.0828
      10     9 0.0908
      # i 91 more rows

# the function qts2avts() works

    Code
      qts2avts(vespa64$igp[[1]], body_frame = FALSE)
    Output
      # A tibble: 100 x 4
          time        x        y          z
         <dbl>    <dbl>    <dbl>      <dbl>
       1     1 -0.0103  -0.00465 -0.0000691
       2     2 -0.0105  -0.00624 -0.000639 
       3     3 -0.0107  -0.00738 -0.00151  
       4     4 -0.00905 -0.00697 -0.00218  
       5     5 -0.00766 -0.00673 -0.00274  
       6     6 -0.00608 -0.00637 -0.00325  
       7     7 -0.00453 -0.00621 -0.00387  
       8     8 -0.00365 -0.00642 -0.00451  
       9     9 -0.00327 -0.00697 -0.00501  
      10    10 -0.00376 -0.00742 -0.00521  
      # i 90 more rows

---

    Code
      qts2avts(vespa64$igp[[1]], body_frame = TRUE)
    Output
      # A tibble: 100 x 4
          time        x        y         z
         <dbl>    <dbl>    <dbl>     <dbl>
       1     1 -0.0104  -0.00444 -0.000795
       2     2 -0.0106  -0.00610 -0.00115 
       3     3 -0.0107  -0.00735 -0.00188 
       4     4 -0.00894 -0.00705 -0.00238 
       5     5 -0.00748 -0.00689 -0.00280 
       6     6 -0.00585 -0.00661 -0.00317 
       7     7 -0.00424 -0.00654 -0.00364 
       8     8 -0.00331 -0.00682 -0.00417 
       9     9 -0.00290 -0.00742 -0.00458 
      10    10 -0.00337 -0.00789 -0.00478 
      # i 90 more rows

