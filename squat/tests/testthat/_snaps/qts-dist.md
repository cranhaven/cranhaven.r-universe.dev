# Function dist.default() works

    Code
      round(D, 5)
    Output
              x
      y 1.41421

# Function dist.qts_sample() works via fdacluster pkg

    Code
      round(D, 5)
    Output
              1       2       3       4
      2 0.00193                        
      3 0.03135 0.02944                
      4 0.03198 0.02913 0.00083        
      5 0.00142 0.00508 0.02941 0.03091

# Function dist.qts_sample() works via dtw pkg

    Code
      round(D, 5)
    Output
              1       2       3       4
      2 0.00949                        
      3 0.05129 0.05065                
      4 0.05121 0.04981 0.00863        
      5 0.00861 0.01552 0.05024 0.05070

