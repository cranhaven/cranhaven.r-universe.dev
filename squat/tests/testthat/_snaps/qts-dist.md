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
      2 0.00697                        
      3 0.10201 0.10485                
      4 0.09409 0.09115 0.00586        
      5 0.00428 0.01868 0.09695 0.09387

# Function dist.qts_sample() works via dtw pkg

    Code
      round(D, 5)
    Output
              1       2       3       4
      2 0.00949                        
      3 0.05129 0.05065                
      4 0.05121 0.04981 0.00863        
      5 0.00861 0.01552 0.05024 0.05070

