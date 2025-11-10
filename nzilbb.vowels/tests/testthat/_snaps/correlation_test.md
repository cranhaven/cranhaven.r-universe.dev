# Correlation summary returns expected result

    Code
      summary(correlation_test(select(onze_intercepts, -speaker), n = 10))
    Output
      Correlation test results.
      Count of significant pairwise correlations in original data at alpha = 0.05: 69
      Mean significant pairwise correlations in permuted data (n = 10) at alpha = 0.05: 9
      Min = 4, Max = 13.
      
      Top 5 pairwise correlations in original data:
      F1_FLEECE, F1_START: 0.55
      F2_FLEECE, F2_NURSE: -0.53
      F2_START, F2_THOUGHT: -0.51
      F1_GOOSE, F1_START: 0.47
      F1_STRUT, F1_TRAP: -0.47

