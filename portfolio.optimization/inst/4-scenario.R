# Use daily-based (crude) weekly return data from the S&P 100 in 2017
data(sp100w17)

# Only use 30 assets with the highest average trading volume during 2017
sort.by.volume <- sort(sp100w17av, index.return=TRUE, decreasing = TRUE)
top30.volume <- sort(sort.by.volume$ix[1:30])

# Create the scenario set with those
scenario.set <- sp100w17[, top30.volume]
rm(sp100w17, sp100w17av, sort.by.volume, top30.volume)
