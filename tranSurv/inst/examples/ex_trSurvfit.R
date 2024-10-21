data(channing, package = "boot")
chan <- subset(channing, sex == "Male" & entry < exit)

## No display
(fit <- with(chan, trSurvfit(entry, exit, cens)))

## With diagnostic plots and the survival estimate
with(chan, trSurvfit(entry, exit, cens, plots = TRUE))

## Plots survival estimate

plot(fit)
