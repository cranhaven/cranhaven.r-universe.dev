data(blaTum)
response <- with(blaTum, PanelSurv(id, time, count))
is.PanelSurv(response)
plot(response)
