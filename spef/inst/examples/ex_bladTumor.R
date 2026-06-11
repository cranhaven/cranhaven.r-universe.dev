data(bladTumor)
## Plot bladder tumor data
p <- plot(with(bladTumor, PanelSurv(subject, time, count2)))
print(p)
