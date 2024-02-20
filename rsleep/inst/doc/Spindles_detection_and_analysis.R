## ----include = FALSE----------------------------------------------------------

options(scipen=999)

options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----edf_read, echo=FALSE, message=FALSE, warning=FALSE-----------------------

library(edfReader)

if(!file.exists("15012016HD.edf")){
  download.file(
  url = "https://rsleep.org/data/15012016HD.edf",
  destfile = "15012016HD.edf",
  method = "curl")}

h <- readEdfHeader("15012016HD.edf")

s <- readEdfSignals(h, signals = "C3-M2")

file.remove("15012016HD.edf")


## ----hypnogram, echo=FALSE, message=FALSE, warning=FALSE----------------------

library(rsleep)

download.file(
  url = "https://rsleep.org/data/15012016HD.csv",
  destfile = "15012016HD.csv",
  method = "curl")

events <- read_events_noxturnal("15012016HD.csv")

file.remove("15012016HD.csv")


## ----plot_hypnogram, fig.width = 7, fig.height = 2----------------------------

plot_hypnogram(events)


## -----------------------------------------------------------------------------

n2_epoch = events[events$event == "N2",][10,]


## ----epoch_boundaries---------------------------------------------------------

n2_epoch_index_start = as.numeric(
  difftime(n2_epoch$begin, s$startTime, units = "secs")) * s$sRate + 1

n2_epoch_index_end = as.numeric(
  difftime(n2_epoch$end, s$startTime, units = "secs")) * s$sRate


## -----------------------------------------------------------------------------

eeg = s$signal[n2_epoch_index_start:n2_epoch_index_end]

eeg = eeg * 1000000


## ----warning=FALSE------------------------------------------------------------

result = a7(
  x = eeg,
  s$sRate)
  

## ----include=FALSE------------------------------------------------------------

knitr::kable(result$spindles[1:3,])


## ----fig.width = 7, fig.height = 2--------------------------------------------

library(ggplot2)

data = data.frame(x=eeg,index=seq_along(eeg))
a = result$spindles$idxStart[2]
b = result$spindles$idxEnd[2]
data = data[(data$index <= (b+600)) & (data$index >= (a-600)), ]

ggplot(data, aes(x = index, y = x)) +
  geom_line() +
  geom_line(data = subset(data, index >= a & index <= b), aes(x = index, y = x), color = "red") +
  labs(x = "Signal index", y = "C3-M2") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------

n2_epochs = events[events$event == "N2",]

epochs_results = list()

for(i in c(1:nrow(n2_epochs))){
  
  n2_epoch_index_start = as.numeric(
    difftime(n2_epochs$begin[i],s$startTime,units = "secs")) * s$sRate + 1

  n2_epoch_index_end = as.numeric(
    difftime(n2_epochs$end[i],s$startTime,units = "secs")) * s$sRate

  eeg = s$signal[n2_epoch_index_start:n2_epoch_index_end]

  eeg = eeg * 1000000
  
  results = rsleep::a7(
    x = eeg,
    s$sRate)
  
  epochs_results[[length(epochs_results)+1]] = results
  
}


## -----------------------------------------------------------------------------

spindles = dplyr::bind_rows(
  lapply(c(1:length(epochs_results)), function(x){
    epochs_results[[x]]$spindles$epoch = x
    epochs_results[[x]]$spindles
}))


## ----fig.width = 7, fig.height = 2--------------------------------------------

library(ggplot2)

ggplot(spindles, aes(x = epoch)) +
  geom_bar() +
  xlab("N2 epoch number") +
  ylab("Detected spindles")


