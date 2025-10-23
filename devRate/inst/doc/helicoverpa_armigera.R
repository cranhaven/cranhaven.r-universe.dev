## ----"r001"-------------------------------------------------------------------
require("devRate")

## ----"r002", fig.width=7, fig.height=4----------------------------------------
temp <- stats::rnorm(n = 90*4, mean = 25, sd = 2)
par(mar = c(4, 4, 0, 0))
plot(
  x = temp, type = "o", 
  xlab = "Time (6 hours time step)", ylab = "Temperature",
  ylim = c(15, 35)
)

## ----"r003", fig.width=7, fig.height=4----------------------------------------
set.seed(1234)

# Using Bartekova and Praslicka, 2006 model
my_model <- ha_bartekova2006()

# This model was designed for eggs, larvae and pupae, using simple linear
# regression models (following Campbell 1974).
forecastX <- devRateIBMparam(
  tempTS = temp, # the temperature time series described above
  timeStepTS = 1/4, # we  have 4 temperature values per day (every 6 hours)
  eq = list("campbell_74", "campbell_74", "campbell_74"), # the model eq for
    # the three life stages
  myParam = list(
    my_model[["model"]]$pupa, # starting with pupae
    my_model[["model"]]$egg,  # then adults and eggs
    my_model[["model"]]$larva # then larvae and the cycle goes on
  ),
  adultLifeStage = 1, # adult life stage after the first model step (pupae 
    # to adults)
  timeLayEggs = 10, # adults longevity fixed to 10 days
  numInd = 10, # 10 individuals for the simulation
  stocha = 0.05 # a bit of intra-population variability
)

## ----"r004", fig.width=7, fig.height=4----------------------------------------
sim01 <- forecastX[[1]]
hist(
  -999, # empty histogram
  xlim = c(0, max(sim01)), ylim = c(0, nrow(sim01)), 
  xlab = "Time", ylab = "Number of individuals", axes = FALSE, main = ""
)
axis(1)
axis(2, las = 1)
cycleLabels <- rep(c("adults", "larvae", "pupae"), 10)
trash <- lapply(1:ncol(sim01), function(i){
  hist(sim01[,i], add = TRUE)
  text(
    x = mean(sim01[,i]), 
    y = max(table(sim01[,i])), 
    labels = colnames(sim01)[i],
    pos = 2
  )
  text(
    x = mean(sim01[,i]), 
    y = max(table(sim01[,i]))+1, 
    labels = cycleLabels[i],
    pos = 2
  )
  
})

## ----"r005", fig.width=7, fig.height=4----------------------------------------
set.seed(1234)

my_model01 <- ha_bartekova2006(plotfig = FALSE)
my_model02 <- ha_mironidis2008_nls(plotfig = FALSE)
my_model03 <- ha_foley1981(plotfig = FALSE)

forecastX <- devRateIBMparam(
  tempTS = temp, # the temperature time series described above
  timeStepTS = 1/4, # we  have 4 temperature values per day (every 6 hours)
  eq = rep(list(
    "campbell_74", "campbell_74", "lactin2_95"
  ), 3), # the model eq for
    # the three life stages
  myParam = list(
    my_model03[["model"]]$diapausingpupae, # starting with diapausing pupae (Foley 1981)
    my_model01[["model"]]$egg,  # then adults and eggs (Bartekova 2006)
    my_model02[["model"]]$larva, # then larvae (Mironidis 2008)
    my_model03[["model"]]$nondiapausingpupae, # pupae G2 (Foley 1981)
    my_model01[["model"]]$egg,
    my_model02[["model"]]$larva,
    my_model03[["model"]]$nondiapausingpupae,
    my_model01[["model"]]$egg,
    my_model02[["model"]]$larva
  ),
  adultLifeStage = 1, # adult life stage after the first model step (pupae 
    # to adults)
  timeLayEggs = 10, # adults longevity fixed to 10 days
  numInd = 10, # 10 individuals for the simulation
  stocha = 0.05 # a bit of intra-population variability
)

## ----"r006", fig.width=7, fig.height=4----------------------------------------
sim02 <- forecastX[[1]]
hist(
  -999, # empty histogram
  xlim = c(0, max(sim02)), ylim = c(0, nrow(sim02)), 
  xlab = "Time", ylab = "Number of individuals", axes = FALSE, main = ""
)
axis(1)
axis(2, las = 1)
cycleLabels <- rep(c("adults", "larvae", "pupae"), 10)
trash <- lapply(1:ncol(sim02), function(i){
  hist(sim02[,i], add = TRUE)
  text(
    x = mean(sim02[,i]), 
    y = max(table(sim02[,i])), 
    labels = colnames(sim02)[i],
    pos = 2
  )
  text(
    x = mean(sim02[,i]), 
    y = max(table(sim02[,i]))+1, 
    labels = cycleLabels[i],
    pos = 2
  )
  
})

## ----"r007", fig.width=7, fig.height=4----------------------------------------
# results of simulations using model 1
apply(sim01, MARGIN = 2, FUN = summary)

# results of simulations using model 2
apply(sim02, MARGIN = 2, FUN = summary)

