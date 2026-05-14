## ----setup, include=FALSE-----------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, fig.height = 6, fig.width = 8, message = FALSE, warning = FALSE)


## ----load---------------------------------------------------------------------

library(ushr)

print(head(actg315raw))

## ----edit---------------------------------------------------------------------
actg315 <- actg315raw %>%
    mutate(vl = 10^log10.RNA.) %>% 
    select(id = Patid, time = Day, vl)

print(head(actg315))

## ----plotdata, fig.height = 8, fig.width = 8----------------------------------
plot_data(actg315, detection_threshold = 100)


## ----fits---------------------------------------------------------------------
model_output <- ushr(data = actg315, detection_threshold = 100, censor_value = 50)


## ----bpfits, fig.width = 6, fig.height = 4------------------------------------
plot_model(model_output, type = "biphasic", detection_threshold = 100)


## ----spfits, fig.width = 3.5, fig.height = 2.5--------------------------------
plot_model(model_output, type = "single", detection_threshold = 100)


## ----summariz-----------------------------------------------------------------
actg315_summary <- summarize_model(model_output, data = actg315, stats = TRUE)

head(actg315_summary$summary)

actg315_summary$biphasicstats

actg315_summary$singlestats

## ----CIs----------------------------------------------------------------------
head(model_output$biphasicCI) 

head(model_output$singleCI)     

## ----TTSparametric------------------------------------------------------------

TTSparametric <- get_TTS(model_output = model_output, parametric = TRUE, 
                             suppression_threshold = 100)
head(TTSparametric)

TTSparametric %>% summarize(median = median(TTS), SD = sd(TTS), N = n())

## ----TTSnonparametric---------------------------------------------------------

TTSnonparametric <- get_TTS(data = actg315, parametric = FALSE, 
                                suppression_threshold = 100, censor_value = 50)
head(TTSnonparametric)

TTSnonparametric %>% summarize(median = median(TTS), SD = sd(TTS), N = n())

## ----TTSplot, fig.width = 2, fig.height = 2-----------------------------------
plot_TTS(TTSparametric, bins = 6, textsize = 7)
plot_TTS(TTSnonparametric, bins = 6, textsize = 7)


