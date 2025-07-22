## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE, message = FALSE---------------------------------------------
library(processanimateR)
library(dplyr)
library(bupaR)

# Extract only the lacticacid measurements
lactic <- sepsis %>%
  mutate(lacticacid = as.numeric(lacticacid)) %>%
  arrange(case_id, timestamp) %>% 
  as.data.frame() %>%
  bupaR::fill(lacticacid, .direction = "up") %>% 
  mutate("act" = activity,
         value = lacticacid) %>% 
  as.data.frame() %>%
  mutate(time = lubridate::floor_date(timestamp, "30 mins")) %>% 
  group_by(act, time) %>% 
  summarise(value = mean(value), .groups = "drop") %>% 
  select(act,
         time,
         value) # format needs to be 'act,time,value'

# Remove the measurement events from the sepsis log
sepsisBase <- sepsis %>%
  filter_activity(c("LacticAcid", "CRP", "Leucocytes", "Return ER",
                    "IV Liquid", "IV Antibiotics"), reverse = T) %>%
  filter_trace_frequency(percentage = 0.95)

# Animate activity aesthetics with the secondary data frame `lactic`
animate_process(sepsisBase, 
                mode = "absolute", 
                duration = 300,
                mapping_activity = activity_aes(color = 
                                                  activity_scale(lactic, 
                                                                 scale = "linear", 
                                                                 range = c("#fff5eb","#7f2704"))))


