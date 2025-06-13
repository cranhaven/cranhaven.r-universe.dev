## ---- echo = FALSE, message = FALSE, warning = FALSE, results='asis'----------
library(HaDeX)
library(ggplot2)
library(knitr)
library(DT)
library(dplyr)
opts_chunk$set(fig.width = 7, fig.height = 5)
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## ----echo=FALSE,results='asis'------------------------------------------------
read.csv2("comparison.csv") %>% 
  datatable(options = list(dom = "t", ordering = FALSE, paging = FALSE), rownames = FALSE, style = "bootstrap") %>%
  formatStyle(c("MSTools", "MEMHDX", "Deuteros", "HaDeX"), backgroundColor = styleEqual(c("Yes", "No"), c("#00BFFF", "#FF8C91")))

## ----warning=FALSE, message=FALSE, echo = FALSE-------------------------------

datatable(
  data = data.frame("Column Name" = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", 
                             "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
           "Column Type" = c("Character", "Integer", "Integer", "Character", "Logic", "Logic", "Numeric", 
                             "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric")),
  rownames = FALSE, style = "bootstrap",
  list(dom = "t", ordering = FALSE, paging = FALSE, autoWidth = TRUE))


## ----warning=FALSE------------------------------------------------------------

dat <- read_hdx(system.file(package = "HaDeX", 
                            "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))


## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------

dat_temp <- read.csv(system.file(package = "HaDeX", 
                            "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))

dat_temp %>% 
  filter(File == "KD_190119_gg_Nucb2_CaCl2_10s_01", Sequence == "KQFEHLNHQNPDTFEPKDLDML", Exposure == 0.167) %>%
  select(Sequence, File, z, RT, Inten, Center)


## ----warning=FALSE------------------------------------------------------------
calc_dat <- prepare_dataset(dat,
                            in_state_first = "gg_Nucb2_EDTA_0.001",
                            chosen_state_first = "gg_Nucb2_EDTA_25",
                            out_state_first = "gg_Nucb2_EDTA_1440",
                            in_state_second = "gg_Nucb2_CaCl2_0.001",
                            chosen_state_second = "gg_Nucb2_CaCl2_25",
                            out_state_second = "gg_Nucb2_CaCl2_1440") 

## ----warning=FALSE------------------------------------------------------------
comparison_plot(calc_dat = calc_dat,
                theoretical = TRUE,
                relative = TRUE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Theoretical fraction exchanged in state comparison in 25 min time")

## ----warning=FALSE------------------------------------------------------------
comparison_plot(calc_dat = calc_dat,
                theoretical = TRUE,
                relative = FALSE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Theoretical fraction exchanged in state comparison in 25 min time")

## ----warning=FALSE------------------------------------------------------------
comparison_plot(calc_dat = calc_dat,
                theoretical = FALSE,
                relative = TRUE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Fraction exchanged in state comparison in 25 min time")

## ----warning=FALSE------------------------------------------------------------
comparison_plot(calc_dat = calc_dat,
                theoretical = FALSE,
                relative = FALSE, 
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Fraction exchanged in state comparison in 25 min time")

## ----warning=FALSE------------------------------------------------------------
woods_plot(calc_dat = calc_dat,
           theoretical = TRUE,
           relative = TRUE) +
  labs(title = "Theoretical fraction exchanged between states in 25 min time")

## ----warning=FALSE------------------------------------------------------------
woods_plot(calc_dat = calc_dat,
           theoretical = TRUE,
           relative = FALSE) +
  labs(title = "Theoretical fraction exchanged between states in 25 min time")

## ----warning=FALSE------------------------------------------------------------
woods_plot(calc_dat = calc_dat,
           theoretical = FALSE, 
           relative = TRUE) +
  labs(title = "Theoretical fraction exchanged between states in 25 min time")

## ----warning=FALSE------------------------------------------------------------
woods_plot(calc_dat = calc_dat,
           theoretical = FALSE, 
           relative = FALSE) +
  labs(title = "Theoretical fraction exchanged between states in 25 min time")

## -----------------------------------------------------------------------------
calculate_confidence_limit_values(calc_dat = calc_dat,
                                  confidence_limit = 0.99,
                                  theoretical = FALSE, 
                                  relative = TRUE)  

## -----------------------------------------------------------------------------
add_stat_dependency(calc_dat, 
                   confidence_limit = 0.98, 
                   theoretical = FALSE, 
                   relative = TRUE)

## ----warning = FALSE----------------------------------------------------------

(kin_YYDEYL_gg_Nucb2_CaCl2 <- calculate_kinetics(dat = dat, 
                                                protein = "db_Nucb2", 
                                                sequence = "YYDEYL",
                                                state = "gg_Nucb2_CaCl2", 
                                                start = 45, 
                                                end = 50, 
                                                time_in = 0.001, 
                                                time_out = 1440))

## ----warning = FALSE----------------------------------------------------------

(kin_YYDEYL_gg_Nucb2_EDTA <- calculate_kinetics(dat = dat, 
                                              protein = "db_Nucb2", 
                                              sequence = "YYDEYL",
                                              state = "gg_Nucb2_EDTA", 
                                              start = 45, 
                                              end = 50, 
                                              time_in = 0.001, 
                                              time_out = 1440))

## ----warning = FALSE----------------------------------------------------------

bind_rows(kin_YYDEYL_gg_Nucb2_CaCl2, kin_YYDEYL_gg_Nucb2_EDTA) %>%
  plot_kinetics(theoretical = TRUE, 
                relative = TRUE)


## ----warning = FALSE----------------------------------------------------------

bind_rows(kin_YYDEYL_gg_Nucb2_CaCl2, kin_YYDEYL_gg_Nucb2_EDTA) %>%
  plot_kinetics(theoretical = TRUE, 
                relative = FALSE)


## ----warning = FALSE----------------------------------------------------------

bind_rows(kin_YYDEYL_gg_Nucb2_CaCl2, kin_YYDEYL_gg_Nucb2_EDTA) %>%
  plot_kinetics(theoretical = FALSE, 
                relative = TRUE)


## ----warning = FALSE----------------------------------------------------------

bind_rows(kin_YYDEYL_gg_Nucb2_CaCl2, kin_YYDEYL_gg_Nucb2_EDTA) %>%
  plot_kinetics(theoretical = FALSE, 
                relative = FALSE)


## ----warning=FALSE------------------------------------------------------------

reconstruct_sequence(dat)


## ----warning=FALSE------------------------------------------------------------

plot_coverage(dat, chosen_state = "gg_Nucb2_CaCl2")

plot_position_frequency(dat, chosen_state = "gg_Nucb2_CaCl2")


## ----warning=FALSE------------------------------------------------------------
result <- quality_control(dat = dat,
                          state_first = "gg_Nucb2_EDTA",
                          state_second = "gg_Nucb2_CaCl2", 
                          chosen_time = 1, 
                          in_time = 0.001)


## ----warning=FALSE------------------------------------------------------------
ggplot(result) + 
  geom_line(aes(x = out_time, y = avg_err_state_first, color = "Average error (first state)")) +
  geom_line(aes(x = out_time, y = avg_err_state_second, color = "Average error (second state)")) +
  scale_x_log10() +
  labs(x = "log(time) [min]", y = "Average uncertainty", title = "Uncertainty change") + 
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

## ----warning=FALSE,echo=FALSE-------------------------------------------------
example_qc <- rbind(data.frame(x = c(10, 25, 60, 1440),
                               y = c(0.008, 0.0075, 0.007, 0.0065),
                               type = "Uncertainty decreases too slowly\nExperiment should be prolonged",
                               Assessment = "Alter experimental settings"),
                    data.frame(x = c(10, 25, 60, 1440),
                               y = c(0.008, 0.001, 0.001, 0.001),
                               type = "Uncertainty decreases too quickly\nExperiment should have more early timepoints",
                               Assessment = "Alter experimental settings"),
                    data.frame(x = c(10, 25, 60, 1440),
                               y = c(0.008, 0.004, 0.003, 0.001),
                               type = "Uncertainty decreases properly",
                               Assessment = "Experiment conducted properly"))
ggplot(example_qc, aes(x = x, y = y, color = Assessment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ type, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

## ----warning=FALSE------------------------------------------------------------
library(HaDeX)

# file import
dat_1 <- read_hdx(system.file(package = "HaDeX", 
                              "HaDeX/data/KD_180110_CD160_HVEM.csv"))

## ----warning=FALSE------------------------------------------------------------
reconstruct_sequence(dat_1)

plot_position_frequency(dat_1, chosen_state = "CD160")

## ----warning=FALSE------------------------------------------------------------
# calculate data
calc_dat_1 <- prepare_dataset(dat = dat_1,
                              in_state_first = "CD160_0.001",
                              chosen_state_first = "CD160_1",
                              out_state_first = "CD160_1440",
                              in_state_second = "CD160_HVEM_0.001",
                              chosen_state_second = "CD160_HVEM_1",
                              out_state_second = "CD160_HVEM_1440")                             

# theoretical comparison plot - relative values
comparison_plot(calc_dat = calc_dat_1,
                theoretical = TRUE,
                relative = TRUE, 
                state_first = "CD160",
                state_second = "CD160_HVEM")


## ----warning=FALSE------------------------------------------------------------
# experimental comparison plot - relative values
comparison_plot(calc_dat = calc_dat_1,
                theoretical = FALSE,
                relative = TRUE,
                state_first = "CD160",
                state_second = "CD160_HVEM")


## ----warning=FALSE------------------------------------------------------------
# theoretical comparison plot - absolute values
comparison_plot(calc_dat = calc_dat_1,
                theoretical = TRUE,
                relative = FALSE, 
                state_first = "CD160",
                state_second = "CD160_HVEM")

# experimental comparison plot - absolute values
comparison_plot(calc_dat = calc_dat_1,
                theoretical = FALSE,
                relative = FALSE,
                state_first = "CD160",
                state_second = "CD160_HVEM")



## ----warning=FALSE------------------------------------------------------------
  

# theoretical Woods plot - relative values
woods_plot(calc_dat = calc_dat_1,
           theoretical = TRUE, 
           relative = TRUE) +
  coord_cartesian(ylim = c(-.2, .2))


## ----warning=FALSE------------------------------------------------------------
# experimental Woods plot - relative values
woods_plot(calc_dat = calc_dat_1,
           theoretical = FALSE,
           relative = TRUE) +
  coord_cartesian(ylim = c(-.2, .2))


## ----warning=FALSE------------------------------------------------------------
# theoretical Woods plot - absolute values
woods_plot(calc_dat = calc_dat_1,
           theoretical = TRUE, 
           relative = FALSE) +
  labs(title = "Theoretical fraction exchanged between states in 1 min time") 

# experimental Woods plot - absolute values
woods_plot(calc_dat = calc_dat_1,
           theoretical = FALSE,
           relative = FALSE) +
  labs(title = "Fraction exchanged between states in 1 min time") 

# quality control - relative values 
(result <- quality_control(dat = dat_1,
                state_first = "CD160",
                state_second = "CD160_HVEM", 
                chosen_time = 1, 
                in_time = 0.001))

# example quality control visualisation
library(ggplot2)
ggplot(result) + 
  geom_line(aes(x = out_time, y = avg_err_state_first, color = "Average error (first state)")) +
  geom_line(aes(x = out_time, y = avg_err_state_second, color = "Average error (second state)")) +
  scale_x_log10() +
  ylim(0, 0.05) + 
  labs(x = "log(time) [min]", y = "Average uncertainty", title = "Uncertainty change in out time") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


## ----warning=FALSE------------------------------------------------------------
library(HaDeX)

# file import
dat_2 <-  read_hdx(system.file(package = "HaDeX", 
                               "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))

# protein sequence reconstruction
reconstruct_sequence(dat_2)

# calculate data
calc_dat_2 <- prepare_dataset(dat = dat_2,
                              in_state_first = "gg_Nucb2_EDTA_0.001",
                              chosen_state_first = "gg_Nucb2_EDTA_25",
                              out_state_first = "gg_Nucb2_EDTA_1440",
                              in_state_second = "gg_Nucb2_CaCl2_0.001",
                              chosen_state_second = "gg_Nucb2_CaCl2_25",
                              out_state_second = "gg_Nucb2_CaCl2_1440")                             

# theoretical comparison plot - relative values
comparison_plot(calc_dat = calc_dat_2,
                theoretical = TRUE,
                relative = TRUE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Theoretical fraction exchanged in \nstate comparison in 25 min time")

# experimental comparison plot - relative values 
comparison_plot(calc_dat = calc_dat_2,
                theoretical = FALSE,
                relative = TRUE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Fraction exchanged in \nstate comparison in 25 min time")

# theoretical comparison plot - absolute values
comparison_plot(calc_dat = calc_dat_2,
                theoretical = TRUE,
                relative = FALSE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Theoretical fraction exchanged in \nstate comparison in 25 min time")

# experimental comparison plot - absolute values 
comparison_plot(calc_dat = calc_dat_2,
                theoretical = FALSE,
                relative = FALSE,
                state_first = "Nucb2 Factor 1",
                state_second = "Nucb2 Factor 2") +
  labs(title = "Fraction exchanged in \nstate comparison in 25 min time")

# theoretical Woods plot - relative values
woods_plot(calc_dat = calc_dat_2,
           theoretical = TRUE,
           relative = TRUE) + 
  labs(title = "Theoretical fraction exchanged between states in 25 min time") +
  coord_cartesian(ylim = c(-.5, .7))

# experimental Woods plot - relative values
woods_plot(calc_dat = calc_dat_2,
           theoretical = FALSE,
           relative = TRUE) +
  labs(title = "Fraction exchanged between states in 25 min time") +
  coord_cartesian(ylim = c(-.5, .7))

# theoretical Woods plot - absolute values
woods_plot(calc_dat = calc_dat_2,
           theoretical = TRUE,
           relative = FALSE) + 
  labs(title = "Theoretical fraction exchanged between states in 25 min time") 

# experimental Woods plot - absolute values
woods_plot(calc_dat = calc_dat_2,
           theoretical = FALSE,
           relative = FALSE) +
  labs(title = "Fraction exchanged between states in 25 min time") 

# quality control
(result <- quality_control(dat = dat_2,
                           state_first = "gg_Nucb2_EDTA",
                           state_second = "gg_Nucb2_CaCl2", 
                           chosen_time = 25, 
                           in_time = 0.001))

# example quality control visualisation - relative values
library(ggplot2)
ggplot(result[result["out_time"]>=1,]) + 
  geom_line(aes(x = out_time, y = avg_err_state_first, color = "Average error (first state)")) +
  geom_line(aes(x = out_time, y = avg_err_state_second, color = "Average error (second state)")) +
  scale_x_log10() +
  labs(x = "log(time) [min]", y = "Average uncertainty", title = "Uncertainty change") + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

