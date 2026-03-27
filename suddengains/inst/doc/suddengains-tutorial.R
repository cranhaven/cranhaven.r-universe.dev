## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, include = FALSE------------------------------------------------
# Load packages
library(suddengains)
library(dplyr)
library(ggplot2)
library(knitr)
library(DT)

## -----------------------------------------------------------------------------
citation("suddengains")

## ----data-bdi, echo = FALSE, screenshot.force = FALSE-------------------------

sgdata_bdi <- sgdata %>% 
    dplyr::select(id, 
                  bdi_s0:bdi_s12, 
                  bdi_fu1:bdi_fu2
                  )
    
DT::datatable(sgdata_bdi,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             fixedColumns = TRUE))

## ----data-rq, echo = FALSE, screenshot.force = FALSE--------------------------

sgdata_rq <- sgdata %>% 
    dplyr::select(id, 
                  rq_s0:rq_s12, 
                  rq_fu1:rq_fu2)
    
DT::datatable(sgdata_rq,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             fixedColumns = TRUE))

## ----select, eval = FALSE-----------------------------------------------------
#  # 1. method = "pattern"
#  select_cases(data = sgdata,
#               id_var_name = "id",
#               sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                               "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                               "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#               method = "pattern",
#               return_id_lgl = FALSE)
#  
#  # 2. method = "min_sess"
#  select_cases(data = sgdata,
#               id_var_name = "id",
#               sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                               "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                               "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#               method = "min_sess",
#               min_sess_num = 9,
#               return_id_lgl = TRUE)

## -----------------------------------------------------------------------------
sgdata_select <- select_cases(data = sgdata,
                              id_var_name = "id",
                              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                             "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                              method = "pattern",
                              return_id_lgl = FALSE) %>% 
                 dplyr::filter(sg_select == TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  # Define cut-off value for first SG criterion
#  # The sd and the reliability are specified manually
#  define_crit1_cutoff(sd = 10.5,
#                      reliability = 0.931)
#  
#  # The reliability is specified manually
#  # The sd gets calculated from variable "bdi_s0" in "sgdata"
#  define_crit1_cutoff(data_sd = sgdata$bdi_s0,
#                      reliability = 0.931)

## ---- eval = FALSE------------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s0",
#                              "bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  identify_sl(data = sgdata,
#              sg_crit1_cutoff = -7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  # This example only uses the first and second sudden gains criteria
#  # All following examples work the same for the "identify_sl()" function
#  # The argument "crit123_details = TRUE" returns details about each between session interval for each criterion.
#  # Details about the third criterion will show NAs for each between session interval because it's not being used (sg_crit3 = FALSE)
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first, second, and a modified third sudden gains criterion (sg_crit3_alpha = 0.01)
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.25,
#              sg_crit3 = TRUE,
#              sg_crit3_alpha = 0.01,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion and a modified second criterion (50%)
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = 0.50,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion
#  # Details about the second and third criterion will show NAs for each between session interval
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = NULL,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion
#  # Details about the second and third criterion will show NAs for each between session interval
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = NULL,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)

## -----------------------------------------------------------------------------
# Check interval for sudden gain using all 3 criteria
# No missing data, alpha = 0.05
check_interval(pre_values = c(32, 31, 33),
               post_values = c(5, 6, 7),
               sg_crit1_cutoff = 7,
               sg_crit2_pct = .25,
               sg_crit3 = TRUE,
               sg_crit3_alpha = .05,
               identify = "sg")

# Check interval for sudden gain using all 3 criteria
# No missing data, alpha = 0.01
check_interval(pre_values = c(32, 31, 33),
               post_values = c(5, 6, 7),
               sg_crit1_cutoff = 7,
               sg_crit2_pct = .25,
               sg_crit3 = TRUE,
               sg_crit3_alpha = .01,
               identify = "sg")

# Check intervall for sudden gain using only third criterion
# Some missing data, alpha = 0.01
check_interval(pre_values = c(NA, 31, 33),
               post_values = c(5, NA, 7),
               sg_crit1_cutoff = NULL,
               sg_crit2_pct = NULL,
               sg_crit3 = TRUE,
               sg_crit3_alpha = .01,
               identify = "sg")

# Check intervall for sudden loss using all three criteria
# Some missing data, alpha = 0.05
check_interval(pre_values = c(5, NA, 7),
               post_values = c(16, 12, 14),
               sg_crit1_cutoff = -7,
               sg_crit2_pct = .25,
               sg_crit3 = TRUE,
               sg_crit3_alpha = .05,
               identify = "sl")

## -----------------------------------------------------------------------------
bysg <- create_bysg(data = sgdata,
                    sg_crit1_cutoff = 7,
                    id_var_name = "id",
                    tx_start_var_name = "bdi_s1",
                    tx_end_var_name = "bdi_s12",
                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                    sg_measure_name = "bdi",
                    identify = "sg")

## ---- echo = FALSE, screenshot.force = FALSE----------------------------------
DT::datatable(bysg,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  fixedColumns = TRUE))

## -----------------------------------------------------------------------------
bysl <- create_bysg(data = sgdata,
                    sg_crit1_cutoff = -7,
                    id_var_name = "id",
                    tx_start_var_name = "bdi_s1",
                    tx_end_var_name = "bdi_s12",
                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                    sg_measure_name = "bdi",
                    identify = "sl")

## ---- echo = FALSE, screenshot.force = FALSE----------------------------------
DT::datatable(bysl,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  fixedColumns = TRUE))

## -----------------------------------------------------------------------------
byperson_first <- create_byperson(data = sgdata,
                                  sg_crit1_cutoff = 7,
                                  id_var_name = "id",
                                  tx_start_var_name = "bdi_s1",
                                  tx_end_var_name = "bdi_s12",
                                  sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                                  "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                                  "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                                  sg_measure_name = "bdi",
                                  identify_sg_1to2 = FALSE,
                                  multiple_sg_select = "first")

## ---- echo = FALSE------------------------------------------------------------
datatable(byperson_first,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## -----------------------------------------------------------------------------
byperson_largest <- create_byperson(data = sgdata,
                                    sg_crit1_cutoff = 7,
                                    id_var_name = "id",
                                    tx_start_var_name = "bdi_s1",
                                    tx_end_var_name = "bdi_s12",
                                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                                    sg_measure_name = "bdi",
                                    identify_sg_1to2 = FALSE,
                                    multiple_sg_select = "largest")

## ---- echo = FALSE, screenshot.force = FALSE----------------------------------
DT::datatable(byperson_largest,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## -----------------------------------------------------------------------------
# For bysg dataset select "id" and "rq" variables first
sgdata_rq <- sgdata %>% 
    dplyr::select(id, rq_s0:rq_s12)

# Join them with the sudden gains data set, here "bysg"
bysg_rq <- bysg %>%
    dplyr::left_join(sgdata_rq, by = "id")

# Extract "rq" scores around sudden gains on "bdi" in the bysg dataset
bysg_rq <- extract_values(data = bysg_rq,
                          id_var_name = "id_sg",
                          extract_var_list = c("rq_s1", "rq_s2", "rq_s3", "rq_s4", 
                                               "rq_s5", "rq_s6", "rq_s7", "rq_s8", 
                                               "rq_s9", "rq_s10", "rq_s11", "rq_s12"),
                          extract_measure_name = "rq",
                          add_to_data = TRUE)

## ---- echo = FALSE------------------------------------------------------------
datatable(bysg_rq,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## -----------------------------------------------------------------------------
# Create plot of average change in depression symptoms (BDI) around the gain
plot_sg_bdi <- plot_sg(data = bysg,
                       id_var_name = "id",
                       tx_start_var_name = "bdi_s1",
                       tx_end_var_name = "bdi_s12",
                       sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                                "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
                       ylab = "BDI", xlab = "Session",
                       colour_single = "#239b89ff")

# Create plot of average change in rumination (RQ) around depression sudden gains
plot_sg_rq <- plot_sg(data = bysg_rq,
                      id_var_name = "id",
                      tx_start_var_name = "rq_s1",
                      tx_end_var_name = "rq_s12",
                      sg_pre_post_var_list = c("sg_rq_2n", "sg_rq_1n", "sg_rq_n",
                                               "sg_rq_n1", "sg_rq_n2", "sg_rq_n3"),
                      ylab = "RQ", xlab = "Session",
                      colour_single = "#440154FF") 


# It is then possible to apply other ggplot2 functions to the plot if desired,
# e.g. y axis scale, or x axis labels ...

plot_sg_bdi <- plot_sg_bdi + 
               ggplot2::coord_cartesian(ylim = c(0, 50))

plot_sg_rq <- plot_sg_rq + 
              ggplot2::scale_x_discrete(labels = c("First", "n-2", "n-1", "n",
                                                   "n+1", "n+2", "n+3", "Last"))

## ---- fig.show = 'hold', fig.width=3.2, fig.height=3.2------------------------
plot_sg_bdi
plot_sg_rq 

## -----------------------------------------------------------------------------
# Set seed
set.seed(123)

# Duplicate data
sgdata_group <- rbind(sgdata, sgdata)

# Overwrite id variable
sgdata_group$id <- c(1:86)

# Add random group variable
sgdata_group$group <- sample(seq(from = 1, to = 2, by = 1), size = 86, replace = TRUE)

# Create byperson data set
byperson_group <- create_byperson(data = sgdata_group,
                                  sg_crit1_cutoff = 7,
                                  id_var_name = "id",
                                  tx_start_var_name = "bdi_s1",
                                  tx_end_var_name = "bdi_s12",
                                  sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
                                                  "bdi_s4", "bdi_s5", "bdi_s6",
                                                  "bdi_s7", "bdi_s8", "bdi_s9",
                                                  "bdi_s10", "bdi_s11", "bdi_s12"),
                                  sg_measure_name = "bdi",
                                  multiple_sg_select = "first")

byperson_group_select <- select_cases(data = byperson_group,
                              id_var_name = "id",
                              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                             "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                              method = "pattern",
                              return_id_lgl = FALSE) %>% 
                 dplyr::filter(sg_select == TRUE)

## -----------------------------------------------------------------------------
plot_byperson_group <- plot_sg(data = byperson_group_select,
                               id_var_name = "id",
                               tx_start_var_name = "bdi_s1",
                               tx_end_var_name = "bdi_s12",
                               sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                                        "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
                               group_var_name = "group",
                               group_levels = c(1, 2),
                               group_labels = c("Treatment A", "Treatment B"),
                               group_title = NULL,
                               colour_group = "viridis",
                               viridis_option = "B",
                               viridis_begin = 0.2,
                               viridis_end = 0.8,
                               apaish = TRUE,
                               ylab = "BDI", xlab = "Session")

## ---- fig.show = 'hold', fig.width=4.4, fig.height=3.2------------------------
plot_byperson_group

## -----------------------------------------------------------------------------
plot_trajectories_1 <- sgdata %>%
    plot_sg_trajectories(id_var = "id",
                         select_id_list = c("2", "4", "5", "9"),
                         var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                      "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                      "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                         show_id = TRUE,
                         id_label_size = 4,
                         label.padding = 0.2,
                         show_legend = FALSE,
                         colour = "viridis",
                         viridis_option = "D",
                         viridis_begin = 0,
                         viridis_end = 0.8,
                         connect_missing = FALSE,
                         scale_x_num = TRUE,
                         scale_x_num_start = 1,
                         apaish = TRUE,
                         xlab = "Session", 
                         ylab = "BDI")

## ---- fig.show = 'hold', fig.width=5.5, fig.height=4--------------------------
plot_trajectories_1

## -----------------------------------------------------------------------------
# 1. Create plot including all cases with a sudden gain at session 3
plot_trajectories_2 <- bysg %>%
    dplyr::filter(sg_session_n == 3) %>% 
    plot_sg_trajectories(id_var = "id_sg",
                         var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                      "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                      "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                         show_id = FALSE,
                         id_label_size = 4,
                         label.padding = 0.2,
                         show_legend = TRUE,
                         colour = "viridis",
                         viridis_option = "D",
                         viridis_begin = 0,
                         viridis_end = 0.8,
                         connect_missing = TRUE,
                         scale_x_num = TRUE,
                         scale_x_num_start = 1,
                         apaish = TRUE,
                         xlab = "Session", 
                         ylab = "BDI")

## ---- fig.show = 'hold', fig.width=6.5, fig.height=4--------------------------
# 1. Show all cases with a sudden gain at session 3
plot_trajectories_2

## -----------------------------------------------------------------------------
# 2. Create plot including 3 randomly selected (select_n = 3) cases who experienced 
#    more than 1 gain (dplyr::filter(sg_freq_byperson > 1))
plot_trajectories_3 <- byperson_first %>%
    dplyr::filter(sg_freq_byperson > 1) %>% 
    plot_sg_trajectories(id_var = "id_sg",
                         var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                      "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                      "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                         select_n = 3,
                         show_id = FALSE,
                         id_label_size = 4,
                         label.padding = 0.2,
                         show_legend = TRUE,
                         colour = "viridis",
                         viridis_option = "D",
                         viridis_begin = 0,
                         viridis_end = 0.8,
                         connect_missing = TRUE,
                         scale_x_num = TRUE,
                         scale_x_num_start = 1,
                         apaish = TRUE,
                         xlab = "Session", 
                         ylab = "BDI")

## ---- fig.show = 'hold', fig.width=6.5, fig.height=4--------------------------
# 2. Show 3 cases (select_n = 3) with more than 1 gain (dplyr::filter(sg_freq_byperson > 1))
plot_trajectories_3

## -----------------------------------------------------------------------------
count_intervals(data = sgdata_select,
                id_var_name = "id",
                sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
                                "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
                                "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                identify_sg_1to2 = FALSE)

## ---- fig.show = 'hold', fig.width=6.5, fig.height=6--------------------------
plot_sg_intervals(data = sgdata_select,
                  id_var_name = "id",
                  sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
                                  "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
                                  "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"))

## -----------------------------------------------------------------------------
# Describe bysg dataset ----
describe_sg(data = bysg, 
            sg_data_structure = "bysg")

# Describe byperson dataset ----
describe_sg(data = byperson_first, 
            sg_data_structure = "byperson")

