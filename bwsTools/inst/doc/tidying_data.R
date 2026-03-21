## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(bwsTools)
library(dplyr)
library(tidyr)

## ----make_data, include = FALSE-----------------------------------------------
dat1 <- tibble(
  pid = 1:3,
  
  q1_1 = c(2,  2,   2),
  q1_2 = c(NA, NA,  1),
  q1_3 = c(1,  1,  NA),
  
  q2_1 = c(1,  1,  NA),
  q2_2 = c(2,  NA,  1),
  q2_3 = c(NA, 2,   2),
  
  q3_1 = c(NA, 1,   2),
  q3_2 = c(2,  2,   1),
  q3_3 = c(1,  NA, NA),
  
  q4_1 = c(NA,  2,  2),
  q4_2 = c(2,   1,  1),
  q4_3 = c(1,  NA,  NA),
)

key <- tibble(
  q = names(dat1)[-1],
  label = c("Steak N Shake", "Shake Shack", "Whataburger",
            "Culvers", "Shake Shack", "Whataburger",
            "Steak N Shake", "Culvers", "Shake Shack",
            "Steak N Shake", "Culvers",  "Whataburger")
)

dat2 <- tibble(
  pid = 1:3,
  
  q1_i1_y = c(2,  2,   2),
  q1_i1_t = rep("Steak N Shake", 3),
  q1_i2_y = c(NA, NA,  1),
  q1_i2_t = rep("Shake Shack", 3),
  q1_i3_y = c(1,  1,  NA),
  q1_i3_t = rep("Whataburger", 3),
  
  q2_i1_y = c(1,  1,  NA),
  q2_i1_t = rep("Culvers", 3),
  q2_i2_y = c(2,  NA,  1),
  q2_i2_t = rep("Shake Shack", 3),
  q2_i3_y = c(NA, 2,   2),
  q2_i3_t = rep("Whataburger", 3),
  
  q3_i1_y = c(NA, 1,   2),
  q3_i1_t = rep("Steak N Shake", 3),
  q3_i2_y = c(2,  2,   1),
  q3_i2_t = rep("Culvers", 3),
  q3_i3_y = c(1,  NA, NA),
  q3_i3_t = rep("Shake Shack", 3),
  
  q4_i1_y = c(NA,  2,  2),
  q4_i1_t = rep("Steak N Shake", 3),
  q4_i2_y = c(2,   1,  1),
  q4_i2_t = rep("Culvers", 3),
  q4_i3_y = c(1,  NA,  NA),
  q4_i3_t = rep("Whataburger", 3),
)

## ----show_dat1----------------------------------------------------------------
dat1

## ----show_key-----------------------------------------------------------------
key

## ----dat1_indiv---------------------------------------------------------------
dat1_i <- dat1 %>% 
  # 1. collect all of the non-pid columns, where variable names are filled into
  #    the column q, and the values are in column resp
  gather("q", "resp", -pid) %>%
  mutate(
    resp = case_when(  # 2. recode resp such that
      resp == 2 ~ 1,   #    if resp is 2, make it 1
      resp == 1 ~ -1,  #    if resp is 1, make it -1
      is.na(resp) ~ 0  #    if resp is NA, make it zero
    )
  ) %>% 
  # 3. join with the key data.frame by the column q
  left_join(key, by = "q") %>% 
  # 4. separate the q column into the block number and the item number
  #    by the underscore
  separate(q, c("block", "temp"), sep = "_") %>% 
  # 5. unselect the item number, since it is no longer needed
  #    as you have the item name now
  select(-temp)

## ----show_dat1_indiv----------------------------------------------------------
dat1_i

## ----dat1_agg-----------------------------------------------------------------
dat1_a <- dat1_i %>% 
  # 1. group by the label
  group_by(label) %>% 
  # 2. summarise such that...
  summarise(
    total = n(),              # n() shows number of times the item appeared
    best = sum(resp == 1),    # sum up the number of times it was selected best
    worst = sum(resp == -1),  # and sum up the times it was selected as worst
  )

## ----it_runs------------------------------------------------------------------
res1_i <- e_bayescoring(dat1_i, "pid", "block", "label", "resp")
head(res1_i)

## ----join_back----------------------------------------------------------------
dat1 <- e_bayescoring(dat1_i, "pid", "block", "label", "resp", wide = TRUE) %>% 
  left_join(dat1, by = "pid")
head(dat1)

## ----show_dat2----------------------------------------------------------------
dat2

## ----tidy_dat2----------------------------------------------------------------
dat2_i <- dat2 %>% 
  # 1. collect all of the non-pid columns, where the column name is now called
  #    temp, and the values in those columns are now all in the value column
  gather("temp", "value", -pid) %>% 
  # 2. break apart the temp column by the underscore, so now it indicates
  #    the block in the block column, the item number in the item column,
  #    and whether or not the value refers to the label (t) or response (y)
  #    in column t_or_y
  separate(temp, c("block", "item", "t_or_y"), sep = "_") %>% 
  # 3. spread out t_or_y, filling it with the values of value
  spread(t_or_y, value) %>% 
  # 4. recode answers, just like in the above example
  mutate(
    y = case_when(
      y == 2 ~ 1,
      y == 1 ~ -1,
      is.na(y) ~ 0
    )
  ) %>% 
  # 5. remove the item number column, as it is not needed anymore
  select(-item)

## ----show dat2_i--------------------------------------------------------------
dat2_i

