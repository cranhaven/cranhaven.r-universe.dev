## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(sift)
library(dplyr)
library(tidyr)

comms

## -----------------------------------------------------------------------------
comms %>% 
  filter(station == "C",
         msg_code == 3060)

## -----------------------------------------------------------------------------
comms_conjecture <- conjecture(comms,     # dataset to reshape.
                               timestamp, # <dttm> friendly. must be coercible to numeric.
                               type,      # any type of atomic vector is fine.
                               "send")    # we could flip our logic and supply "receive" instead.

comms_conjecture

## -----------------------------------------------------------------------------
comms_pivot <- comms %>% 
  pivot_wider(names_from = type,
              values_from = timestamp,
              values_fn = first) %>% 
  filter(receive > send)

comms_pivot

## -----------------------------------------------------------------------------
comms_pivot %>% 
  filter(is.na(receive))

comms_conjecture %>% 
  filter(is.na(receive))

## -----------------------------------------------------------------------------
comms_pivot %>% 
  filter(station == "A",
         msg_code == 221)

comms_conjecture %>% 
  filter(station == "A",
         msg_code == 221)

## -----------------------------------------------------------------------------
comms_small <- comms %>% 
  filter(station == "A",
         msg_code == 221)

comms_small

## -----------------------------------------------------------------------------
send <- comms_small %>% filter(type == "send") %>% pull(timestamp) %>% sort()
send

receive <- comms_small %>% filter(type == "receive") %>% pull(timestamp) %>% sort()
receive

## -----------------------------------------------------------------------------
output <- integer(length = length(send))

for (i in seq_along(send)) {
  output[i] <- NA_integer_
  
  for (j in seq_along(receive)) {
    if (is.na(receive[j])) {
      next
    } else if (receive[j] > send[i]) {
      output[i] <- j
      break
    } else {
      next
    }
  }
}

tibble(send, receive = receive[output])

## -----------------------------------------------------------------------------
# from comms small
receive <- receive[3]

# rerun the algorithm
for (i in seq_along(send)) {
  output[i] <- NA_integer_
  
  for (j in seq_along(receive)) {
    if (is.na(receive[j])) {
      next
    } else if (receive[j] > send[i]) {
      output[i] <- j
      break
    } else {
      next
    }
  }
}

tibble(send, receive = receive[output])

## ----echo = FALSE-------------------------------------------------------------
tibble(send, receive = receive[c(1, NA, NA, NA)])

## -----------------------------------------------------------------------------
library(readr)
library(mopac)

mopac::express

## -----------------------------------------------------------------------------
conjecture(express, time, direction, "South") %>% 
  drop_na() # We can't assume incomplete pairs are commuting to downtown

## ----fig.keep='none'----------------------------------------------------------
library(ggplot2)

conjecture(express, time, direction, "South") %>% 
  drop_na() %>% 
  mutate(trip_length = difftime(North, South, units = "hours")) %>% 
  ggplot(aes(trip_length)) +
  geom_histogram()

## ---- echo = FALSE, fig.width=4-----------------------------------------------
library(ggplot2)

conjecture(express, time, direction, "South") %>% 
  drop_na() %>% 
  mutate(trip_length = difftime(North, South, units = "hours")) %>% 
  ggplot(aes(trip_length)) +
  geom_histogram() +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot") +
  labs(title = "Trip length distribution",
       subtitle = "Vehicles commuting downtown",
       x = "Round trip length [hours]",
       y = NULL)

