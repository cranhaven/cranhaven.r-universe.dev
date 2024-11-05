## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(faux)

## ----plot-sim-design----------------------------------------------------------
between <- list(pet = c(cat = "Cat Owners", 
                        dog = "Dog Owners"))
within <- list(time = c("morning", "noon", "evening", "night"))
mu <- data.frame(
  cat    = c(10, 12, 14, 16),
  dog    = c(10, 15, 20, 25),
  row.names = within$time
)
# add factor labels for plotting
vardesc <- c(pet = "Type of Pet",
             time = "Time of Day")

df <- sim_design(within, between, 
                 n = 100, mu = mu, sd = 5, r = .5,
                 empirical = TRUE, vardesc = vardesc, plot = TRUE)

## ----anon---------------------------------------------------------------------
df <- sim_design(within = c(2,3), between = c(2), 
                 n = 10, mu = 1:12, sd = 1, r = 0.5)

## ----vlist--------------------------------------------------------------------
between <- list(
  pet = c("cat", "dog")
)
within <- list(
  time = c("day", "night")
)

df <- sim_design(within, between, mu = 1:4)

## -----------------------------------------------------------------------------
faux_options(sep = ".")

sim_design(within, n = 5, plot = FALSE)

## -----------------------------------------------------------------------------
# put the separator back to _ for the rest of this vignette
faux_options(sep = "_")

## ----named-vectors------------------------------------------------------------
between <- list(
  pet = c(cat = "Is a cat person", dog = "Is a dog person")
)
within <- list(
  time = c(day = "Tested during the day", night = "Tested at night")
)
df <- sim_design(within, between, mu = 1:4)

## -----------------------------------------------------------------------------
vardesc <- c(pet = "Type of Pet",
             time = "Time of Day")

df <- sim_design(within, between, mu = 1:4, 
                 id = c(pet_id = "Pet ID"),
                 dv = c(score = "Score on the Test"),
                 vardesc = vardesc)

## -----------------------------------------------------------------------------
wide2long(df) %>% head()

## -----------------------------------------------------------------------------
n <- 20 # n per cell, not total
design <- check_design(2, c(2,2), n = n, plot = FALSE)
str(design$n)

## -----------------------------------------------------------------------------
n <- list(
  B1a_B2a = 10, 
  B1a_B2b = 20, 
  B1b_B2a = 30, 
  B1b_B2b = 40
)
design <- check_design(2, c(2,2), n = n, plot = FALSE)
str(design$n)

## -----------------------------------------------------------------------------
n <- data.frame(
  B1b_B2b = 40,
  B1a_B2a = 10, 
  B1a_B2b = 20, 
  B1b_B2a = 30
)
design <- check_design(2, c(2,2), n = n, plot = FALSE)
str(design$n)

## -----------------------------------------------------------------------------
n <- data.frame(n = c(10, 20, 30, 40),
                row.names = c("B1a_B2a", "B1a_B2b", "B1b_B2a", "B1b_B2b"))
design <- check_design(2, c(2,2), n = n, plot = FALSE)
str(design$n)

## -----------------------------------------------------------------------------
between <- list(pet       = c("cat", "dog"), 
                condition = c("A",   "B"))
within  <- list(time      = c("day", "night"))
mu <- c(10, 20, 30, 40, 50, 60, 70, 80)
design <- check_design(within, between, mu = mu, plot = FALSE)
str(design$mu)

## -----------------------------------------------------------------------------
mu <- list(
  cat_B = c(night = 40, day = 30),
  cat_A = c(day = 10, night = 20),
  dog_A = c(day = 50, night = 60),
  dog_B = c(day = 70, night = 80)
)
design <- check_design(within, between, mu = mu, sd = 1, plot = FALSE)
str(design$mu)

## -----------------------------------------------------------------------------
mu <- data.frame(
  cat_A = c(10, 20),
  cat_B = c(30, 40),
  dog_A = c(50, 60),
  dog_B = c(70, 80),
  row.names = c("day", "night")
)
design <- check_design(within, between, mu = mu, plot = FALSE)
str(design$mu)

## -----------------------------------------------------------------------------
mu <- data.frame(
  day = c(10, 30, 50, 70),
  night = c(20, 40, 60, 80),
  row.names = c("cat_A", "cat_B", "dog_A", "dog_B")
)
design <- check_design(within, between, mu = mu, plot = FALSE)
str(design$mu)

## -----------------------------------------------------------------------------
r <- list(
  cat_A = .5,
  cat_B = .5,
  dog_A = .6,
  dog_B = .4
)
design <- check_design(within, between, r = r, plot = FALSE)
design$r

## -----------------------------------------------------------------------------
r <- list(
  B1a = c(.10, .20, .30, 
              .40, .50, 
                   .60),
  B1b = c(.15, .25, .35, 
              .45, .55, 
                   .65)
)
design <- check_design(4, 2, r = r, plot = FALSE)
design$r

## -----------------------------------------------------------------------------
within <- list(cars = c("speed", "dist"))
between <- list(half = c("first", "last"))
r <- list(
  first = cor(cars[1:25,]),
  last = cor(cars[26:50,])
)

design <- check_design(within, between, r = r, plot = FALSE)
design$r

## ----empirical----------------------------------------------------------------
between <- list(pet  = c("cat", "dog"))
within  <- list(time = c("day", "night"))
mu <- list(
  cat = c(day = 10, night = 20),
  dog = c(day = 30, night = 40)
)

sd <- list(
  cat = c(day = 5, night = 10),
  dog = c(day = 15, night = 20)
)

r <- list(cat = .5, dog = .6)

df <- sim_design(within, between, n = 100, 
                 mu = mu, sd = sd, r = r,
                 empirical = TRUE)

## -----------------------------------------------------------------------------
within <- list(
  condition = c(con = "Mean of congruent trials", 
                inc = "Mean of incongruent trials"),
  version = c(easy = "Easy", 
              med  = "Medium", 
              hard = "Difficult")
)

between <- list(
  experience = c(novice = "Novice", expert = "Expert"),
  time = c(day = "Before 5pm", night = "After 5pm")
)

mu <- data.frame(
  row.names = c("con_easy", "con_med", "con_hard",
                "inc_easy", "inc_med",  "inc_hard"),
  novice_day = 10:15,
  novice_night = 11:16,
  expert_day = 9:14,
  expert_night = 10:15
)

## -----------------------------------------------------------------------------
r <- list(
  novice_day = 0.3,
  novice_night = 0.2,
  expert_day = 0.5,
  expert_night = 0.4
)

## -----------------------------------------------------------------------------
# upper right triangle correlation specification
# inc and con have r = 0.5 within each difficultly level, 0.2 otherwise
#          ce,  ie,  cm,  im,  ch,  ih
triangle <-  c(0.5, 0.2, 0.2, 0.2, 0.2, #con_easy
                    0.2, 0.2, 0.2, 0.2, #inc_easy
                         0.5, 0.2, 0.2, #con_med
                              0.2, 0.2, #inc_med
                                   0.5) #con_hard
                                        #inc_hard

r <- list(
  novice_day = triangle,
  novice_night = triangle,
  expert_day = triangle,
  expert_night = triangle
)

## -----------------------------------------------------------------------------
df <- sim_design(within, between, n = 100, 
                 mu = mu, sd = 2, r = r, 
                 dv = c(rt = "Reaction Time"), 
                 plot = FALSE, long = TRUE)

head(df)

## -----------------------------------------------------------------------------
df <- sim_design(within = 2, between = 2, 
                 n = 50, mu = c(1, 1, 1, 1.5), 
                 sd = 1, r = 0.5, plot = FALSE, 
                 long = TRUE, rep = 5)

df

## -----------------------------------------------------------------------------
# using tidyverse functions
analyse <- function(data) {
  stats::aov(y ~ B1 * W1 + Error(id/W1), data = data) %>% 
    broom::tidy()
}

df %>%
  dplyr::mutate(analysis = lapply(data, analyse)) %>%
  dplyr::select(-data) %>%
  tidyr::unnest(analysis)

## -----------------------------------------------------------------------------
df <- sim_design(within = 2, between = 2, 
                 n = 2, mu = c(1, 1, 1, 1.5), 
                 sd = 1, r = 0.5, plot = FALSE, 
                 long = TRUE, rep = 2, nested = FALSE)

df

## ---- results='asis'----------------------------------------------------------
between <- list(pet = c("cat", "dog"))
within <- list(time = c("day", "night"))
vardesc <- c(pet = "Type of Pet",
             time = "Time of Day")
design <- check_design(within, between, n = 10, 
                       mu = 1:4, sd = 1:4, r = 0.5, 
                       vardesc = vardesc, plot = FALSE)

design

## -----------------------------------------------------------------------------
data <- sim_design(design = design)

