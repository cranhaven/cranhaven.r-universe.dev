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

## ----sim-data-----------------------------------------------------------------
pet_data <- sim_design(
  between = list(pet = c(cat = "Cat Owners", 
                         dog = "Dog Owners")),
  n = c(4, 6),
  dv = list(happy = "Happiness Score"),
  id = list(id = "Subject ID"),
  mu = c(10, 12),
  sd = 4,
  plot = FALSE
)

## -----------------------------------------------------------------------------
cb <- codebook(pet_data)

## -----------------------------------------------------------------------------
cb

## ---- messages = FALSE--------------------------------------------------------
cb <- codebook(pet_data, return = "list")
cb

## -----------------------------------------------------------------------------
str(cb)

## -----------------------------------------------------------------------------

pet_data$followup <- sample(0:1, nrow(pet_data), TRUE)

vardesc <- list(
  dataType = list(followup = "b"),
  description = c(id = "Pet ID",
                  pet = "Pet Type",
                  followup = "Followed up 2 weeks later"
                  ),
  levels = list(pet = c(cat = "Cat Owners",
                        dog = "Dog Owners", 
                        ferret = "Ferret Owners"),
                followup = c("0" = "No", "1" = "Yes")
            )
)
cb <- codebook(pet_data, name = "pets", vardesc, return = "list")

cb

## -----------------------------------------------------------------------------
vardesc$levels$followup <- c("FALSE" = "No", "TRUE" = "Yes")
converted_data <- codebook(pet_data, "pets", vardesc, return = "data")

head(converted_data)

## -----------------------------------------------------------------------------
attr(converted_data, "codebook")

## -----------------------------------------------------------------------------
cb <- codebook(pet_data, vardesc = list(new_param = c(id = "YES")))

## -----------------------------------------------------------------------------
dat <- data.frame(
  initial = sample(LETTERS, 10)
)

dat$initial <- factor(dat$initial, levels = LETTERS, ordered = TRUE)

alevels <- paste("The letter", LETTERS)
names(alevels) <- LETTERS

codebook(dat, vardesc = list(levels = list(initial = alevels)), return = "list")

## -----------------------------------------------------------------------------
codebook(pet_data, license = "CC-BY 3.0", author = "Lisa DeBruine", source = "faux")

## -----------------------------------------------------------------------------
dat <- sim_design(plot= FALSE)

author_list <- list(
  list(
    "@type" = "Person",
    "name" = "Melissa Kline"
  ),
  list(
    "@type" = "Person",
    "name" = "Lisa DeBruine"
  )
)


codebook(dat, return = "list", 
         author = author_list,
         keywords = c("test", "demo"))

## -----------------------------------------------------------------------------

vardesc <- list(
  description = list(
    mpg = "Miles/(US) gallon",
    cyl = "Number of cylinders",
    disp = "Displacement (cu.in.)",
    hp = "Gross horsepower",
    drat = "Rear axle ratio",
    wt = "Weight (1000 lbs)",
    qsec = "1/4 mile time",
    vs = "Engine",
    am = "Transmission",
    gear = "Number of forward gears",
    carb = "Number of carburetors"
  ),
  # min and max values can be set manually or from data
  # min and max are often outside the observed range
  minValue = list(mpg = 0, cyl = min(mtcars$cyl)),
  maxValue = list(cyl = max(mtcars$cyl)),
  dataType = list(
    cyl = "integer",
    hp = "integer",
    vs = "integer",
    am = "integer", 
    gear = "integer",
    carb = "integer"
  ),
  # supply levels to mark factors
  levels = list(
    vs = c("0" = "V-shaped", "1" = "straight"),
    am = c("0" = "automatic", "1" = "manual")
  )
)

codebook(mtcars, "Motor Trend Car Road Tests", 
         vardesc, return = "list")

## ---- eval = FALSE------------------------------------------------------------
#  
#  cb <- codebook(mtcars, interactive = TRUE)
#  

