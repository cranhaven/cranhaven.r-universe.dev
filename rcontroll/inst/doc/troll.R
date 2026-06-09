## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 8,
  fig.width = 8
)

## ----traits, echo=FALSE-------------------------------------------------------
table <- data.frame(
  Abbreviation = c(
    "$LMA$", "$N_m$", "$P_m$", "$wsg$",
    "$dbh_{thresh}$", "$h_{lim}$", "$a_h$"
  ),
  Description = c(
    "leaf mass per area",
    "leaf nitrogen content per dry mass",
    "leaf phosphorus content per dry mass",
    "wood specific gravity",
    "diameter at breast height threshold",
    "asymptotic height",
    "parameter of the tree-height-dbh allometry"
  ),
  Units = c(
    "$g.m^{-2}$", "$mg.g^{-1}$", "$mg.g^{-1}$",
    "$g.cm^{-3}$", "$m$", "$m$", "$m$"
  )
)
knitr::kable(table, caption = "Species-specific parameters used in
             TROLL from Maréchaux & Chave (2017). Data originates
             from the BRIDGE (Baraloto et al. 2010) and
             TRY (Kattge et al. 2011) datasets.", format = "pandoc")

