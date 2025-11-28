## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.height = 5, fig.width = 10, fig.align = 'center')

## ---- message=FALSE, warning=FALSE--------------------------------------------
# Package
library(letsR)

# Data
data("PAM")

names <- PAM$Species_name[1:20] # keep only the first 20 names
PAM_subset <- lets.subsetPAM(PAM, names)
par(mfrow = c(1, 2))
plot(PAM, main = "All species")
plot(PAM_subset, main = "Subset")

## -----------------------------------------------------------------------------
data(wrld_simpl)  # World map
data(PAM)
Brazil <- wrld_simpl[wrld_simpl$NAME == "Brazil", ]  # Brazil (polygon)
PAM_crop <- lets.pamcrop(PAM, Brazil, remove.sp = TRUE)
par(mfrow = c(1, 2))
plot(PAM, main = "South America")
plot(PAM_crop, xlab = "Longitude", ylab = "Latitude",
     main = "Phyllomedusa species richness (Brazil crop)")
plot(sf::st_geometry(wrld_simpl), add = TRUE)


