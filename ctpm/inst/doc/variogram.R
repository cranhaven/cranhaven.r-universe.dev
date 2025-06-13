## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=6, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
# Load in the ctpm package
library(ctpm)

#####
#Load in the trait data and phylogenetic tree
data("moid_traits")
data("musteloids")

#Plot the trait data on a log10 - log10 scale
plot(log10(Mass.M) ~ log10(Mass.F),
     data = moid_traits,
     xlab = "Female mass (log10(kg))",
     ylab = "Male mass (log10(kg))")

## -----------------------------------------------------------------------------
#Plot the phylogenetic tree
plot(musteloids,
     cex = 0.5)

## -----------------------------------------------------------------------------
#Create a vector of a colour gradient of the same length as the number of species in the dataset
COLS <- viridis::viridis(nrow(moid_traits))

#Plot the phylogenetic tree with tip labels coloured by SSD values
plot(musteloids,
     tip.color=COLS[order(moid_traits$SSD)],
     cex = 0.45)

## -----------------------------------------------------------------------------
SSD <- moid_traits$SSD
names(SSD) <- moid_traits$Binomial

#Check that the species are correctly lined up
names(SSD) == musteloids$tip.label

#Calculate and plot the variogram
SVF <- variogram(moid_traits$SSD, musteloids, progress = F)
plot(SVF)

## -----------------------------------------------------------------------------
#Fit the evolutionary models
IID.FIT <- ctpm.fit(SSD, musteloids, model = "IID")
BM.FIT <- ctpm.fit(SSD, musteloids, model = "BM")

#AIC based model selection
IID.FIT$AIC
BM.FIT$AIC

##############
# Now plot the variograms and fitted models
plot(SVF,
     list(IID.FIT,
          BM.FIT),
     col.CTPM = c("red",
                  "#046C9A"))
legend("topleft",
       fill = c("red",
                "#046C9A"),
       legend = c("IID",
                  "BM"),
       horiz = T,
       cex = 0.8)

## -----------------------------------------------------------------------------

# #Fit the evolutionary models
# IID.FIT <- ctpm.fit(SSD, musteloids, model = "IID")
# BM.FIT <- ctpm.fit(SSD, musteloids, model = "BM")
# OU.FIT <- ctpm.fit(SSD, musteloids, model = "OU")
# 
# #AIC based model selection
# OU.FIT$AIC
# IID.FIT$AIC
# BM.FIT$AIC
# 
# ##############
# # Now plot the variograms and fitted models
# plot(SVF,
#      list(IID.FIT,
#           BM.FIT,
#           OU.FIT),
#      col.CTPM = c("red",
#                   "purple",
#                   "#046C9A"))
# legend("topleft",
#        fill = c("red",
#                 "purple",
#                 "#046C9A"),
#        legend = c("IID",
#                   "BM",
#                   "OU"),
#        horiz = T,
#        cex = 0.8)

