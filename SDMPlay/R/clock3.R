#' Spatial cross-validation procedure, CLOCK-3 method
#'
#'@description Cross-validation procedures aims at splitting the initial occurrence dataset into a training subset that is used to build the model and the remaining data can be lately used to test model predictions. Spatially splitting training and test datasets helps reducing the influence of data spatial aggregation on model evaluation performance (Guillaumot et al. 2019, 2021).
#'
# The CLOCK-3 method is a spatial cross-validation procedure (Guillaumot et al. 2019), where 3 triangle areas are defined, cutting the Southern Ocean circle into 3 equal areas. Three of these 3 areas contain the data that will be used to train the model, the other remaining one, the data that will be used to test the model after predictions. The areas used to train and test the model are randomly defined at each loop step.
#'
#'@usage
#'clock3(occ, bg.coords)
#'
#'@param occ Dataframe with longitude (column 1) and latitude (column 2) of the presence-only data. Decimal longitude and latitude are required.
#'
#'@param bg.coords Dataframe with longitude (column 1) and latitude (column 2) of the sampled background records. Decimal longitude and latitude are required.

#'@details
#'See Guillaumot et al.(2019) and vignette tutorial #4 "Spatial cross-validation" for complete examples and details.
#'
#'@return
#' A list that details the group to which each data (presence or background record) belongs to; and the detail of the random longitude data that was sampled to initiate the CLOCK scheme.
#' list(occ.grp=occ.grp,bg.coords.grp= bg.coords.grp, tirage)
#'
#'@references
#'Guillaumot C, Artois J, Saucède T, Demoustier L, Moreau C, Eléaume M, Agüera A, Danis B (2019). Broad-scale species distribution models applied to data-poor areas. Progress in Oceanography, 175, 198-207.
#'
#'Guillaumot C, Danis B, Saucède T (2021). Species Distribution Modelling of the Southern Ocean : methods, main limits and some solutions. Antarctic Science.
#'
#'@examples
#'#See Tutorial #4 "Spatial cross-validation"
#'
#'
clock3 <- function(occ, bg.coords){

  #initialise vectors that are used to generate the spatial sampling structure
  random_longA <- seq(0,179,1)
  random_longB <- seq(-180,-1,1)
  random_longC <- seq(0,180,1)
  random_longD <- seq(-179,0,1)
  random_long <- c(random_longA,random_longB,random_longC,random_longD)

  # sample a number between -180 and 180 to define the random sampling transect
  tirage <- sample(seq(181,541,1),1)
  random_long_tirage <- random_long[tirage]
  random_long_tirage_right <- random_long[tirage+120]
  random_long_tirage_left <- random_long[tirage-120]

## define training and test groups (composed of both presence and background data)
occ.grp <- rep(NA, nrow(occ))
bg.coords.grp <- rep(NA, nrow(bg.coords))


## define training and test groups (composed of both presence and background data)
presence_tot <- occ
training_presences.occ <- NA;training_presences.occ_left <-NA;training_presences.occ_right <-NA
training_backgr.occ <- NA; training_backgr.occ_left <- NA;training_backgr.occ_right <-NA
test_presences.occ <- NA; training_presences.occ_inf_left <- NA; training_presences.occ_inf_right <- NA; training_presences.occ_supp_left <- NA; training_presences.occ_supp_right <- NA;training_backgr.occ_supp_left <- NA; training_backgr.occ_supp_right <- NA; training_backgr.occ_inf_left <- NA; training_backgr.occ_inf_right <- NA;
background_data <- bg.coords

# training presences
#---------------------
if (random_long_tirage_left < 0){
  training_presences.occ_left <- which(presence_tot[,1] > random_long_tirage_left & presence_tot[,1] < 0 )
} else {
  training_presences.occ_left <- which(presence_tot[,1] > random_long_tirage_left)
}

if(random_long_tirage_right>0){
  training_presences.occ_right <- which(presence_tot[,1] <random_long_tirage_right & presence_tot[,1] > 0)
} else {
  training_presences.occ_right <- which(presence_tot[,1] <random_long_tirage_right)
}

if(random_long_tirage_right>0 & random_long_tirage_left>0){
  training_presences.occ_supp_left <- which(presence_tot[,1] <random_long_tirage & presence_tot[,1] > -179)
  training_presences.occ_supp_right <- which(presence_tot[,1] >random_long_tirage & presence_tot[,1] < 0)
}

if(random_long_tirage_right<0 & random_long_tirage_left<0){
  training_presences.occ_inf_left <- which(presence_tot[,1] <random_long_tirage & presence_tot[,1] > 0)
  training_presences.occ_inf_right <- which(presence_tot[,1] >random_long_tirage & presence_tot[,1] < 180)
}

training_presence_left_all <- c(training_presences.occ_left,training_presences.occ_inf_left,training_presences.occ_supp_left)
training_presence_right_all <- c(training_presences.occ_right,training_presences.occ_inf_right,training_presences.occ_supp_right)

for (i in training_presence_left_all){
  occ.grp[i] <- 1
}
for (i in training_presence_right_all){
  occ.grp[i] <- 2
}

occ.grp[which(is.na(occ.grp))]=3

# Background
#---------------------
if (random_long_tirage_left < 0){
  training_backgr.occ_left <- which(background_data[,1] > random_long_tirage_left & background_data[,1] < 0 )
} else {
  training_backgr.occ_left <- which(background_data[,1] > random_long_tirage_left)
}

if(random_long_tirage_right>0){
  training_backgr.occ_right <- which(background_data[,1] <random_long_tirage_right & background_data[,1] > 0)
} else {
  training_backgr.occ_right <- which(background_data[,1] <random_long_tirage_right)
}

if(random_long_tirage_right>0 & random_long_tirage_left>0){
  training_backgr.occ_supp_left <- which(background_data[,1] <random_long_tirage & background_data[,1] > -179)
  training_backgr.occ_supp_right <- which(background_data[,1] >random_long_tirage & background_data[,1] < 0)
}

if(random_long_tirage_right<0 & random_long_tirage_left<0){
  training_backgr.occ_inf_left <- which(background_data[,1] <random_long_tirage & background_data[,1] > 0)
  training_backgr.occ_inf_right <- which(background_data[,1] >random_long_tirage & background_data[,1] < 180)
}

training_backgr_left_all <- c(training_backgr.occ_left,training_backgr.occ_inf_left,training_backgr.occ_supp_left)
training_backgr_right_all <- c(training_backgr.occ_right,training_backgr.occ_inf_right,training_backgr.occ_supp_right)

for (i in training_backgr_left_all){
  bg.coords.grp[i] <- 1
}
for (i in training_backgr_right_all){
  bg.coords.grp[i] <- 2
}

bg.coords.grp[which(is.na(bg.coords.grp))]=3

out <- list(occ.grp=occ.grp,bg.coords.grp= bg.coords.grp)
return(out)
}
