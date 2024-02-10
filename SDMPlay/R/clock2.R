#' Spatial cross-validation procedure, CLOCK-2 method
#'
#'@description Cross-validation procedures aims at splitting the initial occurrence dataset into a training subset that is used to build the model and the remaining data can be lately used to test model predictions. Spatially splitting training and test datasets helps reducing the influence of data spatial aggregation on model evaluation performance (Guillaumot et al. 2019, 2021).
#'
# The CLOCK-2 method is a spatial cross-validation procedure (Guillaumot et al. 2019), where the Southern Ocean circle is splitted into 2 areas. One area will contain the data that will be used to train the model, the other remaining one, the data that will be used to test the model after predictions. The areas used to train and test the model are randomly defined at each loop step.
#'
#'@usage
#'clock2(occ, bg.coords)
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
clock2 <- function(occ, bg.coords){
#initialise vectors that are used to generate the spatial sampling structure
random_long <- seq(-180,180,1)
random_long_opp <- rep(NA,361)

for (i in 1:361){
  if (random_long[i] >= 0){
    random_long_opp[random_long > 0] <- random_long[random_long > 0]-180
  } else{
    random_long_opp[random_long <= 0] <- random_long[random_long <= 0]+180
  }
}

# sample a number between -180 and 180 to define the random sampling transect
random_long_tirage <- sample(random_long,1)
random_long_tirage_t <- random_long_opp[random_long==random_long_tirage]

## define training and test groups (composed of both presence and background data)

occ.grp <- rep(NA, nrow(occ))
bg.coords.grp <- rep(NA, nrow(bg.coords))

if (random_long_tirage>0){

  for (i in 1:length(occ[,1])){
    if ((occ[i,1])<random_long_tirage &(occ[i,1])>random_long_tirage_t){
      occ.grp[i] <- 1 }
    else {
      occ.grp[i] <- 2
      }
    }

  for (i in 1:length(bg.coords[,1])){
    if ((bg.coords[i,1])<random_long_tirage &(bg.coords[i,1])>random_long_tirage_t){
      bg.coords.grp[i] <- 1 }
    else {
        bg.coords.grp[i] <- 2
    }
  }}

if (random_long_tirage<0) {
  for (i in 1:length(occ[,1])){
    if ((occ[i,1])>random_long_tirage &(occ[i,1])<random_long_tirage_t){
      occ.grp[i] <- 1 }
    else {
      occ.grp[i] <- 2
    }
  }
  for (i in 1:length(bg.coords[,1])){
    if ((bg.coords[i,1])>random_long_tirage &(bg.coords[i,1])<random_long_tirage_t){
      bg.coords.grp[i] <- 1 }
    else {
      bg.coords.grp[i] <- 2
    }
  }
}

out <- list(occ.grp=occ.grp,bg.coords.grp= bg.coords.grp)
return(out)
}
