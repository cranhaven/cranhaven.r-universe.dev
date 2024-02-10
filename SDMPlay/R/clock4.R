#' Spatial cross-validation procedure, CLOCK-4 method
#'
#'@description Cross-validation procedures aims at splitting the initial occurrence dataset into a training subset that is used to build the model and the remaining data can be lately used to test model predictions. Spatially splitting training and test datasets helps reducing the influence of data spatial aggregation on model evaluation performance (Guillaumot et al. 2019, 2021).
#'
# The CLOCK-4 method is a spatial cross-validation procedure (Guillaumot et al. 2019), where 4 triangle areas are defined, cutting the Southern Ocean circle into 4 equal areas. Three of these 4 areas contain the data that will be used to train the model, the other remaining one, the data that will be used to test the model after predictions. The areas used to train and test the model are randomly defined at each loop step.
#'
#'@usage
#'clock4(occ, bg.coords)
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
#' #See Tutorial #4 "Spatial cross-validation"




clock4 <- function(occ, bg.coords){
  #initialise vectors that are used to generate the spatial sampling structure
  random_longA <- seq(0,179,1)
  random_longB <- seq(-180,-1,1)
  random_longC <- seq(0,180,1)
  random_longD <- seq(-179,0,1)
  random_long <- c(random_longA,random_longB,random_longC,random_longD)

  # sample a number between -180 and 180 to define the random sampling transect
  tirage <- sample(seq(181,541,1),1)
  random_long_tirage <- random_long[tirage]
  random_long_tirage_A1 <- random_long[tirage+90]
  random_long_tirage_A2 <- random_long[tirage+180]
  random_long_tirage_A3 <- random_long[tirage-90]
  #random_long_tirage_A4 <- random_long[tirage-180]


  ## define training and test groups (composed of both presence and background data)
  occ.grp <- rep(NA, nrow(occ))
  bg.coords.grp <- rep(NA, nrow(bg.coords))

  ## define training and test groups (composed of both presence and background data)
  presence_tot <- occ
  background_data <- bg.coords
  training_presences.occ1 <- NA;training_presences.occ3 <-NA
  training_presences.occ1_part1 <- NA;training_presences.occ3_part1 <-NA
  training_presences.occ1_part2 <- NA;training_presences.occ3_part2 <-NA
  training_backgr.occ1 <- NA; training_backgr.occ3 <- NA
  training_backgr.occ1_part1 <- NA; training_backgr.occ3_part1 <- NA
  training_backgr.occ1_part2 <- NA; training_backgr.occ3_part2 <- NA
  training_presences.occ2 <- NA;training_presences.occ4 <- NA
  training_presences.occ2_part1 <- NA;training_presences.occ4_part1 <- NA
  training_presences.occ2_part2 <- NA;training_presences.occ4_part2 <- NA
  training_backgr.occ2 <- NA;training_backgr.occ4 <- NA
  training_backgr.occ2_part1 <- NA;training_backgr.occ4_part1 <- NA
  training_backgr.occ2_part2 <- NA;training_backgr.occ4_part2 <- NA


  #---------------------
  # TRAINING PRESENCES
  #---------------------

  ### ZONE 1 ####
  if (random_long_tirage <= 0 & random_long_tirage_A1 <=0){ # imply that A2 >= 0 and A3>=0
    training_presences.occ1 <- which(presence_tot[,1] > random_long_tirage & presence_tot[,1] < random_long_tirage_A1)
    training_backgr.occ1 <- which(background_data[,1] > random_long_tirage & background_data[,1] < random_long_tirage_A1)
  }
  if (random_long_tirage >= 0 & random_long_tirage_A1 >=0){
    training_presences.occ1 <- which(presence_tot[,1] > random_long_tirage & presence_tot[,1] < random_long_tirage_A1)
    training_backgr.occ1 <- which(background_data[,1] > random_long_tirage & background_data[,1] < random_long_tirage_A1)
  }

  if (random_long_tirage >= 0 & random_long_tirage_A1 <=0){
    training_presences.occ1_part1 <- which(presence_tot[,1] > random_long_tirage & presence_tot[,1] < 180 )
    training_presences.occ1_part2 <- which(presence_tot[,1] > -180 & presence_tot[,1] < random_long_tirage_A1 )
    training_backgr.occ1_part1 <- which(background_data[,1] > random_long_tirage & background_data[,1] < 180 )
    training_backgr.occ1_part2 <- which(background_data[,1] > -180 & background_data[,1] < random_long_tirage_A1 )
  }

  if (random_long_tirage <= 0 & random_long_tirage_A1 >=0){
    training_presences.occ1_part1 <- which(presence_tot[,1] > random_long_tirage & presence_tot[,1] < 0 )
    training_presences.occ1_part2 <- which(presence_tot[,1] > 0 & presence_tot[,1] < random_long_tirage_A1 )
    training_backgr.occ1_part1 <- which(background_data[,1] > random_long_tirage & background_data[,1] < 0 )
    training_backgr.occ1_part2 <- which(background_data[,1] > 0 & background_data[,1] < random_long_tirage_A1 )
  }

  ### ZONE 2 ####

  if (random_long_tirage_A1 <= 0 & random_long_tirage_A2 <=0){
    training_presences.occ2 <- which(presence_tot[,1] > random_long_tirage_A1 & presence_tot[,1] < random_long_tirage_A2 )
    training_backgr.occ2 <- which(background_data[,1] > random_long_tirage_A1 & background_data[,1] < random_long_tirage_A2)
  }
  if (random_long_tirage_A1 >= 0 & random_long_tirage_A2 >=0){
    training_presences.occ2 <- which(presence_tot[,1] > random_long_tirage_A1 & presence_tot[,1] < random_long_tirage_A2 )
    training_backgr.occ2 <- which(background_data[,1] > random_long_tirage_A1 & background_data[,1] < random_long_tirage_A2)
  }

  if (random_long_tirage_A1 >= 0 & random_long_tirage_A2 <=0){
    training_presences.occ2_part1 <- which(presence_tot[,1] > random_long_tirage_A1 & presence_tot[,1] < 180)
    training_presences.occ2_part2 <- which(presence_tot[,1] > -180 & presence_tot[,1] < random_long_tirage_A2)
    training_backgr.occ2_part1 <- which(background_data[,1] > random_long_tirage_A1 & background_data[,1] < 180)
    training_backgr.occ2_part2 <- which(background_data[,1] > -180 & background_data[,1] < random_long_tirage_A2)
  }

  if (random_long_tirage_A1 <= 0 & random_long_tirage_A2 >=0){
    training_presences.occ2_part1 <- which(presence_tot[,1] > random_long_tirage_A1 & presence_tot[,1] < 0 )
    training_presences.occ2_part2 <- which(presence_tot[,1] > 0 & presence_tot[,1] < random_long_tirage_A2 )
    training_backgr.occ2_part1 <- which(background_data[,1] > random_long_tirage_A1 & background_data[,1] < 0 )
    training_backgr.occ2_part2 <- which(background_data[,1] > 0 & background_data[,1] < random_long_tirage_A2 )
  }

  ### ZONE 3 ####
  if (random_long_tirage_A2 <= 0 & random_long_tirage_A3 <=0){
    training_presences.occ3 <- which(presence_tot[,1] > random_long_tirage_A2 & presence_tot[,1] < random_long_tirage_A3 )
    training_backgr.occ3 <- which(background_data[,1] > random_long_tirage_A2 & background_data[,1] < random_long_tirage_A3)
  }
  if (random_long_tirage_A2 >= 0 & random_long_tirage_A3 >=0){
    training_presences.occ3 <- which(presence_tot[,1] > random_long_tirage_A2 & presence_tot[,1] < random_long_tirage_A3 )
    training_backgr.occ3 <- which(background_data[,1] > random_long_tirage_A2 & background_data[,1] < random_long_tirage_A3)
  }

  if (random_long_tirage_A2 >= 0 & random_long_tirage_A3 <=0){
    training_presences.occ3_part1 <- which(presence_tot[,1] > random_long_tirage_A2 & presence_tot[,1] < 180 )
    training_presences.occ3_part2 <- which(presence_tot[,1] > -180 & presence_tot[,1] < random_long_tirage_A3 )
    training_backgr.occ3_part1 <- which(background_data[,1] > random_long_tirage_A2 & background_data[,1] < 180 )
    training_backgr.occ3_part2 <- which(background_data[,1] > -180 & background_data[,1] < random_long_tirage_A3 )
  }

  if (random_long_tirage_A2 <= 0 & random_long_tirage_A3 >=0){
    training_presences.occ3_part1 <- which(presence_tot[,1] > random_long_tirage_A2 & presence_tot[,1] < 0 )
    training_presences.occ3_part2 <- which(presence_tot[,1] > 0 & presence_tot[,1] < random_long_tirage_A3 )
    training_backgr.occ3_part1 <- which(background_data[,1] > random_long_tirage_A2 & background_data[,1] < 0 )
    training_backgr.occ3_part2 <- which(background_data[,1] > 0 & background_data[,1] < random_long_tirage_A3 )
  }

  ### set the groups
  training_presence_grp1_all <- as.vector(stats::na.omit(c(training_presences.occ1,training_presences.occ1_part1,training_presences.occ1_part2)))
  training_presence_grp2_all <- as.vector(stats::na.omit(c(training_presences.occ2,training_presences.occ2_part1,training_presences.occ2_part2)))
  training_presence_grp3_all <- as.vector(stats::na.omit(c(training_presences.occ3,training_presences.occ3_part1,training_presences.occ3_part2)))


  for (i in training_presence_grp1_all){
    occ.grp[i] <- 1
  }
  for (i in training_presence_grp2_all){
    occ.grp[i] <- 2
  }
  for (i in training_presence_grp3_all){
    occ.grp[i] <- 3
  }
  occ.grp[which(is.na(occ.grp))]=4


  training_backgr_grp1_all <- as.vector(stats::na.omit(c(training_backgr.occ1,training_backgr.occ1_part1,training_backgr.occ1_part2)))
  training_backgr_grp2_all <- as.vector(stats::na.omit(c(training_backgr.occ2,training_backgr.occ2_part1,training_backgr.occ2_part2)))
  training_backgr_grp3_all <- as.vector(stats::na.omit(c(training_backgr.occ3,training_backgr.occ3_part1,training_backgr.occ3_part2)))

  for (i in training_backgr_grp1_all){
    bg.coords.grp[i] <- 1
  }
  for (i in training_backgr_grp2_all){
    bg.coords.grp[i] <- 2
  }
  for (i in training_backgr_grp3_all){
    bg.coords.grp[i] <- 3
  }
  bg.coords.grp[which(is.na(bg.coords.grp))]=4


  out <- list(occ.grp=occ.grp,bg.coords.grp= bg.coords.grp, tirage=tirage)
  return(out)
}


