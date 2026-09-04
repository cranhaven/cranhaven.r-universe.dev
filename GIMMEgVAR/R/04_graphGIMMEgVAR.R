#' GIMMEgVAR: GraphGIMMEgVAR
#'
#' Produces network graphs for beta and kappa matrices
#' as .png files.
#'
#' Group and individual network paths are overlayed to form
#' a single network graph for each matrix, respectively.
#'
#' Kappa matrix is depicted using solid lines, beta network is depicted using dashed lines.
#' In both networks, group paths appear as black lines. Individual paths appear as light grey lines.
#'
#' For individual paths, line thickness is weighted by the proportion of individuals with paths
#' present.
#'
#' @param outputPathGIMMEgVAR Directory where network graphs will be saved.
#' @param gimmeGVARThreshold The cutoff value for group-level paths. Defaults to .50, indicating
#'                           that a path must be non-zero across >= .50% of individuals to be
#'                           included as a group-level path.
#' @param pathProportionKappa The proportion of individuals identified as having a path in the Kappa
#'                            matrix.  Used to determine group-level paths in the final group Kappa network.
#' @param groupKappa Matrix that identifies the presence of a group path in the Kappa matrix. 0 indicates no
#'                   group path present, 1 indicates group path present.
#' @param lableNames Vector of names used to label nodes in Network graph. Defaults to variable names if no
#'                   vector is supplied.
#' @importFrom qgraph qgraph
#' @importFrom png readPNG
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot
#' @importFrom graphics rasterImage

#-------------
#kappa graphs
#-------------


graphKappa <- function(outputPathGIMMEgVAR,
                       gimmeGVARThreshold,
                       pathProportionKappa,
                       lableNames,
                       groupKappa
                       )
  {

    #get user's original options
    originalPar <- par(no.readonly = TRUE)

    #set options back to original
    on.exit(par(originalPar))

    #STEP1: PREPARE KAPPA DATA

    # create indicator of individual paths for kappa with diagonal set to 0
    kappaIndividual <- as.data.frame(ifelse(pathProportionKappa < gimmeGVARThreshold, pathProportionKappa, 0))
    diag(kappaIndividual) <- 0

    # create individual Kappa plot
    kappaIndividualGraph <- qgraph(kappaIndividual,directed=FALSE,layout="circle",edge.labels=FALSE,weighted=TRUE, edge.color="grey22",vTrans=0,label.scale=FALSE,esize=2,
                                   bg="transparent", labels=lableNames)

    # create group Kappa plot
    kappaGroupGraph <- qgraph(groupKappa,directed=FALSE,layout="circle",edge.labels=FALSE,esize=5,weighted=TRUE, edge.color="black", directed=FALSE, diag=FALSE, label.scale=FALSE,
                         labels=lableNames)

    #STEP2: PLOT OVERLAYED KAPPA NETWORKS

    # create overlay image
    tmpFile1 <- here(outputPathGIMMEgVAR,"k1Ind.png")
    png(filename = tmpFile1,bg ="transparent")
    plot(kappaIndividualGraph)
    dev.off()

    # create background image
    tmpFile2 <- here(outputPathGIMMEgVAR,"k1Grp.png")
    png(filename = tmpFile2) #create image
    plot(kappaGroupGraph)
    dev.off()

    # Read in png images
    backGrndK = readPNG(here(outputPathGIMMEgVAR,"k1Grp.png"))
    overImgK = readPNG(here(outputPathGIMMEgVAR,"k1Ind.png"))

    # Now overlay the two images,  first the background file, then the overlay file
    finalKappaNetwork <- here(outputPathGIMMEgVAR,"KappaNetwork.png")
    png(filename = finalKappaNetwork)
    par(mai=c(0,0,0,0))
    plot.new()

    rasterImage(backGrndK, 0, 0, 1, 1)
    rasterImage(overImgK, 0, 0, 1, 1)
    dev.off()

    # clean up by removing the temporary png files used to create combined image
    file.remove(tmpFile1)
    file.remove(tmpFile2)


}

#-------------
#beta graphs
#-------------

graphBeta <- function(outputPathGIMMEgVAR,
                      gimmeGVARThreshold,
                      pathProportionBeta,
                      lableNames,
                      groupBeta
                      ){

  #STEP1: PREPARE BETA DATA

  # beta individual
  betaIndividual <- ifelse(pathProportionBeta < gimmeGVARThreshold, pathProportionBeta , 0)#change group paths to 0
  betaIndividual <- as.data.frame(betaIndividual[,-1])#drop vector of 1's used in estimation


  # create individual Beta plot
  #note: this graph is transparent for overlaying
  betaIndividualGraph <- qgraph(betaIndividual,directed=TRUE,layout="circle",edge.labels=FALSE,esize=4, edge.color="grey41", lty=3,diag=FALSE, vTrans=0,label.scale=FALSE,
                                weighted=TRUE,bg="transparent", labels=lableNames)#graph beta

  # create group Beta
  betaGroup <- as.data.frame(groupBeta[,-1])#drop vector of 1's used in estimation
  betaGroupGraph <- qgraph(betaGroup,directed=TRUE,layout="circle",edge.labels=FALSE,esize=5, edge.color="black", lty=2, directed=TRUE, diag=TRUE, label.scale=FALSE,weighted=TRUE,
                           labels=lableNames)#graph betaplot(b1Grp)

  #STEP2: PLOT OVERLAYED BETA NETWORKS

  # create overlay image
  tmpFile3 <- here(outputPathGIMMEgVAR,"b1Ind.png")
  png(filename = tmpFile3,bg = "transparent")
  plot(betaIndividualGraph)
  dev.off()

  # create background image
  tmpFile4 <- here(outputPathGIMMEgVAR,"b1Grp.png")
  png(filename = tmpFile4)
  plot(betaGroupGraph)
  dev.off()

  # Read in image background and overlay images
  backGrndB = readPNG(here(outputPathGIMMEgVAR,"b1Grp.png"))
  overImgB = readPNG(here(outputPathGIMMEgVAR,"b1Ind.png"))

  # Now overlay the two images,  first the background file, then the overlay file
  finalBetaNetwork <- here(outputPathGIMMEgVAR,"BetaNetwork.png")
  png(filename = finalBetaNetwork)

  par(mai=c(0,0,0,0))
  plot.new()

  rasterImage(backGrndB, 0, 0, 1, 1)
  rasterImage(overImgB, 0,0,1,1)
  dev.off()

  #clean up by removing the temporary png files used to create combined image
  file.remove(tmpFile3)
  file.remove(tmpFile4)

}

