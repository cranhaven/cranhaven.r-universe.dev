## --------------- saromatic.R --------------- ##
#                                               #
#   saro.dist                                   #
#   saro.geometry                               #
#   saro.motif                                  #
#   xprod                                       #
#                                               #
## ------------------------------------------- ##

## ---------------------------------------------------------------- ##
#     saro.dist <- function(pdb,  threshold = 7, rawdata = FALSE)    #
## ---------------------------------------------------------------- ##
#' Compute Distances to the Closest Aromatic Residues
#' @description Computes distances to the closest aromatic residues.
#' @usage saro.dist(pdb, threshold = 7, rawdata = FALSE)
#' @param pdb either the path to the PDB file of interest or the 4-letters identifier.
#' @param threshold distance in ångströms, between the S atom and the aromatic ring centroid, used as threshold.
#' @param rawdata logical to indicate whether we also want the raw distance matrix between delta S and aromatic ring centroids.
#' @details For each methionyl residue this function computes the distances to the closest aromatic ring from Y, F and W. When that distance is equal or lower to the threshold, it will be computed as a S-aromatic motif.
#' @return The function returns a dataframe with as many rows as methionyl residues are found in the protein. The distances in ångströms to the closest tyrosine, phenylalanine and triptophan are given in the columns, as well as the number of S-aromatic motifs detected with each of these amino acids. Also a raw distance matrix can be provided.
#' @author Juan Carlos Aledo
#' @examples \dontrun{saro.dist('1CLL')}
#' @references Reid, Lindley & Thornton, FEBS Lett. 1985, 190:209-213.
#' @seealso saro.motif(), saro.geometry()
#' @importFrom bio3d read.pdb
#' @export

saro.dist <- function(pdb, threshold = 7, rawdata = FALSE){

  ## ---------- Getting pdb & Checking for M, Y, F and W ----------- ##
  x <- tryCatch(
    {
      suppressWarnings(bio3d::read.pdb(pdb, verbose = FALSE))
    },
    error = function(cond){
      return(-999)
    }
  )

  if (is.numeric(x)){
    message("Sorry, read.pdb failed")
    return(NULL)
  }

  x <- x$atom
  is_there_M <- TRUE %in% (x$resid == "MET")
  is_there_Y <- TRUE %in% (x$resid == "TYR")
  is_there_F <- TRUE %in% (x$resid == "PHE")
  is_there_W <- TRUE %in% (x$resid == "TRP")

  if (! is_there_M){
    output <- "There are not methionine residues in this protein"
    return(output)
  }

  ## ----------------------- Methionine Residues ----------------------------- ##
  is.Msulfur <- x$resid == "MET" & x$elety == "SD" # SD atoms marked TRUE
  if (sum(is.Msulfur) == 0){
    output <- 'No delta sulfur atoms found in this protein'
    return(output)
  }
  SD_atoms <- x[is.Msulfur,] # pdb object restricted to SD atoms

  ## ------------ Centroids of TYROSIONE Ring Atoms ----------------------- ##
  if (is_there_Y) {
    is.Yring <- x$resid == "TYR" &
      x$elety %in% c("CG", "CD1","CD2", "CE1", "CE2", "CZ")
    Yring_atoms <- x[is.Yring,] # Subset with atoms from Tyr rings
    Yring_atoms$id <- paste(Yring_atoms$resno, Yring_atoms$chain, sep = '-')
    pos_Y_res <- unique(Yring_atoms$id)

    ## Coordinates of the ring's centroid:
    Yring_centroid <- as.data.frame(matrix(rep(NA,
                                               6*length(pos_Y_res)), ncol = 6))
    names(Yring_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')

    counter <- 1
    for (t in pos_Y_res){

      pos <- as.numeric(strsplit(t, split = '-')[[1]][1])
      chain <- strsplit(t, split = '-')[[1]][2]

      x_coordinate <- sum(Yring_atoms$x[which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)])
      y_coordinate <- sum(Yring_atoms$y[which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)])
      z_coordinate <- sum(Yring_atoms$z[which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)])

      Yring_centroid$resid[counter] <- 'TYR'
      Yring_centroid$chain[counter] <- chain
      Yring_centroid$resno[counter] <- pos
      Yring_centroid$x[counter] <- round(x_coordinate/length(which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)), 3)
      Yring_centroid$y[counter] <- round(y_coordinate/length(which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)), 3)
      Yring_centroid$z[counter] <- round(z_coordinate/length(which(Yring_atoms$resno == pos & Yring_atoms$chain == chain)), 3)
      counter <- counter + 1
    }
  } else { # An empty object of class "data.frame" is created
    Yring_centroid <- as.data.frame(matrix(rep(NA, 6), ncol = 6))
    names(Yring_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')
  }

  ## ----------------- Centroids of PHENYLALANINE Ring Atoms --------------- ##
  if (is_there_F) {
    is.Fring <- x$resid == "PHE" &
      x$elety %in% c("CG", "CD1", "CD2", "CE1", "CE2", "CZ")
    Fring_atoms <- x[is.Fring,] # Subset with atoms from Phe rings
    Fring_atoms$id <- paste(Fring_atoms$resno, Fring_atoms$chain, sep = '-')
    pos_F_res <- unique(Fring_atoms$id)

    ## Coordinates of the ring's centroid:
    Fring_centroid <- as.data.frame(matrix(rep(NA,
                                               6*length(pos_F_res)), ncol = 6))
    names(Fring_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')

    counter <- 1
    for (t in pos_F_res){

      pos <- as.numeric(strsplit(t, split = '-')[[1]][1])
      chain <- strsplit(t, split = '-')[[1]][2]

      x_coordinate <- sum(Fring_atoms$x[which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)])
      y_coordinate <- sum(Fring_atoms$y[which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)])
      z_coordinate <- sum(Fring_atoms$z[which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)])

      Fring_centroid$resid[counter] <- 'PHE'
      Fring_centroid$chain[counter] <- chain
      Fring_centroid$resno[counter] <- pos
      Fring_centroid$x[counter] <- round(x_coordinate/length(which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)), 3)
      Fring_centroid$y[counter] <- round(y_coordinate/length(which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)), 3)
      Fring_centroid$z[counter] <- round(z_coordinate/length(which(Fring_atoms$resno == pos & Fring_atoms$chain == chain)), 3)
      counter <- counter + 1
    }
  } else { # An empty object of class "data.frame" is created
    Fring_centroid <- as.data.frame(matrix(rep(NA, 6), ncol = 6))
    names(Fring_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')
  }

  ## ----------------- Centroids of TRYPTOPHANE Ring Atoms ---------------------- ##
  if (is_there_W) {
    is.Wring1 <- x$resid == "TRP" &
      x$elety %in% c("CG", "CD1", "CD2", "CE2", "NE1")
    Wring1_atoms <- x[is.Wring1,] # Subset with atoms from Trp rings
    Wring1_atoms$id <- paste(Wring1_atoms$resno, Wring1_atoms$chain, sep = '-')
    pos_W1_res <- unique(Wring1_atoms$id)

    ## Coordinates of the ring's centroid:
    Wring1_centroid <- as.data.frame(matrix(rep(NA,
                                                6*length(pos_W1_res)), ncol = 6))
    names(Wring1_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')

    counter <- 1
    for (t in pos_W1_res){

      pos <- as.numeric(strsplit(t, split = '-')[[1]][1])
      chain <- strsplit(t, split = '-')[[1]][2]

      x_coordinate <- sum(Wring1_atoms$x[which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)])
      y_coordinate <- sum(Wring1_atoms$y[which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)])
      z_coordinate <- sum(Wring1_atoms$z[which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)])

      Wring1_centroid$resid[counter] <- 'TRP'
      Wring1_centroid$chain[counter] <- chain
      Wring1_centroid$resno[counter] <- pos
      Wring1_centroid$x[counter] <- round(x_coordinate/length(which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)), 3)
      Wring1_centroid$y[counter] <- round(y_coordinate/length(which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)), 3)
      Wring1_centroid$z[counter] <- round(z_coordinate/length(which(Wring1_atoms$resno == pos & Wring1_atoms$chain == chain)), 3)
      counter <- counter + 1
    }

    is.Wring2 <- x$resid == "TRP" &
      x$elety %in% c("CD2", "CE2", "CE3", "CZ3", "CH2", "CZ2")

    Wring2_atoms <- x[is.Wring2,] # Subset with atoms from Trp rings
    Wring2_atoms$id <- paste(Wring2_atoms$resno, Wring2_atoms$chain, sep = '-')
    pos_W2_res <- unique(Wring2_atoms$id)

    ## Coordinates of the ring's centroid:
    Wring2_centroid <- as.data.frame(matrix(rep(NA,
                                                6*length(pos_W2_res)), ncol = 6))
    names(Wring2_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')

    counter <- 1
    for (t in pos_W2_res){

      pos <- as.numeric(strsplit(t, split = '-')[[1]][1])
      chain <- strsplit(t, split = '-')[[1]][2]

      x_coordinate <- sum(Wring2_atoms$x[which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)])
      y_coordinate <- sum(Wring2_atoms$y[which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)])
      z_coordinate <- sum(Wring2_atoms$z[which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)])

      Wring2_centroid$resid[counter] <- 'TRP'
      Wring2_centroid$chain[counter] <- chain
      Wring2_centroid$resno[counter] <- pos
      Wring2_centroid$x[counter] <- round(x_coordinate/length(which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)), 3)
      Wring2_centroid$y[counter] <- round(y_coordinate/length(which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)), 3)
      Wring2_centroid$z[counter] <- round(z_coordinate/length(which(Wring2_atoms$resno == pos & Wring2_atoms$chain == chain)), 3)
      counter <- counter + 1
    }
  } else { # An empty object of class "coords" is created
    Wring1_centroid <- as.data.frame(matrix(rep(NA, 6), ncol = 6))
    names(Wring1_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z')
    Wring2_centroid <- Wring1_centroid
  }

  ## ------------------------------- Output Dataframe -------------------------- ##
  output <- as.data.frame(matrix(rep(NA, nrow(SD_atoms) * 10), ncol = 10))
  names(output) <- c('Met', 'Tyr_closest', 'Yd', 'number_bonds_MY',
                     'Phe_closest', 'Fd', 'number_bonds_MF',
                     'Trp_closest', 'Wd', 'number_bonds_MW')
  output$Met <- paste(SD_atoms$resid, SD_atoms$chain, SD_atoms$resno, sep = "-")

  S_atoms <- as.matrix(SD_atoms[,c('x', 'y', 'z')])
  rownames(S_atoms) <- paste(SD_atoms$resid, SD_atoms$chain, SD_atoms$resno, sep = "-")
  raw <- as.data.frame(output$Met)
  names(raw)[1] <- "Met"
  ## ----------------- Distances between SD atoms and Tyr's centroids ----------------- ##
  if (is_there_Y){
    Y_centroids <- as.matrix(Yring_centroid[,4:6])
    row.names(Y_centroids) <- paste(Yring_centroid$resid, Yring_centroid$chain, Yring_centroid$resno, sep = "-")

    d <- pairwise.dist(S_atoms, Y_centroids, squared = FALSE)
    for (i in 1:nrow(output)){
      t <- d[i,]
      names(t) <- colnames(d)
      distancia <- min(t)
      residuo <- names(t)[which(t == distancia)]
      output$Tyr_closest[i] <- residuo
      output$Yd[i] = distancia
      output$number_bonds_MY[i] <- length(which(t <= threshold))
    }
    raw <- cbind(raw, as.data.frame(d))
  }


  ## ----------------- Distances between SD atoms and Phe's centroids ----------------- ##
  if (is_there_F){
    F_centroids <- as.matrix(Fring_centroid[,4:6])
    row.names(F_centroids) <- paste(Fring_centroid$resid, Fring_centroid$chain, Fring_centroid$resno, sep = "-")

    d <- pairwise.dist(S_atoms, F_centroids, squared = FALSE)
    for (i in 1:nrow(output)){
      t <- d[i,]
      names(t) <- colnames(d)
      distancia <- min(t)
      residuo <- names(t)[which(t == distancia)]
      output$Phe_closest[i] <- residuo
      output$Fd[i] = distancia
      output$number_bonds_MF[i] <- length(which(t <= threshold))
    }
    raw <- cbind(raw, as.data.frame(d))
  }


  ## ----------------- Distances between SD atoms and Trp's centroids ----------------- ##
  if (is_there_W){
    W1_centroids <- as.matrix(Wring1_centroid[,4:6])
    row.names(W1_centroids) <- paste(Wring1_centroid$resid, Wring1_centroid$chain, Wring1_centroid$resno, sep = "-")
    W2_centroids <- as.matrix(Wring2_centroid[,4:6])
    row.names(W2_centroids) <- paste(Wring2_centroid$resid, Wring2_centroid$chain, Wring2_centroid$resno, sep = "-")

    d1 <- pairwise.dist(S_atoms, W1_centroids, squared = FALSE)
    d2 <- pairwise.dist(S_atoms, W2_centroids, squared = FALSE)
    d <- matrix(rep(NA, nrow(d1)*ncol(d1)), ncol = ncol(d1))
    colnames(d) <- colnames(d1)

    for (i in 1:nrow(d1)){ # Keep only the ring closest to methione
      for (j in 1:ncol(d1)){
        d[i,j] = min(d1[i,j], d2[i,j])
      }
    }

    for (i in 1:nrow(output)){
      t <- d[i,]
      distancia <- min(t)
      residuo <- names(t)[which(t == distancia)]
      output$Trp_closest[i] <- residuo
      output$Wd[i] = distancia
      output$number_bonds_MW[i] <- length(which(t <= threshold))
    }
    raw <- cbind(raw, as.data.frame(d))
  }


  ## -------------------- Returning the results ------------------------- ##
  if (rawdata){
    output <- list(output, raw)
    return(output)
  } else {
    return(output)
  }
  ## -------------------------------------------------------------------- ##
}


## ---------------------------------------------------------------------- ##
#         saro.geometry <- function(pdb, rA, chainA, rB, chainB)           #
## ---------------------------------------------------------------------- ##
#' Compute Geometric Parameters of S-Aromatic Motifs
#' @description Computes distances and angles of S-aromatic motifs.
#' @usage saro.geometry(pdb, rA, chainA = 'A', rB, chainB = 'A')
#' @param pdb either the path to the PDB file of interest or the 4-letters identifier.
#' @param rA numeric position of one of the two residues involved in the motif.
#' @param chainA a character indicating the chain to which belong the first residue.
#' @param rB numeric position of the second residue involved in the motif.
#' @param chainB a character indicating the chain to which belong the second residue.
#' @details The distance between the delta sulfur atom and the centroid of the aromatic ring is computed, as well as the angle between this vector and the one perpendicular to the plane containing the aromatic ring. Based on the distance (d) and the angle (theta) the user decide whether the two residues are considered to be S-bonded or not (usually when d < 7 and theta < 60º).
#' @return The function returns a dataframe providing the coordinates of the sulfur atom and the centroid (centroids when the aromatic residue is tryptophan), as well as the distance (ångströms) and the angle (degrees) mentioned above.
#' @author Juan Carlos Aledo
#' @examples \dontrun{saro.geometry('1CLL', rA = 141, rB = 145)}
#' @references Reid, Lindley & Thornton, FEBS Lett. 1985, 190, 209-213.
#' @seealso saro.motif(), saro.dist()
#' @importFrom bio3d read.pdb
#' @export

saro.geometry <- function(pdb, rA, chainA = 'A', rB, chainB = 'A'){

  ## -------------------- Reading the pdb file ---------------------- ##
  temp <- tempfile()

  x <- tryCatch(
    {
      suppressWarnings(bio3d::read.pdb(pdb, verbose = FALSE))
    },
    error = function(cond){
      return(-999)
    }
  )

  if (is.numeric(x)){
    message("Sorry, read.pdb failed")
    return(NULL)
  }

  x <- x$atom
  xA <- x[which(x$resno == rA & x$chain == chainA),]
  xB <- x[which(x$resno == rB & x$chain == chainB),]
  x <- rbind(xA, xB)

  ## -------- Check the rA and rB are suitable amino acids ---------- ##
  if (x$resid[which(x$resno == rA)][1] == "MET"){
    if (! x$resid[which(x$resno == rB)][1] %in% c("TYR", "PHE", "TRP")){
      stop("No aromatic residue has been provided")
    }
  } else if (x$resid[which(x$resno == rB)][1] == "MET"){
    if (! x$resid[which(x$resno == rA)][1] %in% c("TYR", "PHE", "TRP")){
      stop("No aromatic residue has been provided")
    } else {
      ra <- rA
      rA <- rB
      rB <- ra
      rm(ra)
    }
  } else {
    stop("No methionyl residue has been provided")
  }

  ## ---------- Centroid of the aromatic ring  ----------- ##
  if (x$resid[which(x$resno == rB)][1] == "TYR"){
    is.ring <- x$resid == "TYR" &
      x$elety %in% c("CG", "CD1","CD2", "CE1", "CE2", "CZ")
  } else if (x$resid[which(x$resno == rB)][1] == "PHE"){
    is.ring <- x$resid == "PHE" &
      x$elety %in% c("CG", "CD1", "CD2", "CE1", "CE2", "CZ")
  } else if (x$resid[which(x$resno == rB)][1] == "TRP"){
    is.ring <- x$resid == "TRP" &
      x$elety %in% c("CG", "CD1", "CD2", "CE2", "NE1",
                     "CD2", "CE2", "CE3", "CZ3", "CH2", "CZ2")
  }

  ring_atoms <- x[is.ring,]

  ## ------- Coordinates of the ring centroid ------------ ##
  if (ring_atoms$resid[1] == "TRP"){
    nf <- 3
  } else{
    nf <- 2
  }

  ring_centroid <- as.data.frame(matrix(rep(NA, 8*nf), ncol = 8))
  names(ring_centroid) <- c('resid', 'chain', 'resno', 'x', 'y', 'z', 'length','theta')

  if ("SD" %in% x$elety){
    ring_centroid$resid[1] <- "MET"
    ring_centroid$chain[1] <- x$chain[which(x$elety == "SD")]
    ring_centroid$resno[1] <- rA
    ring_centroid$x[1] <- x$x[which(x$elety == "SD")]
    ring_centroid$y[1] <- x$y[which(x$elety == "SD")]
    ring_centroid$z[1] <- x$z[which(x$elety == "SD")]
    ring_centroid$theta[1] <- NA
  } else {
    stop("Delta S atom has not been found")
  }

  if (nf == 3){ # Two rings from tryptophan
    ring1 <- c("CG", "CD1", "CD2", "CE2", "NE1")
    ring2 <- c("CD2", "CE2", "CE3", "CZ3", "CH2", "CZ2")
    x_coordinate1 <- sum(ring_atoms$x[which(ring_atoms$elety %in% ring1)])
    x_coordinate2 <- sum(ring_atoms$x[which(ring_atoms$elety %in% ring2)])
    y_coordinate1 <- sum(ring_atoms$y[which(ring_atoms$elety %in% ring1)])
    y_coordinate2 <- sum(ring_atoms$y[which(ring_atoms$elety %in% ring2)])
    z_coordinate1 <- sum(ring_atoms$z[which(ring_atoms$elety %in% ring1)])
    z_coordinate2 <- sum(ring_atoms$z[which(ring_atoms$elety %in% ring2)])

    ring_centroid$resid[2] <- ring_centroid$resid[3] <- 'TRP'
    ring_centroid$chain[2] <- ring_centroid$chain[3] <- ring_atoms$chain[1]
    ring_centroid$resno[2] <- ring_centroid$resno[3] <- rB

    ring_centroid$x[2] <- round(x_coordinate1/length(which(ring_atoms$elety %in% ring1)), 3)
    ring_centroid$x[3] <- round(x_coordinate2/length(which(ring_atoms$elety %in% ring2)), 3)
    ring_centroid$y[2] <- round(y_coordinate1/length(which(ring_atoms$elety %in% ring1)), 3)
    ring_centroid$y[3] <- round(y_coordinate2/length(which(ring_atoms$elety %in% ring2)), 3)
    ring_centroid$z[2] <- round(z_coordinate1/length(which(ring_atoms$elety %in% ring1)), 3)
    ring_centroid$z[3] <- round(z_coordinate2/length(which(ring_atoms$elety %in% ring2)), 3)
  } else { # Either Tyr or Phe ring
    x_coordinate <- sum(ring_atoms$x[which(ring_atoms$resno == rB)])
    y_coordinate <- sum(ring_atoms$y[which(ring_atoms$resno == rB)])
    z_coordinate <- sum(ring_atoms$z[which(ring_atoms$resno == rB)])

    ring_centroid$resid[2] <- ring_atoms$resid[1]
    ring_centroid$chain[2] <- ring_atoms$chain[1]
    ring_centroid$resno[2] <- rB
    ring_centroid$x[2] <- round(x_coordinate/length(which(ring_atoms$resno == rB)), 3)
    ring_centroid$y[2] <- round(y_coordinate/length(which(ring_atoms$resno == rB)), 3)
    ring_centroid$z[2] <- round(z_coordinate/length(which(ring_atoms$resno == rB)), 3)
  }

  ## ------------------- Computing norms & angles -------------------- ##
  CG_CD1 <- c(ring_atoms$x[2] - ring_atoms$x[1],
              ring_atoms$y[2] - ring_atoms$y[1],
              ring_atoms$z[2] - ring_atoms$z[1])
  CG_CD2 <- c(ring_atoms$x[3] - ring_atoms$x[1],
              ring_atoms$y[3] - ring_atoms$y[1],
              ring_atoms$z[3] - ring_atoms$z[1])
  u <- xprod(CG_CD1, CG_CD2) # orthogonal to the ring plane

  for (i in 2:nrow(ring_centroid)){
    CS <- c(ring_centroid$x[1] - ring_centroid$x[i], # vector centroid-S
            ring_centroid$y[1] - ring_centroid$y[i],
            ring_centroid$z[1] - ring_centroid$z[i])

    normCS <- sqrt(crossprod(CS, CS)) # actually the function crossprod computes the dotproduct
    ring_centroid$length[i] <- normCS
    cos_theta <- crossprod(u, CS)/(crossprod(u, u) * crossprod(CS, CS))
    theta <- round(acos(cos_theta) * 180 / pi, 2)
    ring_centroid$theta[i] <- theta
  }
  return(ring_centroid)
}


## ---------------------------------------------------------------- ##
#     saro.motif <- function(pdb,  threshold = 7, onlySaro = T)      #
## ---------------------------------------------------------------- ##
#' Search for S-Aromatic Motifs
#' @description Searches for S-aromatic motifs in proteins.
#' @usage saro.motif(pdb, threshold = 7, onlySaro = TRUE)
#' @param pdb either the path to the PDB file of interest or the 4-letters identifier.
#' @param threshold distance in ångströms, between the S atom and the aromatic ring centroid, used as threshold.
#' @param onlySaro logical, if FALSE the output includes information about Met residues that are not involved in S-aromatic motifs.
#' @details For each methionyl residue taking place in a S-aromatic motif, this function computes the aromatic residues involved, the distance between the delta sulfur and the aromatic ring's centroid, as well as the angle between the sulfur-aromatic vector and the normal vector of the plane containing the aromatic ring.
#' @return The function returns a dataframe reporting the S-aromatic motifs found for the protein of interest.
#' @author Juan Carlos Aledo
#' @examples \dontrun{saro.motif('1CLL')}
#' @references Reid, Lindley & Thornton, FEBS Lett. 1985, 190, 209-213.
#' @seealso saro.dist(), saro.geometry()
#' @importFrom bio3d read.pdb
#' @export

saro.motif <- function(pdb, threshold = 7, onlySaro = TRUE){
  # Test with a pdb for a protein containing a single methionine
  # Test with a pdb for a protein containing a single aromatic
  # Test with a pdb for a protein lacking either Met or aromatic residues

  ## -------------- Getting raw distance matrix ------------- ##
  d <- saro.dist(pdb, threshold, rawdata = TRUE)

  if (!is.list(d)){
    if (is.null(d)){
      message("Sorry, saro.dist failed")
      return(NULL)
    }
  } else {
    d <- d[[2]]
  }

  d[,1] <- as.character(d[,1])

  ## -------------- Output dataframe ------------------------ ##
  output <- as.data.frame(matrix(rep(NA, 4), ncol = 4))
  names(output) <- c("Met", "Aromatic", "Length", "Angle")

  ## ------- Identifying the aromatic moiety ---------------- ##
  for (i in 1:nrow(d)){
    t <- colnames(d)[which(d[i,-1] <= threshold) + 1]
    if (length(t) == 0){
      empty <- as.data.frame(matrix(c(d[i,1], 'Any', NA, NA), ncol = 4))
      names(empty) <-  c("Met", "Aromatic", "Length", "Angle")
      output <- rbind(output, empty)
    } else {
      for (j in 1:length(t)){ # number of aromatic residues
        partial <- as.data.frame(matrix(c(d[i,1], t[j], NA, NA), ncol = 4))
        names(partial) <- c("Met", "Aromatic", "Length", "Angle")
        output <- rbind(output, partial)
      }
    }
  }
  output <- output[-1,]

  ## ---- Geometric parameters for the S-aromatic motif ----- ##
  for (i in 1:nrow(output)){
    if (output$Aromatic[i] == 'Any'){
      next
    } else {
      rA <- strsplit(output$Met[i], "-")[[1]]
      chainA <- rA[2]
      rA <- as.numeric(rA[3])

      rB <- strsplit(output$Aromatic[i], "-")[[1]]
      chainB <- rB[2]
      rB <- as.numeric(rB[3])

      t <- saro.geometry(pdb, rA, chainA, rB, chainB)

      if (nrow(t) == 2){ # For Tyr and Phe
        output$Length[i] <- round(t$length[2], 2)
        output$Angle[i] <- t$theta[2]
      } else if (nrow(t) == 3){ # For Trp
        output$Length[i] <- round(t$length[which(t$length == min(t$length[2], t$length[3]))], 2)
        output$Angle[i] <- t$theta[which(t$length == min(t$length[2], t$length[3]))]
      }
    }
  }

  if (onlySaro){
    output <- output[output$Aromatic != "Any", ]
  }
  attr(output, 'threshold') <- threshold
  return(output)
}



## ------------------------------------------------------------------------------- ##
#                   xprod(c(x1, y1, z1), c(x2, y2, z2)))                            #
## ------------------------------------------------------------------------------- ##
#' Compute Cross Product
#' @description Computes the  cross product of two vectors in three-dimensional euclidean space.
#' @usage xprod(...)
#' @param ... vectors involved in the cross product.
#' @details For each methionyl residue taking place in a S-aromatic motif, this function computes the aromatic residue involved, the distance between the delta sulfur and the aromatic ring's centroid, as well as the angle between the sulfur-aromatic vector and the normal vector of the plane containing the aromatic ring.
#' @return This function returns a vector that is orthogonal to the plane containing the two vector used as arguments.
#' @author Juan Carlos Aledo
#' @examples xprod(c(1,1,1), c(1,2,1))
#' @export

xprod <- function(...) {
  args <- list(...)

  ## ------------------------ Check for valid arguments ------------------------ ##
  if (length(args) == 0) {
    stop("No data supplied")
  }
  len <- unique(sapply(args, FUN=length))
  if (length(len) > 1) {
    stop("All vectors must be the same length")
  }
  if (len != length(args) + 1) {
    stop("Must supply N-1 vectors of length N")
  }

  ## ---- Compute cross product by taking the determinant of sub-matricies ---- ##
  m <- do.call(rbind, args)
  sapply(seq(len),
         FUN=function(i) {
           det(m[,-i,drop=FALSE]) * (-1)^(i+1)
         })
}



