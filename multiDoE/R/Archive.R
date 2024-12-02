#### Archive ####
Archive <- function(dim, capacity) {
  ar <- list()
  if (nargs() < 2) {
    capacity <- 100
  }
  ar$nsols <- 0
  ar$dim <- dim
  if (capacity < 0) {
    capacity = 0
  }
  ar$scores <- matrix(0, capacity, dim)
  ar$solutions <- vector("list", capacity)
  return(ar)
}

#### Resize ####
Resize <- function(ar) {
  n <- dim(ar$scores)[1] # capacity
  m <- dim(ar$scores)[2] # dim (numero criteri)
  ar$scores <- rbind(ar$scores, matrix(0, n, m))
  ar$solutions <- c(ar$solutions, vector("list", n))
  return(ar)
}

#### Add ####
Add <- function(ar, sol, score) {
  # check for capacity
  if (length(ar$solutions) == ar$nsols) {
    ar <- Resize(ar)
  }
  ar$nsols <- ar$nsols + 1

  # add solution and scores
  ar$solutions[[ar$nsols]] <- sol
  if (nrow(ar$scores) == 0) {
    ar$scores <- rbind(ar$scores, score)
  } else {
    ar$scores[ar$nsols, ] <- score
  }
  return(ar)
}

#### FixRepmat ####
# problema con la funzione "repmat"

# non uso direttamente repmat (pacchetto pracma) perchè in R quando "data" è una
# matrice con elementi nulli il risultato è null, mentre dovrei avere come
# risultato una matrice di dimensione diversa da 1 e con elementi nulli.
# Fixrepmat prende in input solitamente (verificare se sempre) una riga di una
# matrice.
# CASI CHE DANNO PROBLEMI:
# es. repmat(matrix(c(0,0,1,2,3,3), 2,3, byrow = T),0,1) -- diverso da matlab
# es. repmat(matrix(NA,0,3),2,1) -- NULL

#' @importFrom pracma repmat
FixRepmat <- function(data, num_rows, num_cols) {
  if ((length(data) == 0 & is.matrix(data)) || (num_rows == 0 & is.matrix(data))) {
    return(matrix(NA, 0, dim(data)[2] * num_cols))
  } else if (num_rows == 0) {
    return(matrix(NA, 0, length(data) * num_cols))
  } else {
    return(repmat(data, num_rows, num_cols))
  }
}

#### RemoveDominated ####
# RemoveDominated: Problema se il numero di soluzioni ar$nsols è
# diverso (minore) dalle righe della matrice scores. Vuol dire che rimangono
# delle righe di zeri, da rimuovere poi con la funzione Trim (mai usata?!)
# In R funziona se e solo se il numero di soluzioni coincide con
# la lunghezza della matrice scores dei punteggi e dovrebbe accadere proprio
# questo.

RemoveDominated <- function(ar) {
  # select dominated solutions (and empty lines)
  # toRemove <- matrix(0, dim(ar$scores)[1], 1)
    toRemove <- matrix(0, ar$nsols, 1)

  for (i in 1:ar$nsols) {
  # i.score <- FixRepmat(ar$scores[i, ], dim(ar$scores)[1], 1)
    i.score <- FixRepmat(ar$scores[i, ], ar$nsols, 1)
    toRemove <- toRemove |
    apply((ar$scores >= i.score) & (ar$scores != i.score), 1, all)
  }
  # remove selected
  ar$solutions <- ar$solutions[ ! toRemove]
  ar$scores <- ar$scores[ ! toRemove, ]
  ar$nsols <- length(ar$solutions)
  return(ar)
}

#### RemoveDuplicates ####
RemoveDuplicates <- function(ar) {
  i <- ! duplicated(ar$scores)
  ar$scores <- ar$scores[i, ]
  ar$solutions <- ar$solutions[i]
  ar$nsols <- length(ar$solutions)
  return(ar)
}

#### Trim ####
Trim <- function(ar) {
  toRemove <- apply(ar$scores == 0, 1, all)
  ar$scores <- ar$scores[ ! toRemove, ]
  ar$solutions <- ar$solutions[ ! toRemove]
  ar$nsols <- length(ar$solutions)
  return(ar)
}





