#' Create an host-symbiont association matrix
#'
#' Creates a binary host-symbiont association matrix from a two-columns matrix
#' or data frame of host-symbiont associations.
#'
#' @param hs A two-columns matrix or data frame representing associations
#'        between hosts (column 1) and symbionts (column 2) species.
#'
#' @return An association binary matrix, with hosts in rows and symbionts in
#'         columns, sorted alphabetically.
#'
#' @export
#'
#' @examples
#' data(nuc_cp)
#' NTaxa <- sort(NUCtr$tip.label)
#' CPTaxa <- sort(CPtr$tip.label)
#'
#' NC <- assoc_mat(data.frame(NTaxa, CPTaxa))
#'
assoc_mat <- function(hs) {
  host <- hs[,1]
  symb <- hs[,2]
  mhs <- (matrix(NA, nrow = length(symb), ncol = length(host)))
  diag(mhs) <- 1
  mhs[is.na(mhs)] <- 0
  colnames(mhs) <- symb
  rownames(mhs) <- host

  m <- mhs %>% by(rownames(mhs), colSums)
  m1 <- as.matrix(do.call(rbind,m))

  m2 <- t(m1)

  m2 <- m2 %>% by(rownames(m2), colSums)
  m2 <- as.matrix(do.call(cbind,m2))
  return(m2)
}
