#' Return ancestry-matrix-zone masks for different relationship types
#'
#' This returns a list of two logical matrices.  The first catches the
#' zone that is above the diagonal (when plotted with ind_1 on the x-axis
#' and ind_2 on the y-axis).  These correspond to ind_2 being the "older"
#' member of the pair than ind_1.  The second catches the zone on the
#' lower diagonal.  If the zone spans the diagonal, then each matrix
#' returned in the list is the same.
#' @param num_generations the number of generations to go back. 0 = self;
#' 1 = to the parents; 2 = to the grandparents; 3 = to the great grandparents,
#' etc.
#' @param R the relationship whose zones you want to mask with TRUEs in the
#' output matrices.  See below for the possible choices.
#' @details The relationships whose zones we are equipped to deal with go out to n=4 generation,
#' and are named (and ordered) as follows, which is the order in which they
#' appear in the package data vector `relationship_zone_names`:
#' - `Se`: self.  This is as far as it goes with num_generations = 0
#' - `PO`: parent-offspring
#' - `Si`: sibling. This is as far as it goes with num_generations = 1.
#' - `GP`: grandparental
#' - `A` : avuncular (aunt-niece)
#' - `FC`: first cousin. This is as far as it goes with num_generations = 2.
#' - `GGP`: great-grandparental
#' - `GA`: great-avuncular (great-aunt/great-niece, etc).
#' - `FCr1`: first-cousin once removed
#' - `SC`: second cousin. This is as far as it goes with num_generations = 3
#' - `GGGP`: great-great-grandparental
#' - `GGA`: great-great-avuncular
#' - `FCr2`: first cousin twice removed
#' - `SCr1`: second cousin once removed
#' - `TC`: third cousin
#' At this point the additional zones for num_generations = 3 have not been
#' implemented.
#' @return This function returns a list of two matrices.  Each one has FALSEs everywhere
#' except in the zones corresponding to the relationship, where it has TRUEs.  As mentioned
#' above, the first matrix captures the zones where ind_2 is the "older" one and the
#' second matrix captures the zones where ind_1 is the "older" one.  For the symmetrical
#' relationships (Se, Si, FC, etc.) the two matrices in the list are identical.
#' @keywords internal
#' @export
anc_match_masks <- function(
  num_generations,
  R
) {

  # catch errors
  stopifnot(R %in% relationship_zone_names)

  Ridx = which(R == relationship_zone_names)
  # here is the highest possible index for each number of generations, counting from
  # zero (so we have to bump it up when we test it)
  maxis <- c(1, 3, 6, 10, 15)
  if(Ridx > maxis[num_generations + 1]) {
    stop(
      paste(
        "Requested relationship, ",
        R,
        ", is too distant for num_generations = ",
        num_generations,
        collapse = "",
        sep = ""
      )
    )
  }

  L <- 2 ^ (num_generations + 1) - 1  # number of rows and columns in matrix

  M1 <- matrix(FALSE, nrow = L, ncol = L )
  M2 <- matrix(FALSE, nrow = L, ncol = L )

  switch(
    R,
    Se = {
      M1[1, 1] <- TRUE
      list(M1, M1)
    },
    PO = {
      M1[1, 2:3] <- TRUE
      M2[2:3, 1] <- TRUE
      list(M1, M2)
    },
    Si = {
      M1[2:3, 2:3] <- TRUE
      list(M1, M1)
    },
    GP = {
      M1[1, 4:7] <- TRUE
      M2[4:7, 1] <- TRUE
      list(M1, M2)
    },
    A = {
      M1[2:3, 4:7] <- TRUE
      M2[4:7, 2:3] <- TRUE
      list(M1, M2)
    },
    FC = {
      M1[4:7, 4:7] <- TRUE
      list(M1, M1)
    },
    GGP = {
      M1[1, 8:15] <- TRUE
      M2[8:15, 1] <- TRUE
      list(M1, M2)
    },
    GA = {
      M1[2:3, 8:15] <- TRUE
      M2[8:15, 2:3] <- TRUE
      list(M1, M2)
    },
    FCr1 = {
      M1[4:7, 8:15] <- TRUE
      M2[8:15, 4:7] <- TRUE
      list(M1, M2)
    },
    SC = {
      M1[8:15, 8:15] <- TRUE
      list(M1, M1)
    },
    GGGP = {
      M1[1, 16:31] <- TRUE
      M2[16:31, 1] <- TRUE
      list(M1, M2)
    },
    GGA = {
      M1[2:3, 16:31] <- TRUE
      M2[16:31, 2:3] <- TRUE
      list(M1, M2)
    },
    FCr2 = {
      M1[4:7, 16:31] <- TRUE
      M2[16:31, 4:7] <- TRUE
      list(M1, M2)
    },
    SCr1 = {
      M1[8:15, 16:31] <- TRUE
      M2[16:31, 8:15] <- TRUE
      list(M1, M2)
    },
    TC = {
      M1[16:31, 16:31] <- TRUE
      list(M1, M1)
    }
  )


}
