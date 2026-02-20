## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  prompt = TRUE, 
  comment = NA, 
  background = "white", 
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 65, blank = TRUE),
  collapse = TRUE,
  comment = "#>"
)

options(continue = " ") # Removes a "+" from a line of code that continues on to the next line

## ----setup--------------------------------------------------------------------
library(binGroup2)

## ----groupmem, echo=FALSE-----------------------------------------------------
GroupMembershipMatrix <- function(stage1, stage2 = NULL,
                                  stage3 = NULL, stage4 = NULL) {

  # Check group sizes
  if (!is.null(stage2)) {
    if (sum(stage2) != stage1) {
      stop("The number of individuals in stage 2 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }
  }

  if (!is.null(stage3)) {
    if (sum(stage3) != stage1) {
      stop("The number of individuals in stage 3 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }

    if (is.null(stage2)) {
      stop("No group sizes were provided for stage 2 of testing. A group membership matrix will be constructed for a two-stage hierarchical algorithm.\n")
    }
  }

  if (!is.null(stage4)) {
    if (sum(stage4) != stage1) {
      stop("The number of individuals in stage 3 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }

    if (is.null(stage2)) {
      stop("No group sizes were provided for stage 2 of testing. A group membership matrix will be constructed for a two-stage hierarchical algorithm.\n")
    }

    if (is.null(stage3)) {
      stop("No group sizes were provided for stage 3 of testing. A group membership matrix will be constructed for a three-stage hierarchical algorithm.\n")
    }

  }

  # Create group membership matrix for two-stage hierarchical testing
  if (is.null(stage2)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2), nrow = 2, byrow = TRUE)
    }

  # Create group membership matrix for three-stage hierarchical testing
  else if (is.null(stage3)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3), nrow = 3, byrow = TRUE)
    }

  # Create group membership matrix for four-stage hierarchical testing
  else if (is.null(stage4)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- NULL # Initiate row3
    for (i in 1:length(stage3)) {
      row3 <- c(row3, rep(x = i, time = stage3[i]))
      }

    # Set row4
    row4 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3, row4),
                          nrow = 4, byrow = TRUE)
    }

  else {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- NULL # Initiate row3
    for (i in 1:length(stage3)) {
      row3 <- c(row3, rep(x = i, time = stage3[i]))
      }

    # Set row4
    row4 <- NULL # Initiate row3
    for (i in 1:length(stage4)) {
      row4 <- c(row4, rep(x = i, time = stage4[i]))
      }

    # Set row5
    row5 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3, row4, row5),
                          nrow = 5, byrow = TRUE)
    }

  hier.config
}

## -----------------------------------------------------------------------------
# Group membership matrix
group.member <- GroupMembershipMatrix(stage1 = 10)
group.member

# Compute operating characteristics for a one-infection assay
save1 <- opChar1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99, 
                 hier.config = group.member, print.time = FALSE)
names(save1)
summary(save1)
ExpTests(save1)

## -----------------------------------------------------------------------------
# Compute operating characteristics for a two-infection assay
save2 <- opChar2(algorithm = "D2", p.vec = c(0.95, 0.02, 0.02, 0.01), 
                 Se = c(0.99, 0.99), Sp = c(0.99, 0.99), 
                 hier.config = group.member, print.time = FALSE)
names(save2)
summary(save2)
ExpTests(save2)

## -----------------------------------------------------------------------------
# Find OTC for a one-infection assay
save3 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99, 
              group.sz = 3:20, obj.fn = "ET", print.time = FALSE)
names(save3)
summary(save3)
Config(save3)

## -----------------------------------------------------------------------------
# Find OTC for a two-infection assay
save4 <- OTC2(algorithm = "D2", p.vec = c(0.95, 0.02, 0.02, 0.01), 
              Se = c(0.99, 0.99), Sp = c(0.99, 0.99), 
              group.sz = 3:20, obj.fn = "ET", print.time = FALSE)
names(save4)
summary(save4)
Config(save4)

## -----------------------------------------------------------------------------
# Compare testing configurations
group.member.OTC <- GroupMembershipMatrix(stage1 = 5)
save5 <- opChar2(algorithm = "D2", p.vec = c(0.95, 0.02, 0.02, 0.01), 
                 Se = c(0.99, 0.99), Sp = c(0.99, 0.99), 
                 hier.config = group.member.OTC, print.time = FALSE)
CompareConfig(save2, save5)

