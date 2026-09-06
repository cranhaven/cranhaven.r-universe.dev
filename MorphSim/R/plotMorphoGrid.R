#' Plots morphological matrix
#'
#' @description
#' This function plots the full morphological matrix assocaited with the character data
#' at the tips of a tree. Requires a morpho object as input.
#' @param data A morpho object
#' @param seq the sequence data to plot: "tips", "nodes", "SA", or "recon"
#' @param num.trait default is set to "all" which plots all traits in black font. If you
#' want to focus on a specific trait set it here, e.g. num.trait = 1 and this trait will
#' be highlighted
#' @param col A vector of colors that should be the same length or longer than the number of different character states (k). if not specified, the traits from 0 to 6 can be differentiated
#' @param timetree  TRUE or FALSE Indicate whether you want to plot a time tree or not. default FALSE, uses distance tree if FALSE
#'
#' @return
#' No return value, called for its side effect of producing a plot.
#'
#' @importFrom ape write.tree
#'
#' @export
#'
#' @examples
#' data(morpho_data)
#' # plot the character matrix
#' plotMorphoGrid(data = morpho_data, seq = "tips", num.trait = "all")


plotMorphoGrid <- function(data = NULL,
                           timetree = FALSE,
                           seq = "tips",
                           num.trait = "all",
                           col =  c("lavender", "white", "lightskyblue1", "pink", "gold2", "forestgreen", "coral")){


  if (is.null(data)) stop("Error: 'data' cannot be NULL.")

  if (!is.morpho(data)) stop("Error: 'data' must be a 'morpho'.")

  if (!seq %in% c("tips", "recon")) {
    stop("Error: 'seq' must be either 'tips' or 'recon'.")
  }

  if (!("transition_history" %in% names(data)) || length(data$transition_history) == 0) {
    stop("Error: 'data' must contain a non-empty 'transition_history' element.")
  }

  n.traits.total <- length(data$transition_history)

  if (!identical(num.trait, "all")) {
    if (!is.numeric(num.trait) || length(num.trait) != 1 || num.trait < 1 || num.trait > n.traits.total) {
      stop(paste0("Error: 'num.trait' must be 'all' or a numeric value between 1 and ", n.traits.total, "."))
    }
  }





   old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if (inherits(data$sequences[[seq]][[1]][1], "character")) {
    data$sequences[[seq]] <- lapply(data$sequences[[seq]], function(x) {
      x[x == "?"] <- NA        # Replace "?" with NA
      as.numeric(x)            # Convert character vector to numeric
    })
  }

  if (seq == "recon"){
    n.taxa <- length(data$trees$Recon$tree$tip.label)
  } else {
  n.taxa <- length(data$trees$EvolTree$tip.label)
  }
  n.traits <- length( data$sequences[[seq]][[1]])

  ## Are we using a time tree?
  if (timetree) {
    if (seq == "recon") {
      tree_string <-  ape::write.tree(data$trees$Recon$tree)
      tip_labs <- regmatches(tree_string, gregexpr("t\\d+_\\d+", tree_string))[[1]]
    } else {
    tree_string <-  ape::write.tree(data$trees$TimeTree)
    tip_labs <- regmatches(tree_string, gregexpr("t\\d+", tree_string))[[1]]
    }
  } else {
    tree_string <-  ape::write.tree(data$trees$EvolTree)
    tip_labs <- regmatches(tree_string, gregexpr("t\\d+", tree_string))[[1]]
  }

  par(xaxs = "i", yaxs = "i")
  plot(x =c(0,1), y =c(0,1),xaxt = 'n', yaxt = 'n',bty = 'n', pch = '',
       ylab = '', xlab = '', main = "Morphological matrix", cex.main= 1, font.main = 6)

  ## use ablines instead of grid so there is a bit more control
  # need to divide the plot into the number of traits and taxa
  xx <- 1/n.traits
  yy <- 1/n.taxa

  ## to get the center of the yy boxes
  center_a <- yy * 0.5
  center_final <- 1 - center_a
  y_labs <- seq(center_a, center_final, yy )

  axis(2, at=y_labs,labels=tip_labs,
       col.axis="black", las=2, cex.axis=0.8, lwd ="0")

  ## to get the center of the xx boxes
  center_b <- xx * 0.5
  center_final <- 1 - center_b
  x_labs <- seq(center_b, center_final, xx )

  axis(3, at=x_labs,labels=1:n.traits,
       col.axis="black", las=1, cex.axis=0.8, lwd ="0", pos = 0.95)

  if(!is.null(data$model$RateVarTrait) && is.null(data$combined)){
    axis(1, at=x_labs,labels=data$model$RateVarTrait,
         col.axis="black", las=1, cex.axis=0.8, lwd ="0", pos = 0)
    mtext('Rate Category', side=1, line=1, at=-0.07, cex = 0.7, font = 6)
  }
  for (i in 1:n.traits) {
    for (j in 1:n.taxa) {
      state <- as.numeric( data$sequences[[seq]][[tip_labs[j]]][i])
      bg_col <- col[state + 1]
      if (is.na(state)) bg_col = "lightgrey"
      # Draw a rectangle for each box
      rect(
        xleft = x_labs[i]-xx/2, xright = x_labs[i]+xx/2,
        ybottom = y_labs[j]-yy/2, ytop = y_labs[j]+yy/2,
        col = bg_col, border = "black"
      )
    }
  }

  # add border lines
  abline(v=0, col= "grey")
  abline(h=0, col= "grey")


  ## grid up the plot area
  for ( i in 1:n.traits){
    abline(v = xx*i, col= "grey")
  }


  for ( j in 1:n.taxa){
    abline(h = yy*j, col= "grey")
  }


  ## fill in the boxes with the state
  for (i in 1:n.traits) {
    for (j in 1:n.taxa)   {
      state <- as.numeric( data$sequences[[seq]][[tip_labs[j]]][i])
      if (is.na(state)) state = "?"

      # Add the state text in the box
      if (i == num.trait || num.trait == "all") {
        text(x_labs[i], y_labs[j], state, cex = 1, col = "black")
      } else {
        text(x_labs[i], y_labs[j], state, cex = 1, col = "darkgrey")
      }
    }
  }
}
