## ------------- aa.R ------------- ##
#                                    #
#     aa.at                          #
#     is.at                          #
#     aa.comp                        #
#                                    #
## -------------------------------- ##

## --------------------------------------------------------------- ##
#             aa.at(at, target, uniprot = TRUE)                     #
## --------------------------------------------------------------- ##
#' Residue Found at the Requested Position
#' @description Returns the residue found at the requested position.
#' @usage aa.at(at, target, uniprot = TRUE)
#' @param at the position in the primary structure of the protein.
#' @param target a character string specifying the UniProt ID of the protein of interest or, alternatively, the sequence of that protein.
#' @param uniprot logical, if TRUE the argument 'target' should be an ID.
#' @details Please, note that when uniprot is set to FALSE, target can be the string returned by a suitable function, such as get.seq or other.
#' @return Returns a single character representing the residue found at the indicated position in the indicated protein.
#' @author Juan Carlos Aledo
#' @examples \dontrun{aa.at(28, 'P01009')}
#' @seealso is.at(), aa.comp()
#' @importFrom bio3d read.fasta
#' @export

aa.at <- function(at, target, uniprot = TRUE){
  if (uniprot == TRUE){
    target <- tryCatch(
      {
        get.seq(target, as.string = FALSE)
      },
      error = function(cond){
        return(NULL)
      }
    )

    if (is.null(target)){
      message("Sorry, get.seq failed")
      return(NULL)
    } else {
      target <- target[[1]]
    }

  } else {
    if (length(target) == 1){
      target <- strsplit(target, split="")[[1]]
    }
  }
  if (at %in% 1:length(target)){
    return(target[at])
  } else {
    message(paste(at , " isn't a valid position for this protein", sep=""))
    return(NULL)
  }
}


## --------------------------------------------------------------- ##
#             is.at(at, target, aa = 'M', uniprot = TRUE)           #
## --------------------------------------------------------------- ##
#' Check Residue a Fixed Position
#' @description Checks if a given amino acid is at a given position.
#' @usage  is.at(at, target, aa = 'M', uniprot = TRUE)
#' @param at the position in the primary structure of the protein.
#' @param target a character string specifying the UniProt ID of the protein of interest or, alternatively, the sequence of that protein.
#' @param aa the amino acid of interest.
#' @param uniprot logical, if TRUE the argument 'target' should be an ID.
#' @details Please, note that when uniprot is set to FALSE, target can be the string returned by a suitable function, such as get.seq or other.
#' @return Returns a boolean. Either the residue is present at that position or not.
#' @author Juan Carlos Aledo
#' @examples \dontrun{is.at(28, 'P01009', 'Q')}
#' @seealso aa.at(), aa.comp()
#' @export

is.at <- function(at, target, aa = 'M', uniprot = TRUE){
  if (uniprot == TRUE){

    target <- tryCatch(
      {
        get.seq(target)
      },
      error = function(cond){
        return(NULL)
      }
    )

    if (is.null(target)){
      message("Sorry, get.seq failed")
      return(NULL)
    } else {
      target <- target[[1]]
    }

  }
  return(at %in% gregexpr(aa, target)[[1]])
}


## --------------------------------------------------------------- ##
#             aa.comp(target, uniprot = TRUE, reference, init)      #
## --------------------------------------------------------------- ##
#' Amino Acid Composition
#' @description Returns a table with the amino acid composition of the target protein.
#' @usage aa.comp(target, uniprot = TRUE, reference = 'human', init = FALSE)
#' @param target a character string specifying the UniProt ID of the protein of interest or, alternatively, the sequence of that protein.
#' @param uniprot logical, if TRUE the argument 'target' should be an ID.
#' @param reference amino acid frequencies (in percent) of the proteinogenic amino acids to be used as reference. It should be either 'human', 'up' (composition of proteins in UniProt in 2019). Alternatively, the user can pass as argument any vector with 20 values to be used as reference.
#' @param init logical, whether remove or not the first residue (initiation methionine) from the sequence.
#' @return Returns a list where the first element is a dataframe with the observed and expected frequencies for each amino acid, the second element is the result of the Chi-squared test. In addition, a plot to reflect potential deviations from the reference standard composition is shown.
#' @author Juan Carlos Aledo
#' @examples aa.comp('MPSSVSWGILLLAGLCCLVPVSLAEDPQGDAAQK', uniprot = FALSE)
#' @seealso is.at(), renum.pdb(), renum.meto(), renum(), aa.at()
#' @importFrom graphics text
#' @importFrom graphics abline
#' @importFrom graphics points
#' @importFrom stats fisher.test
#' @importFrom stats chisq.test
#' @export

aa.comp <- function(target, uniprot = TRUE, reference = 'human', init = 'FALSE'){

  if (uniprot == TRUE){
    seq <- tryCatch(
      {
        get.seq(id = target)
      },
      error = function(cond){
        return(NULL)
      }
    )
    if (is.null(seq)){
      message("Sorry, get.seq failed")
      return(NULL)
    }
    id <- target
  } else {
    seq <- target
    id <- 'user-provided sequence'
  }

  output <- data.frame(aa = aai$aa, observed = NA)

  if (init){
    seq <- substring(seq, first = 2, last= nchar(seq))
  }
  N <- nchar(seq)

  for (aa in output$aa){
    t <- gregexpr(aa, seq)[[1]]
    if (t[1] == -1){
      output$observed[which(output$aa == aa)] <- 0
    } else {
      output$observed[which(output$aa == aa)] <- length(t)
    }
  }

  output$fobs <- round(100 * (output$observed/nchar(seq)), 2)

  if (reference == 'human'){
    output$fexp <- aai$human
  } else if (reference == 'up'){
    output$fexp <- aai$uniprot_2019
  } else if (class(reference) == numeric & length(reference) == 20){
    output$fexp <- reference
  } else {
    warning("A proper amino acid frequency reference should be provided")
  }

  output$expected <- round((output$fexp * N)/100, 0)
  output$o_e <- output$observed - output$expected
  war <- which(output$expected < 5)


  ## ---- Plotting results
  esp <- output$expected
  names(esp) <- output$aa
  esp <- esp[order(esp)]

  M <- max(output$expected, output$observed)

  plot(output$expected[which(output$o_e > 1)],
       output$observed[which(output$o_e > 1)],
       col = "red",
       xlim = c(0,M+10), ylim = c(0,M+10),
       xlab = 'Expected', ylab = 'Observed')

  abline(v = output$expected[which(output$o_e > 1)],
         lty = 2, lwd = 0.3, col = "red")

  points(output$expected[which(output$o_e < -1)],
         output$observed[which(output$o_e < -1)],
         col = "blue",)

  abline(v = output$expected[which(output$o_e < -1)],
         lty = 2, lwd = 0.3, col = "blue")

  points(output$expected[which(output$o_e %in% -1:1)],
         output$observed[which(output$o_e %in% -1:1)],
         pch = 19)

  abline(0, 1)

  df <- data.frame(aa = output$aa,
                   ex = output$expected,
                   o_e = output$o_e)
  df$color <- NA

  df <- df[order(df$ex), ]
  df$color[df$o_e > 1] <- "red"
  df$color[df$o_e < -1] <- "blue"
  df$color[is.na(df$color)] <- "black"
  df$aa <- as.character(df$aa)

  for(i in 1:20){
    if (i%%2 == 0){
      y <- M+5
    } else {
      y <- M+10
    }
    text(df$ex[i], y, labels = df$aa[i],
         col = df$color[i], cex = 0.4)
  }


  Chi <- chisq.test(output[, c(2,5)], simulate.p.value = TRUE)


  attr(output, 'seq') <- target
  attr(output, 'number_aa_with_expected_less_5') <- length(war)
  return(list(output, Chi))
}

