#' Export Metabolic Rate
#'
#' The function is used to export final dataset with information about background respiration, absolute and mass-specific metabolic rates into a .txt or .csv file. If two traits (MR.data.1, MR.data.2) are used, the datasets might be merged. Additionally, absolute, mass-specific and factorial metabolic scope might be calculated, where MR.data.1 is standard or resting metabolic rate and MR.data.2 is active or maximum metabolic rate.
#'
#' @usage
#' export.MR(MR.data.1, MR.data.2, file = "",
#'           simplify = TRUE, MS = TRUE,
#'           plot.MS.abs = TRUE,
#'           plot.MS.mass = TRUE,
#'           plot.MS.fact = TRUE)
#'
#' @param MR.data.1  a data frame obtained by using the function \code{\link{extract.slope}}
#' @param MR.data.2  a data frame obtained by using the function \code{\link{extract.slope}}
#' @param file  the name of an exported file with results of the analysis
#' @param simplify  logical: if TRUE, the number of columns in the extracted data frame is reduced
#' @param MS  logical: if TRUE, metabolic scope is calculated and attached to the exported dataset
#' @param plot.MS.abs  logical: if TRUE, the graph of absolute metabolic scope is plotted (x-axis shows measurement phases for MR.data.2)
#' @param plot.MS.mass  logical: if TRUE, the graph of mass-specific metabolic scope is plotted (x-axis shows measurement phases for MR.data.2)
#' @param plot.MS.fact  logical: if TRUE, the graph of factorial metabolic scope is plotted (x-axis shows measurement phases of for MR.data.2)
#'
#' @return If only one traits exists, the function exports a data frame with full or simplified structure. If both traits are used, the function returns and exports 'MR.data.1' and 'MR.data.2' with metabolic scope parameters (optionally).
#'
#' @importFrom lattice bwplot
#' @importFrom utils write.table write.csv
#'
#' @examples
#' \dontrun{
#' # if the data have been already loaded to R,
#' # skip the first two lines of the code:
#' data(SMR)
#' data(AMR)
#'
#' results <- export.MR(SMR, AMR,
#'                      file = "results.txt",
#'                      simplify = TRUE,
#'                      MS = TRUE,
#'                      plot.MS.abs = TRUE,
#'                      plot.MS.mass = TRUE,
#'                      plot.MS.fact = TRUE)
#' }
#'
#' @export

export.MR <- function(MR.data.1, MR.data.2, file = "",
                      simplify = TRUE, MS = TRUE,
                      plot.MS.abs = TRUE,
                      plot.MS.mass = TRUE,
                      plot.MS.fact = TRUE){

  if(missing(MR.data.2)){

    if(simplify == TRUE){
      MR.data.1 <- MR.data.1[,c(1,2,3,4,16,7,11,13,14,15)]
    }
    else{
      MR.data.1 <- MR.data.1[,c(1,2,3,4,16,5,6,7,8,9,10,11,12,13,14,15)]
      }
      if(grepl("\\.txt", file) == TRUE){
        write.table(MR.data.1, file, sep = "\t",  row.names = FALSE)
      }
      else if(grepl("\\.csv", file) == TRUE){
        write.csv(MR.data.1, file,  row.names = FALSE)
      }
      else{
        print("Please, check: the file should be in .txt or .csv format")
     }
    return(MR.data.1)
  }

  else{
    name.1 <- deparse(substitute(MR.data.1))
    colnames(MR.data.1)[5] <- paste(name.1, "Date.Time", sep = "_")
    colnames(MR.data.1)[6] <- paste(name.1, "Phase", sep = "_")
    colnames(MR.data.1)[7] <- paste(name.1, "Temp", sep = "_")
    colnames(MR.data.1)[8] <- paste(name.1, "Slope.with.BR", sep = "_")
    colnames(MR.data.1)[9] <- paste(name.1, "Slope", sep = "_")
    colnames(MR.data.1)[10] <- paste(name.1, "SE", sep = "_")
    colnames(MR.data.1)[11] <- paste(name.1, "R2", sep = "_")
    colnames(MR.data.1)[12] <- paste(name.1, "MR.mass.with.BR", sep = "_")
    colnames(MR.data.1)[13] <- paste(name.1, "BR", sep = "_")
    colnames(MR.data.1)[14] <- paste(name.1, "MR.abs", sep = "_")
    colnames(MR.data.1)[15] <- paste(name.1, "MR.mass", sep = "_")

    name.2 <- deparse(substitute(MR.data.2))
    colnames(MR.data.2)[5] <- paste(name.2, "Date.Time", sep = "_")
    colnames(MR.data.2)[6] <- paste(name.2, "Phase", sep = "_")
    colnames(MR.data.2)[7] <- paste(name.2, "Temp", sep = "_")
    colnames(MR.data.2)[8] <- paste(name.2, "Slope.with.BR", sep = "_")
    colnames(MR.data.2)[9] <- paste(name.2, "Slope", sep = "_")
    colnames(MR.data.2)[10] <- paste(name.2, "SE", sep = "_")
    colnames(MR.data.2)[11] <- paste(name.2, "R2", sep = "_")
    colnames(MR.data.2)[12] <- paste(name.2, "MR.mass.with.BR", sep = "_")
    colnames(MR.data.2)[13] <- paste(name.2, "BR", sep = "_")
    colnames(MR.data.2)[14] <- paste(name.2, "MR.abs", sep = "_")
    colnames(MR.data.2)[15] <- paste(name.2, "MR.mass", sep = "_")

    final.data <- merge(MR.data.1, MR.data.2)

    if(MS == FALSE){
      if(simplify == TRUE){
        final.data <- final.data[,c(1,2,3,4,5,8,12,14,15,16,19,23,25,26,27)]
      }
      else{}

      if(grepl("\\.txt", file) == TRUE){
        write.table(final.data, file, sep = "\t",  row.names = FALSE)
      }
      else if(grepl("\\.csv", file) == TRUE){
        write.csv(final.data, file,  row.names = FALSE)
      }
      else{
        print("Please, check: the file should be in .txt or .csv format")
      }

      return(final.data)
    }

    if(MS == TRUE){

      final.data$MS.abs <- final.data[,26] - final.data[,15]
      final.data$MS.mass <- final.data[,27] - final.data[,16]
      final.data$MS.fact <- final.data[,26] / final.data[,15]

      a <- bwplot(MS.abs~final.data[,18]|Ind, data=final.data, as.table = T,
                  xlab = "Measurement phase",
                  ylab = bquote("Absolute MS (" ~ .(final.data$DO.unit[1]) ~ h^-1 ~ ")"),
                  main = "Absolute metabolic scope")
      b <- bwplot(MS.mass~final.data[,18]|Ind, data=final.data, as.table = T,
                  xlab = "Measurement phase",
                  ylab = bquote("Mass-specific MS (" ~ .(final.data$DO.unit[1]) ~ kg^-1 ~ h^-1 ~ ")"),
                  main = "Mass-specific metabolic scope")
      d <- bwplot(MS.fact~final.data[,18]|Ind, data=final.data, as.table = T,
                  xlab = "Measurement phase",
                  ylab = "Factorial MS (coefficient)",
                  main = "Factorial metabolic scope")

      if (plot.MS.abs == TRUE){
        par(mfrow = c(2, 1), ask = T)
        print(a)
      }

      if (plot.MS.mass == TRUE){
        par(mfrow = c(2, 1), ask = T)
        print(b)
      }

      if (plot.MS.fact == TRUE){
        par(mfrow = c(2, 1), ask = T)
        print(d)
      }

      if(simplify == TRUE){
        final.data <- final.data[,c(1,2,3,4,5,8,12,14,15,16,19,
                                    23,25,26,27,28,29,30)]
      }
      else{}

      if(grepl("\\.txt", file) == TRUE){
        write.table(final.data, file, sep = "\t",  row.names = FALSE)
      }
      else if(grepl("\\.csv", file) == TRUE){
        write.csv(final.data, file,  row.names = FALSE)
      }
      else{
        print("Please, check: the file should be in .txt or .csv format")
      }

      return(final.data)
    }
  }
}
