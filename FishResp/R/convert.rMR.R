#' Convert Raw Respirometry Data (rMR)
#'
#' This function is the modification of the function \code{\link[rMR]{DO.unit.convert}} from the R package \pkg{rMR} allowing to convert raw respirometry data from one DO unit to another obtained in multichannel respirometry systems.
#'
#' @usage
#' convert.rMR(import.file, export.file,
#'             n.chamber = c(1,2,3,4,5,6,7,8),
#'             logger = c("AutoResp", "FishResp", "QboxAqua"),
#'             DO.units.in, DO.units.out, salinity = 0,
#'             bar.press = 101.325, bar.units.in = "kpa")
#'
#' @param import.file  the name of a file with raw respirometry data which should be imported to convert DO units
#' @param export.file  the name of a file with results of the DO unit conversion
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param logger  string: the name of a logger software used for intermittent-flow respirometry. Note, that both 'OxyView' and 'Pyro Oxygen Logger' used in couple with the 'AquaResp' software should be converted to the 'FishResp' format before running this function (see the functions \code{\link{presens.aquaresp}} or \code{\link{pyroscience.aquaresp}}, respectively).
#' @param DO.units.in  string: dissolved oxygen unit in an imported file (more information can be found in the documentation of the function \code{\link[rMR]{DO.unit.convert}}, R package \pkg{rMR})
#' @param DO.units.out  string: dissolved oxygen unit in an exported file (more information can be found in the documentation of the function \code{\link[rMR]{DO.unit.convert}}, R package \pkg{rMR})
#' @param salinity  string: salinity is measured in ppm (more information can be found in the documentation of the function \code{\link[rMR]{DO.unit.convert}}, R package \pkg{rMR})
#' @param bar.press  string: ambient barometric pressure value (more information can be found in the documentation of the function \code{\link[rMR]{DO.unit.convert}}, R package \pkg{rMR})
#' @param bar.units.in  string: barometric pressure unit (more information can be found in the documentation of the function \code{\link[rMR]{DO.unit.convert}}, R package \pkg{rMR})
#'
#' @return The function exports a data frame with converted DO units.
#'
#' @importFrom rMR DO.unit.convert
#' @importFrom utils write.table write.csv
#'
#' @examples
#' \dontrun{
#' # Import raw data for active metabolic rate
#' AMR.path = system.file("extdata/stickleback/AMR_raw.txt.xz", package = "FishResp")
#'
#' convert.rMR(import.file = AMR.path,
#'             export.file = "converted_AMR_raw.txt",
#'             n.chamber = 2, logger = "AutoResp", salinity = 0,
#'             DO.units.in = "mg/L", DO.units.out = "PP",
#'             bar.press = 101.325, bar.units.in = "kpa")
#' }
#' @export

convert.rMR <- function(import.file, export.file,
                        n.chamber = c(1,2,3,4,5,6,7,8),
				                logger = c("AutoResp", "FishResp", "QboxAqua"),
				                DO.units.in, DO.units.out,
						            salinity = 0,
                        bar.press = 101.325,
                        bar.units.in = "kpa"){

  ### AutoResp format ###
  if (logger == "AutoResp"){
    MR.data.head <- readLines(import.file, 37)
    MR.data.body <- read.table(import.file, sep = "\t", skip=37,
                               header=T, check.names=FALSE, strip.white=T)

	if (n.chamber == 1){
      MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                salinity = salinity,
                                salinity.units = "pp.thou",
								                bar.press = bar.press,
								                bar.units.in = bar.units.in,
								                bar.units.out = bar.units.in)
      }

      else if (n.chamber == 2){
      MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                salinity = salinity,
                                salinity.units = "pp.thou",
								                bar.press = bar.press,
								                bar.units.in = bar.units.in,
								                bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 3){
      MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                salinity = salinity,
                                salinity.units = "pp.thou",
								                bar.press = bar.press,
								                bar.units.in = bar.units.in,
								                bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  }

    else if (n.chamber == 4){
	  MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 5){
	  MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,19] <- DO.unit.convert(x = MR.data.body[,19],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,18],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 6){
	  MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,19] <- DO.unit.convert(x = MR.data.body[,19],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,18],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,22] <- DO.unit.convert(x = MR.data.body[,22],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,21],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 7){
	  MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,19] <- DO.unit.convert(x = MR.data.body[,19],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,18],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,22] <- DO.unit.convert(x = MR.data.body[,22],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,21],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,25] <- DO.unit.convert(x = MR.data.body[,25],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,24],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 8){
	  MR.data.body[,7] <- DO.unit.convert(x = MR.data.body[,7],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,6],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
 								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,13] <- DO.unit.convert(x = MR.data.body[,13],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,12],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,19] <- DO.unit.convert(x = MR.data.body[,19],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,18],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,22] <- DO.unit.convert(x = MR.data.body[,22],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,21],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,25] <- DO.unit.convert(x = MR.data.body[,25],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,24],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,28] <- DO.unit.convert(x = MR.data.body[,28],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,27],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	## rename columns ##
    col.DO <- c(7,10,13,16,19,22,25,28)
    ch=1
	for(i in col.DO){
	  names(MR.data.body)[i] <- paste("CH", ch, " O2 [", DO.units.out, "]", sep = "")
      ch = ch + 1
    }

	write.table(MR.data.head,
		file = export.file,
		append = F,
		sep = "\t",
		row.names = F,
		col.names = F,
		quote = F)

	MR.data.name <- data.frame()
	MR.data.name <- rbind(MR.data.name, names(MR.data.body))

	write.table(MR.data.name,
	            file = export.file,
	            append = T,
	            sep = "\t",
	            row.names = F,
	            col.names = F,
	            quote = F)

	names(MR.data.body) <- NULL

	write.table(MR.data.body,
		file = export.file,
		append = T,
		sep = "\t",
		row.names = F,
		col.names = F,
		quote = F)
    }

  else if (logger == "FishResp"){

    MR.data.body <- read.table(import.file, sep = "\t", header=T,
                               check.names=FALSE, strip.white=T)

	if (n.chamber == 1){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
        }
    else if (n.chamber == 2){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 3){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  }

    else if (n.chamber == 4){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 5){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,12] <- DO.unit.convert(x = MR.data.body[,12],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,11],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 6){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,12] <- DO.unit.convert(x = MR.data.body[,12],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,11],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,14] <- DO.unit.convert(x = MR.data.body[,14],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,13],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 7){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,12] <- DO.unit.convert(x = MR.data.body[,12],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,11],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,14] <- DO.unit.convert(x = MR.data.body[,14],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,13],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
      }

	else if (n.chamber == 8){
      MR.data.body[,4] <- DO.unit.convert(x = MR.data.body[,4],
                                DO.units.in = DO.units.in,
                                DO.units.out = DO.units.out,
                                temp.C = MR.data.body[,3],
                                salinity = salinity,
                                salinity.units = "pp.thou",
                                bar.press = bar.press,
								                bar.units.in = bar.units.in,
								                bar.units.out = bar.units.in)
	  MR.data.body[,6] <- DO.unit.convert(x = MR.data.body[,6],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,5],
							                	 salinity = salinity,
                                 salinity.units = "pp.thou",
						                		 bar.press = bar.press,
							                 	 bar.units.in = bar.units.in,
							                	 bar.units.out = bar.units.in)
	  MR.data.body[,8] <- DO.unit.convert(x = MR.data.body[,8],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,7],
							                	 salinity = salinity,
                                 salinity.units = "pp.thou",
							                	 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,10] <- DO.unit.convert(x = MR.data.body[,10],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,9],
							                	 salinity = salinity,
                                 salinity.units = "pp.thou",
							                	 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,12] <- DO.unit.convert(x = MR.data.body[,12],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,11],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
                                 bar.units.out = bar.units.in)
	  MR.data.body[,14] <- DO.unit.convert(x = MR.data.body[,14],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,13],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,16] <- DO.unit.convert(x = MR.data.body[,16],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,15],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	  MR.data.body[,18] <- DO.unit.convert(x = MR.data.body[,18],
                                 DO.units.in = DO.units.in,
                                 DO.units.out = DO.units.out,
                                 temp.C = MR.data.body[,17],
								                 salinity = salinity,
                                 salinity.units = "pp.thou",
								                 bar.press = bar.press,
								                 bar.units.in = bar.units.in,
								                 bar.units.out = bar.units.in)
	 }

		## rename columns ##
    col.DO <- c(4,6,8,10,12,14,16,18)
    col.DO <- col.DO[1:n.chamber]
    ch=1
	for(i in col.DO){
	  names(MR.data.body)[i] <- paste("Ox.", ch, " [", DO.units.out, "]", sep = "")
      ch = ch + 1
	  }

    MR.data.name <- data.frame()
    MR.data.name <- rbind(MR.data.name, names(MR.data.body))

    write.table(MR.data.name,
                file = export.file,
                sep = "\t",
                row.names = F,
                col.names = F,
                quote = F)

    names(MR.data.body) <- NULL

    write.table(MR.data.body,
                file = export.file,
                append = T,
                sep = "\t",
                row.names = F,
                col.names = F,
                quote = F)
    }


    else if (logger == "QboxAqua"){
      MR.data.body <- read.table(import.file, sep = ",", header=T,
                                 check.names=FALSE, strip.white=T)
    if (n.chamber == 1){
      MR.data.body[, ncol(MR.data.body)] <- DO.unit.convert(x = MR.data.body[, ncol(MR.data.body)],
                                            DO.units.in = DO.units.in,
                                            DO.units.out = DO.units.out,
                                            temp.C = MR.data.body[,4],
    								                        salinity = salinity,
                                            salinity.units = "pp.thou",
    								                        bar.press = bar.press,
    								                        bar.units.in = bar.units.in,
    								                        bar.units.out = bar.units.in)
    }else{
      print("If 'Qubit Systems' starts producing multi-chamber systems for aquatic respirometry, please contact us via email: fishresp@gmail.com")
    }

    MR.data.name <- data.frame()
    MR.data.name <- rbind(MR.data.name, names(MR.data.body))

    write.table(MR.data.name,
                file = export.file,
                sep = ",",
                row.names = F,
                col.names = F,
                quote = F)

    names(MR.data.body) <- NULL

    write.table(MR.data.body,
                file = export.file,
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                quote = F)
    }
  }
