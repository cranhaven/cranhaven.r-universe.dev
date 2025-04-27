#' Convert Raw Respirometry Data (respirometry)
#'
#' This function is the modification of the function \code{\link[respirometry]{conv_o2}} from the R package \pkg{respirometry} allowing to convert raw respirometry data from one DO unit to another obtained in multichannel respirometry systems.
#'
#' @usage
#' convert.respirometry(import.file, export.file,
#'                      n.chamber = c(1,2,3,4,5,6,7,8),
#'                      logger = c("AutoResp", "FishResp", "QboxAqua"),
#'                      from, to, sal = 0, atm_pres = 1013.25)
#'
#' @param import.file  the name of a file with raw respirometry data which should be imported to convert DO units
#' @param export.file  the name of a file with results of the DO unit conversion
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param logger  string: the name of a logger software used for intermittent-flow respirometry. Note, that both 'OxyView' and 'Pyro Oxygen Logger' used in couple with the 'AquaResp' software should be converted to the 'FishResp' format before running this function (see the functions \code{\link{presens.aquaresp}} or \code{\link{pyroscience.aquaresp}}, respectively).
#' @param from  string: dissolved oxygen unit in an imported file (more information can be found in the documentation of the function \code{\link[respirometry]{conv_o2}}, R package \pkg{respirometry})
#' @param to  string: dissolved oxygen unit in an exported file (more information can be found in the documentation of the function \code{\link[respirometry]{conv_o2}}, R package \pkg{respirometry})
#' @param sal  string: salinity is measured in ppm (more information can be found in the documentation of the function \code{\link[respirometry]{conv_o2}}, R package \pkg{respirometry})
#' @param atm_pres  string: ambient atmospheric pressure value (more information can be found in the documentation of the function \code{\link[respirometry]{conv_o2}}, R package \pkg{respirometry})
#'
#' @return The function exports a data frame with converted DO units.
#'
#' @importFrom respirometry conv_o2
#' @importFrom utils write.table write.csv
#'
#' @examples
#' \dontrun{
#' # Import raw data for standard metabolic rate
#' SMR.path = system.file("extdata/stickleback/SMR_raw.txt.xz", package = "FishResp")
#'
#' convert.respirometry(import.file = SMR.path,
#'                      export.file = "converted_SMR_raw.txt",
#'                      n.chamber = 1, logger = "AutoResp",
#'                      from = "mg_per_l", to = "mmol_per_l",
#'                      sal = 0, atm_pres = 1013.25)
#' }
#' @export

convert.respirometry <- function(import.file, export.file,
					              n.chamber = c(1,2,3,4,5,6,7,8),
								 logger = c("AutoResp", "FishResp", "QboxAqua"),
								 from, to, sal = 0,
								 atm_pres = 1013.25){

  ### AutoResp format ###
  if (logger == "AutoResp"){
    MR.data.head <- readLines(import.file, 37)
    MR.data.body <- read.table(import.file, sep = "\t", skip=37,
                               header=T, check.names=FALSE, strip.white=T)

	if (n.chamber == 1){
      MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
      }

      else if (n.chamber == 2){
      MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 3){
      MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  }

    else if (n.chamber == 4){
	  MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 5){
	  MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,19] <- conv_o2(o2 = MR.data.body[,19],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,18],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 6){
	  MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,19] <- conv_o2(o2 = MR.data.body[,19],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,18],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,22] <- conv_o2(o2 = MR.data.body[,22],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,21],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 7){
	  MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,19] <- conv_o2(o2 = MR.data.body[,19],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,18],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,22] <- conv_o2(o2 = MR.data.body[,22],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,21],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,25] <- conv_o2(o2 = MR.data.body[,25],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,24],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 8){
	  MR.data.body[,7] <- conv_o2(o2 = MR.data.body[,7],
                                from = from,
                                to = to,
                                temp = MR.data.body[,6],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,13] <- conv_o2(o2 = MR.data.body[,13],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,12],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,19] <- conv_o2(o2 = MR.data.body[,19],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,18],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,22] <- conv_o2(o2 = MR.data.body[,22],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,21],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,25] <- conv_o2(o2 = MR.data.body[,25],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,24],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,28] <- conv_o2(o2 = MR.data.body[,28],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,27],
                                 sal = sal, atm_pres = atm_pres)
      }

	## rename columns ##
    col.DO <- c(7,10,13,16,19,22,25,28)
    ch=1
	for(i in col.DO){
	  names(MR.data.body)[i] <- paste("CH", ch, " O2 [", to, "]", sep = "")
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
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
        }
    else if (n.chamber == 2){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 3){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  }

    else if (n.chamber == 4){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 5){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,12] <- conv_o2(o2 = MR.data.body[,12],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,11],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 6){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,12] <- conv_o2(o2 = MR.data.body[,12],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,11],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,14] <- conv_o2(o2 = MR.data.body[,14],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,13],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 7){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,12] <- conv_o2(o2 = MR.data.body[,12],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,11],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,14] <- conv_o2(o2 = MR.data.body[,14],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,13],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
      }

	else if (n.chamber == 8){
      MR.data.body[,4] <- conv_o2(o2 = MR.data.body[,4],
                                from = from,
                                to = to,
                                temp = MR.data.body[,3],
                                sal = sal, atm_pres = atm_pres)
	  MR.data.body[,6] <- conv_o2(o2 = MR.data.body[,6],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,5],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,8] <- conv_o2(o2 = MR.data.body[,8],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,7],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,10] <- conv_o2(o2 = MR.data.body[,10],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,9],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,12] <- conv_o2(o2 = MR.data.body[,12],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,11],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,14] <- conv_o2(o2 = MR.data.body[,14],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,13],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,16] <- conv_o2(o2 = MR.data.body[,16],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,15],
                                 sal = sal, atm_pres = atm_pres)
	  MR.data.body[,18] <- conv_o2(o2 = MR.data.body[,18],
                                 from = from,
                                 to = to,
                                 temp = MR.data.body[,17],
                                 sal = sal, atm_pres = atm_pres)
      }

		## rename columns ##
    col.DO <- c(4,6,8,10,12,14,16,18)
    col.DO <- col.DO[1:n.chamber]
    ch=1
	for(i in col.DO){
	  names(MR.data.body)[i] <- paste("Ox.", ch, " [", to, "]", sep = "")
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
		    MR.data.body[, ncol(MR.data.body)] <- conv_o2(o2 = MR.data.body[, ncol(MR.data.body)],
	                                  from = from,
		                                to = to,
		                                temp = MR.data.body[,4],
		                                sal = sal, atm_pres = atm_pres)
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
