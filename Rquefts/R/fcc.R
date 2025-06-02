# Authors: Robert J. Hijmans and Stephan Haefele
# International Rice Research Institute
# Date : March 2009
# Version 0.1
# License GPL v3


# Based on a BASIC program that accompanied the FAO Soil Map of the World. 
# This functions was adapted for the Homogenized World Soil Database, HWSD


.FCC <- function(hwsd) {
# Fertility Capability Classification
	hwsd$PHASE <- as.integer(hwsd$PHASE)
	hwsd$T_TEXTURE <- as.integer(hwsd$T_TEXTURE)
	left <- substr(hwsd$SU_SYM74, 1, 1)
	right <- substr(hwsd$SU_SYM74, 2, 1)
	soil <- hwsd$SU_SYM74
	
	fcc <- matrix(FALSE, nrow=length(soil), ncol=18)
	colnames(fcc) <- c('g', 'd', 'e', 'a', 'h', 'i', 'x', 'v', 'k', 'b', 's', 'n', 'c', "'", '', '', '', '')
	
#   1) gley (g)
	fcc[left == 'G' | left == 'W' | left=='O' | soil == 'Jt' | soil == 'Gt' | right == 'g' , 1] <- TRUE

#   2) dry (d)	
	fcc[left == 'X' | left == 'Y', 2] <- TRUE
	
# 3) low CEC (e)	
	fcc[left == 'Q'] <- TRUE
	fcc[left == 'F' & right != 'h' & hwsd$T_TEXTURE == 1, 3] <- TRUE

	fcc[hwsd$T_CEC_SOIL < 4] <- TRUE
	
#    4) aluminium toxicity (a)
	fcc[soil == 'Gd' | soil == 'Bd' | soil == 'Wd' | soil == 'Fh' | soil == 'Ah', 4] <- TRUE
	fcc[hwsd$T_PH_H2O < 5, 4] <- TRUE
	
#5) acid (h)
	fcc[grep(soil, 'Rd_Nd_Od_Jd_Gh_Th_Fh_Nh_Ah_Wh') == 1 | left == 'P' | left == 'U', 5] <- TRUE
	fcc[(left == 'L' | left == 'B') & (right != 'k' & right != 'e' & right != 'v'), 5] <- TRUE
#	fcc[hwsd$T_PH_H2O < 6, 5] <- TRUE
	
	
#  6 ) high P-fixation (i)
	fcc[(left == 'F' | left == 'A') & hwsd$T_TEXTURE == 3, 6] <- TRUE

	
#  7) X-ray amorphous (x)
	fcc[left == 'T', 7]  <- TRUE

#  8) vertisol (v)
	fcc[left == 'V' | (right == 'v'  & soil != 'Tv'), 8] <- TRUE

#'    9) low K-reserves (k)
	fcc[soil == 'Qa' | soil == 'Qf' | left == 'F' | left == 'A' | left == 'N', 9] <- TRUE
      
#    10) basic reaction (b)
#CODE	VALUE
#4	Petrocalcic
#5	Petrogypsic	
	fcc[hwsd$PHASE == 4 | hwsd$PHASE == 5, 10] <- TRUE
	fcc[left == 'C' | left == 'E' |  ((left=='X' | left=='Y') & right != 'l'), 10] <- TRUE
	fcc[soil == "Bk"  & hwsd$T_TEXTURE != 1, 10] <- TRUE
	fcc[hwsd$T_PH_H2O > 7.3, 6] <- TRUE

	
#   11) salinity (s)
#CODE	VALUE
#10	Saline
	fcc[left == "Z" | hwsd$PHASE == 10, 11]  <- TRUE
	fcc[hwsd$T_ECE > 4, 11] <- TRUE

# 12) natric (n)
#CODE	VALUE
#11	Sodic
	fcc[left == "S" | right == "s" | hwsd$PHASE == 11, 12] <- TRUE
	fcc[hwsd$T_ESP > 15, 12] <- TRUE

	
# 13) cat clay (c)
	fcc[soil == "Jt" | soil == "Gt" | soil == "Hj" , 13] <- TRUE

# 14) gravelly soils
#CODE	VALUE
#1	Stony
	fcc[hwsd$PHASE == 1, 16] <- TRUE
	fcc[hwsd$T_GRAVEL > 15, 16] <- TRUE
	
	
# 15) steep slopes (8-30)

#  16) very steep slopes (>30)

#   17) organic soils (o)
	fcc[left == "O" , 17] <- TRUE

#18) low moisture holding (S)	
	fcc[hwsd$T_TEXTURE == 1, 18] <- TRUE
	return(fcc)
}


#library(RODBC)
#db <- odbcConnectAccess('I:/Soil/HWSD/HWSD.mdb')
#query <- paste("SELECT * FROM HWSD_DATA")
#soil <- sqlQuery(db, query)
#odbcClose(db)
#f <- FCC(data)


.FCCagg <- function(hwsd) {
	fcc <- .FCC(hwsd)
	fcc <- hwsd$SHARE * fcc
	fccagg <- vector()
	for (i in 1:dim(fcc)[2]) {
		j <- tapply( fcc[,i], INDEX=hwsd$MU_GLOBAL, FUN=sum )
		fccagg <- cbind(fccagg, j)
	}
	return(fccagg)
}

