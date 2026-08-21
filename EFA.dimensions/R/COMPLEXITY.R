


 
COMPLEXITY <- function (loadings, percent=TRUE, degree.change=100, averaging.value=100, verbose=TRUE) {
	
	comp_rows <- as.matrix(apply(loadings, 1, hofmann))
	colnames(comp_rows) <- ''
	
	output <- list(comp_rows=comp_rows) 

	if(percent ) {comp_percent <- 
		complexity_percent(loadings, degree.change = degree.change, averaging.value = averaging.value)
		output$percent <- comp_percent * 100
	}
		
	if(verbose) {
		
		cat('\n\nFactor Solution Complexity:')
	
	  cat('\n\nVariable Complexities:\n')
		# print(round(comp_rows,2))
		
		writeLines(paste0("    ", capture.output(print(round(comp_rows,2), row.names = FALSE))))
		if(percent)  cat('\nPercent Complexity: ',round(comp_percent*100,2), '\n')
	}
	
	return(invisible(output))
}




complexity_percent <- function (loadings, degree.change = 100, averaging.value = 500) {
     
          comps <- data.vs.deteriorating.ss.commun.adjust.loading.matched(
               loadings, degree.change = degree.change, averaging.value = averaging.value)
     
          comppercent <- which(abs(comps[,2] - hofmann.c(loadings)) == min(abs(comps[,2] - hofmann.c(loadings))))
          
          return(invisible(comppercent * .01))
}     




# the syntax below comes from the Appendix of:
# Pettersson, E., & Turkheimer, E. (2014). Self-reported personality pathology has complex structure and imposing 
# simple structure degrades test information. Multivariate Behavioral Research, 49(4), 372-389. 


# HOFMANN'S COMPLEXITY INDEX 


hofmann <- function (x) { (sum(x^2))^2/(sum(x^4)) }


hofmann.c <- function (x) {
        hofmann.row.c <- apply(x,1,hofmann) 
        hofmann.avg <- sum(hofmann.row.c)/nrow(x) 
        return(hofmann.avg) 
}


# SET UP DETERIORATING SIMPLE STRUCTURE 
# AVERAGED ACROSS MANY TRIALS
# ORIGINAL.STRUCTURE = FACTOR PATTERN MATRIX
# DEGREE.CHANGE = NUMBER OF INCREMENTAL CHANGES TOWARD SIMPLE STRUCTURE 
# AVERAGING.VALUE = NUMBER OF REPEATS PER UNIT OF DEGREE CHANGE

data.vs.deteriorating.ss.commun.adjust.loading.matched <- function(
     original.structure,degree.change = 100,averaging.value = 100){
     
     
     # DETERIORATING SIMPLE STRUCTURE 
     # ORIG.STRUCTURE = ORIGINAL STRUCTURE (USED FOR ADJUSTING COMMUNALITY) 
     # DEGREE = DEGREE OF SIMPLE STRUCTURE.
     # 1 = NO SIMPLE STRUCTURE (CIRCUMPLEXICAL) 0 = COMPLETE SIMPLE STRUCTURE

     deteriorating.simple.structure.commun.adjust.loading.matched <- 
     	function(orig.structure, degree = 1) {
          
          nvar <- nrow(orig.structure)
          nspace <- ncol(orig.structure)
          orig.structure <- orig.structure[order(orig.structure[,1]),]
          
          # REQUIRED FUNCTION: SET UP MATRIX FOR STORING VALUES
          decreasing.outer.matrix <- function(orthonormalized.data,degree = 1){
               test2 <- matrix(NA,nrow = nrow(orthonormalized.data), ncol = ncol(orthonormalized.data))
               
               for (k in 1:nrow(orthonormalized.data)){ # IDENTIFY NON-LARGEST VALUES
                    
                    test.save.min <- which(abs(orthonormalized.data[k,]) < max(abs(orthonormalized.data[k,])))
                    # REDUCE NON-LARGEST VALUES BY X AMOUNT 
                    test.save.reduced <- orthonormalized.data[k,test.save.min] * degree
                    # INSERT REDUCED VALUES INTO NEW MATRIX 
                    test2[k,test.save.min] <- test.save.reduced
                    # IDENTIFY LARGEST VALUE
                    test.save.max <- which(abs(orthonormalized.data[k,]) == max(abs(orthonormalized.data[k,])))
                    # INCREASE LARGEST VALUE BY LEFT-OVER FROM DECREASING THE OTHERS 
                    test.save.increased <- sqrt(1- sum(test2[k,test.save.min]^2)) * 
                    								sign(orthonormalized.data[k,test.save.max])
                    # INSERT INCREASED MAXIMUM VALUE INTO NEW MATRIX
                    test2[k,test.save.max] <- test.save.increased }
               
               return(test2)
          }
          
          
          
          # DISTRIBUTION PROPERTIES OF ORIGINAL DATA
          # START SCRIPT
          pts <- matrix(NA,nrow = nvar,ncol = nspace) 
               for(i in 1:nspace){
                    pts[,i] <- rnorm(nvar,mean = mean(orig.structure[,i]), sd = sd(orig.structure[,i]))
          }
          
          # COMMUNALITY OF VARIABLES
          test.communality <- apply(pts^2,1,sum)
          
          # PROJECT ITEMS TO SURFACE BY DIVIDING BY SQRT OF COMMUNALITY
          pts.orth <- pts/sqrt(test.communality)
          # APPLY DECREASING SIMPLE STRUCTURE BY CHOSEN DEGREE
          pts.orth.simple.structured <- decreasing.outer.matrix(pts.orth,degree)
          # SORT DATA
          pts.orth.simple.structured.sorted <- 
          								pts.orth.simple.structured[order(pts.orth.simple.structured[,1]),]
          # COMMUNALITY OF ORIGINAL DATA
          orig.com <- apply(orig.structure^2,1,sum)
          # SCALE SIMULATED DATA BY SQRT OF COMMU- NALITY OF ORIGINAL DATA 
          pts.orth.simple.structured.communality.reduced <- 
               pts.orth.simple.structured.sorted * sqrt(orig.com) 
          return(pts.orth.simple.structured.communality.reduced) 
     }
     store.hofmann <- matrix(NA,nrow = degree.change,ncol = averaging.value)
     
     for(k in 1:averaging.value){
          for(i in 1:degree.change){
               degree.change.sequence <- seq(0,1,1/(degree.change-1))[i]
               structure.simplified <-               
               deteriorating.simple.structure.commun.adjust.loading.matched(original.structure, 
               																degree = degree.change.sequence)
               store.hofmann[i,k] <- hofmann.c(structure.simplified)
          } 
     } 
     hofmann.mean <- apply(store.hofmann,1,mean,na.rm = F)
     
     
     final.full.matrix <- matrix(NA,nrow = degree.change, ncol = 2)
     final.full.matrix[,1] <- 1:degree.change 
     final.full.matrix[,2] <- hofmann.mean
     colnames(final.full.matrix) <- c("Degree complexity", "Hofmann's Complexity")
     
     return(final.full.matrix)
}