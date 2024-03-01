# Determine the maximum number of exposures per case
 	max_expo <- function(indiv, adrug){
	  expo_no <- cbind("indiv"=indiv, "adrug"=adrug)
	  expo_no_unique <- data.frame(unique(expo_no))  # to delete replication of adrug 
                                   # for same case having more than one event

	  no_of_expo <- table(expo_no_unique$indiv)
	  max_no_of_expo <- max(no_of_expo)
      return(max_no_of_expo)
     }
