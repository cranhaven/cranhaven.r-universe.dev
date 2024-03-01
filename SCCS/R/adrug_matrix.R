# A function that converts the adrug or aedrug column vector to a matrix with
# nrow = number of total events (not cases as some cases can have more than one event)
# and ncol=the maxmimum number of exposures per case.

adrug_matrix <- function(indiv, aevent, adrug) {
  
  events_no <- data.frame(cbind("indiv"=indiv, "aevent"=aevent)) # will be used to determine the number of rows of the adrug_new ( adrug in format 1)
  no_of_events <- data.frame(table((unique(events_no)$indiv))) # number of events for each case 
  
  events_expo <- data.frame(cbind("indiv"=indiv, "aevent"=aevent, "adrug"=adrug))
  
  # new 23-04-2019 After a glitch in the formatdata function was found (sorting data by indiv)
  # events_expo <- events_expo[order(events_expo$indiv), ] 
  events_expo <- events_expo[order(events_expo$indiv,events_expo[,3]), ]  # 29_11_21 - Replaced the above line by this
  
  
  events_expo_unique <- data.frame(unique(events_expo))
  
  # number of exposures for each event
  no_of_expo <- data.frame(table(events_expo_unique$indiv))
  
  # The actual number of exposures for each case 
  no_of_expo_case <- no_of_expo
  no_of_expo_case[,2] <- no_of_expo_case[,2]/no_of_events[,2]  
  
  # the actual number of exposures for each event
  
  no_of_expo_events <- rep(no_of_expo_case[,2], times=no_of_events[,2])
  
  # Cumulative sum of the number of exposures per event
  cumsum_no_of_expo_events <- cumsum(no_of_expo_events) 
  cumsum_no_of_expo_events_1 <- c(1,(cumsum(no_of_expo_events)+1)) # add 1 at the beginning of the vector to start the first row from 1
  cumsum_no_of_expo_events_1 <- cumsum_no_of_expo_events_1[-length(cumsum_no_of_expo_events_1)]
  
  
  # max number of exposures, use the function
  max_no_of_expo <- max_expo(indiv, adrug) 
  adrug_new <- matrix(NA, nrow=nrow(unique(events_no)), ncol=max_no_of_expo)
  
  for (i in 1:(nrow(adrug_new))) {
    adrug_new[i,1:no_of_expo_events[i]] <-  events_expo_unique[cumsum_no_of_expo_events_1[i]:cumsum_no_of_expo_events[i] ,3]     
  }
  return(adrug_new)
}

