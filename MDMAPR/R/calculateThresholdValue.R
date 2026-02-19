#Function calculates threshold value for each individual well on a qPCR plate based on raw absorbance values. The threshold function is based on the second derivative method to calculate the threshold value for qPCR data.
calculate_threshold_value <- function(fluorescence_values) {

  fluorescence_values <- as.data.frame(t(fluorescence_values))

  #New table with threshold data
  thresholdData <- as.data.frame(matrix(nrow = ncol(fluorescence_values) , ncol = 1))
  colnames(thresholdData) <- c("computedThresholdValue")

  for (runSample in 1:ncol(fluorescence_values)) {

    #Get the total number of thremocycler cycles for this sample
    number_of_cycles <- length(fluorescence_values [,runSample])

    #Set the initial reference absorbance to the minimum absorbance for the sample
    reference_absorbance_value <- as.numeric(min(fluorescence_values[, runSample]))

    #Set the initial absorbance cycle to the cycle with the minimum absorbance value
    place_holder_cycle = which.min(fluorescence_values[, runSample])

    #Calculate the absorbance range for the sample to be able to assess the percent change from cycle to cycle
    absorb_range = as.numeric(max(fluorescence_values[, runSample])) - as.numeric(min(fluorescence_values[, runSample]))


    #Step through the data points for the cycles from the minimum value to the end of the cycles to see if the data is inceasing within a certain percentage as compared to the total absorbance range for the sample
    for (cycle_number_counter in which.min(fluorescence_values[, runSample]):length(fluorescence_values[, runSample])) {

      #Calculate the difference between the reference (first, initially set to the minimum value for the dataset) value and the test value (current value in the data series for this loop)
      difference = as.numeric(reference_absorbance_value - as.numeric(fluorescence_values[cycle_number_counter, runSample]))

      #Check to see if the difference between the reference and the test divided by the total range is greater than 0.01.
      #NOTE: could have the minimum variation between successive data points a user defined value here we use less than 1%
      #If yes then making the place holder cycle equal to this cycle number as I will then only use data after this cycle to calculate the threshold.
      if ( (difference / absorb_range) >= 0.01  ) {

        #Update place holder value cycle number
        place_holder_cycle <- cycle_number_counter
      }

      #Setting the reference equal to the value at this loop to represent the reference for the next loop where the test value will be incremented to the absorbance value for the next cycle for the sample
      reference_absorbance_value <- as.numeric(fluorescence_values[cycle_number_counter, runSample])

    } # Closing loop

    ########################### Obtaining the threshold value ###########################

    #NOTE: could have the minimum number of data points to calculate the threshold as a user defined value
    #Finally, checking to see if more than 75% of the data points passed these quality checks for use in calculating the threshold. if not then bad data no threshold calculation is conducted.
    if ((place_holder_cycle/number_of_cycles) < 0.75) {

      #Subset dataframe to plot it
      data_to_plot <- as.data.frame(as.numeric(t(fluorescence_values[, runSample])))

      data_to_plot <- cbind(as.data.frame(c(1:number_of_cycles)), data_to_plot)

      data_to_plot <- data_to_plot[-c(1:place_holder_cycle), ]

      # This is the section where I get the second derivative of the curve and determine the value at which we will set the threshold

      deriv <- function(x, y)
        diff(y) / diff(x)

      middle_pts <- function(x)
        x[-1] - diff(x) / 2

      second_d <- deriv(middle_pts(data_to_plot[, 1]), deriv(data_to_plot[, 1], data_to_plot[, 2]))

      #Getting the max value of the second derivative data set. This will represent the points between the values on the curve between the end of the noise data and the end of the data set.
      #So we need to add the max value index to the placeholder and then add one to round up to the next value to get the index (or cycle number) on the original data set.
      max_deriv_value_index <- which.max(second_d)

      #The theshold value at the cycle determined by the double derivitave is...
      #we need to add one
      threshold_value <- fluorescence_values[(place_holder_cycle + max_deriv_value_index +1),runSample]


      #Add threshold value to dataframe
      thresholdData$computedThresholdValue[runSample] <- threshold_value

    }

    #If no threshold value can be computed
    else {thresholdData$computedThresholdValue[runSample] <- "Unable to Determine Threshold" }
  }

  return(thresholdData)
}
