
processStories <- function(groupedStories) {
  # Add the value
  groupedStories$goal <- NA
  groupedStories$location <- NA

  # Result set
  resultSet <- groupedStories[0,]


  # Process objectives
  objs <- subset(groupedStories, type == "objective")
  if(nrow(objs) > 0) {
    objectives <- processObjectives(objs)
    rbind(resultSet, objectives)
  }


  # Process inputs
  inpt <- subset(groupedStories, type == "input")
  if(nrow(inpt) > 0) {
    inputs <- processInputs(inpt)
    rbind(resultSet, inputs)
  }


  # Process outputs
  outp <- subset(groupedStories, type == "output")
  if(nrow(outp) > 0) {
    outputs <- processOutputs(outp)
    rbind(resultSet, outputs)
  }


  # Add the rest of it
  others <- subset(groupedStories, type == "scenario" | type == "constraint")



  # Return the value
  return( rbind(resultSet, others) )
}








processOutputs <- function(outputs) {
  # Read the classification
  outkw <- outputKeywords

  # Keep the keywords
  seeKw <- " and see it as "
  writeKw <- " saved as "

  # Now for each output
  for(row in 1:nrow(outputs)) {
    # Get the what
    what <- outputs[row,]$what
    firstWord <- word(what)

    # Put the classification
    class <- subset(outkw, keyword == firstWord)$direction
    outputs[row,]$goal <- class

    # Now if it is visualisation and contains the keyword
    if(class == "visualise") {
      # If the split is there
      if(stringr::str_detect(what, seeKw)) {
        # Split
        split <- stringr::str_split(what, seeKw, simplify = TRUE)

        # And save the information
        outputs[row,]$what <- split[1,1]
        outputs[row,]$location <- split[1,2]
      }
    }
    # If it is writing
    else if(class == "write") {
      # If the split is there
      if(stringr::str_detect(what, writeKw)) {
        # Split
        split <- stringr::str_split(what, writeKw, simplify = TRUE)

        # And save information
        outputs[row,]$what <- split[1,1]
        outputs[row,]$location <- split[1,2]
      }
    }
  }

  # Return the thing
  return(outputs)
}








processInputs <- function(inputs) {
  # We need to split by the keyword
  keyword <- " from "

  # Loop through the cases
  for(row in 1:nrow(inputs)) {
    # Get the what
    what <- inputs[row,]$what

    # If the character is there
    if(stringr::str_detect(what, keyword)) {
      # Split first
      split <- stringr::str_split(inputs[row,]$what, keyword, simplify = TRUE)

      # Now assign the new values
      inputs[row, ]$what <- split[1,1]
      inputs[row, ]$location <- split[1,2]
    }
  }

  # Return the value
  return(inputs)
}









processObjectives <- function(objectives) {
  # Read the classification
  objkw <- objKeywords

  # Go through every row, and find the value
  for(row in 1:nrow(objectives)) {
    # Keyword
    kw <- stringr::word(objectives[row,]$what)

    # Get the type
    direction <- subset(objkw, keywords == kw)$direction

    # Add it
    objectives[row, "goal"] <- direction
  }



  # Check if there are conflicts
  directions <- unique(objectives$goal)
  directions <- directions[!is.na(directions)]

  objDirs <- unique(objkw$direction)
  objDirs <- objDirs[!is.na(objDirs)]

  if( all( unique(objkw$direction) %in% directions ) )
    message("Warning: there are conflicting objectives")


  # Return the objective
  return(objectives)
}
