
groupStories <- function(frame) {
  # Add an empty column for the types
  newFrame <- frame
  newFrame$type <- NA

  # Check it for each row
  for(row in 1:nrow(newFrame))
    newFrame[row,"type"] <- checkStoryTypes(newFrame[row,"what"])
  rm(row)

  # Return the resulting value
  return(newFrame)
}











getStoryType <- function(evalVector) {
  # Get the index
  index <- if(any(evalVector)) which(evalVector) else 0

  # Get the text
  return( switch(index, case1  = "objective", case2 = "constraint", case3 = "input", case4 = "output", case5 = "scenario") )
}







checkStoryTypes <- function(storyWhat) {
  # Read all keywords
  keywordsList <- readKeywords()

  objkw <- keywordsList[[1]]
  outkw <- keywordsList[[2]]
  inkw <- keywordsList[[3]]
  conskw <- keywordsList[[4]]
  scenskw <- keywordsList[[5]]

  # Get four values
  isObjective <- FALSE
  isConstraint <- FALSE
  isInput <- FALSE
  isOutput <- FALSE
  isScenario <- FALSE


  # Check if objective function
  for(o in objkw)
    isObjective <- isObjective || startsWith(storyWhat, o)

  # Check if scenario
  for(s in scenskw)
    isScenario <- isScenario || startsWith(storyWhat, s)

  # Check if constraint
  for(c in conskw)
    isConstraint <- isConstraint || startsWith(storyWhat, c)

  # Check if it is an input
  for(i in inkw)
    isInput <- isInput || startsWith(storyWhat, i)

  # Or check if it is an output
  for(p in outkw)
    isOutput <- isOutput || startsWith(storyWhat, p)


  # Return the resulting value
  evalVector <- c(isObjective, isConstraint, isInput, isOutput, isScenario)
  return( if(any(evalVector)) getStoryType(evalVector) else NA )
}
