
# START DEMO

## This demo shows how to analyze eye tracking data from a reading exersise.

# This demo uses the simulated data included in the package
# But because the data frame already contains some results, we will subset it so
#  it contains the bare minimum
data( SimData )

SimData <- SimData[ , c(1:4, 6:8)]

str( SimData )

invisible( readline( prompt = "Pres any key to continue" ) )

# First we need to compile the three binary AOI columns into one AOI column
#  contianing the names of the fixated AOI's
SimData$AOI <- compileAOI( data = SimData, AOI = c( "AOI1", "AOI2", "AOI3" ) )

SimData$AOI

invisible( readline( prompt = "Pres any key to continue" ) )

# Following that we will summarize the transitions
trans <- AOItransitions( SimData$AOI )

trans

invisible( readline( prompt = "Pres any key to continue" ) )

## We can also analyze which passes happened. There are two options here
# 1/ We want to know which passes were first passes and which were second passes
#      a first pass is the first time that the participant looks in that AOI
#      a second pass is every time the participant looks back at the AOI after exiting it
SimData$passes <- codePasses( data = SimData, AOI = "AOI" )

SimData$passes

invisible( readline( prompt = "Pres any key to continue" ) )

# By changing the minimal number of fixations needed for a first pass we can
#   determine when we go to a second pass (The default is set to 3)
resultCompare <- codePasses( data = SimData, AOI = "AOI", fix_min = 1 )

resultCompare <- data.frame( SimData$passes, resultCompare )

resultCompare

# notice that fixation 20 to 34 now changed to second pass

invisible( readline( prompt = "Pres any key to continue" ) )

# 2/ In addition to 1 we want to know which first passes clasify as rereading
#     Keep in mind that when setting rereading to true we must provide the
#     x and y coordinates for the fixation point
SimData$passes <- codePasses( data = SimData, AOI = "AOI",rereading = TRUE, 
                              fpx = "xcoord", fpy = "ycoord", fix_size = 20 )

SimData$passes

invisible( readline( prompt = "Pres any key to continue" ) )

# note that changeing the value for the gaze accuracy (i.e. the minimal size of the fovea;
#   needed to determine if a fixation is still on the same line [or in the same 
#   place for that matter])
resultCompare <- codePasses( data = SimData, AOI = "AOI",rereading = TRUE, 
                             fpx = "xcoord", fpy = "ycoord", fix_size = 40 )

resultCompare <- data.frame( SimData$passes, resultCompare )

resultCompare

# notice that changing fix_size, changed the coding of fixations 8 to 11, 20 to 23
#    and 28. Those were actually fixations on a different line but because of the
#    accuracy they are considered by the function as being on the same line.

invisible( readline( prompt = "Pres any key to continue" ) )

## Finally, from our coding we can calculate 1/ the total time spent in eacht AOI
fixDur( data = SimData, fixTime = "fixTime", 
        passes = "AOI" )

invisible( readline( prompt = "Pres any key to continue" ) )

# 2/ the total time of first pass forward and rereading and of second pass per AOI
fixDur( data = SimData, fixTime = "fixTime", 
        passes = "passes" )

# END DEMO
