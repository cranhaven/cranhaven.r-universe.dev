# TODO: Add comment
# 
# Author: ablommaert
###############################################################################


context( "Equivalence subsetting processInTime" )

exampleSpectra       <-  getSpectraInTimeExample()
exampleSpectraComp   <-  spectralNMF( exampleSpectra , rank = 2 , subsamplingFactor = 10 , nruns =1 )



test_that( "Identical subsetting process times " , {
      expect_equal( exampleSpectra[1:10, 1:10] , exampleSpectraComp[1:10, 1:10] )
      expect_equal( exampleSpectra[1:10, ] , exampleSpectraComp[1:10, ] )
      expect_equal( exampleSpectra[ , r(300, 350) ] , exampleSpectraComp[ , r(300, 350) ] )
      expect_equal( exampleSpectra[ , e(300, 350) ] , exampleSpectraComp[ , e(300, 350) ] )
    }
)

preprocessedSpectra    <-   smooth( baselineCorrect( normalize( exampleSpectra )   ))

test_that( "Identical preprocessing SpectraInTime and SpectraInTimeComp" , {
      expect_equal( smooth( exampleSpectra ) , smooth( exampleSpectraComp ) )
      expect_equal( baselineCorrect( exampleSpectra ) , baselineCorrect( exampleSpectraComp ) )
      expect_equal( preprocessedSpectra , preprocess( exampleSpectraComp , with = preprocessedSpectra )  )
    }
)




