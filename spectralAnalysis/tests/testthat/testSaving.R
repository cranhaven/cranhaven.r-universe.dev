# TODO: Add comment
# 
# Author: ablommaert
###############################################################################

context( "saving and reading to JSON format is compatible  " )



## do calculations 
spectra         <-  getSpectraInTimeExample()
#spectraNMFComp  <-  spectralNMF( spectra , rank = 2 , nruns = 2)
directory       <-  tempdir() 
saveSpectra(  spectra , directory )
experimentName  <-  getExperimentName( spectra )
file            <-  file.path( directory , paste0( experimentName , ".txt")  )
spectraRead     <-  readSpectra( file )

#saveSpectra( spectraNMFComp , directory = directory , name = "spectraCompTest" )
#spectraNMFCompRead   <-  readSpectra(  file.path( directory , paste0( "spectraCompTest" , ".txt" )  ) , class = "SpectraInTimeComp"  ) # problem converting back S4 object
#
#saveRDS( spectraNMFCompRead , file.path( directory , "spectraCompTest.Rds"  ) )
#test  <-  readRDS( file.path( directory , "spectraCompTest.Rds"  ) )




unlink( directory )

test_that( "read in object the same as saved object " , {
      expect_equal( str(spectra) , str( spectraRead) , info = "equal structure of object" ) 
      expect_equal( getSpectra(spectra) , getSpectra( spectraRead) , info = "equal structure of spectra" ) 
      expect_equal( spectra , spectraRead , info = "equal object" )  
    }
)

# not not used for spectra in time
#test_that( "read in object of class SpectraInTimeComp the same as saved object " , {
#      expect_equal( str(spectraNMFComp) , str( spectraNMFCompRead) , info = "equal structure of object" ) 
#      expect_equal( getDimensionReduction(spectraNMFComp) , getDimensionReduction( spectraNMFCompRead) , info = "equal structure of spectra" ) 
#    }
#)


