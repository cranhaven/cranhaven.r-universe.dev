# spacejamr 0.2.1

    * This version fixed a bug that would cause one of the unit tests to fail.

# spacejamr 0.2
    
    * This version allows more flexibility when creating spacejamr objects. 
    Users can decide whether to use the coordinate reference system in the 
    supplied shapefile or let the as.spacejamr() method find the best projected 
    CRS.
    
    * From version 0.2 onwards this package no longer depends on the methods 
    package.
    
    * Previous versions depended on the spatstat.core package to generate 
    spatial point processes, however, the spatstat team moved this functionality 
    to the new spatstat.random package. Accordingly, this package no longer 
    depends on spatstate.core but instead depends on spatstat.random.

# spacejamr 0.1.1

    * This version fixes a bug that would cause an error when testing the
    as.spacejamr() method on Solaris.

# spacejamr 0.1

    * This is the first version of spacejamr. As bugs are fixed, functionality 
    is added, or other changes are made, they will be described here.
