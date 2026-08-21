
.onAttach<- function(libname, pkgname){
  packageStartupMessage("**************************************************************************************************\n",
                        pkgname," ",packageDescription("EFA.dimensions")$Version,
                        "\n\nPlease contact Brian O'Connor at brian.oconnor@ubc.ca if you have questions or suggestions.\n",
                        "**************************************************************************************************", 
                        appendLF = TRUE)
}

NULL

