##########################
##########################
##above all else code 
##(hidden here because 
## first .r code file 
## alphabetically)
##########################
##########################

#example
#utils::globalVariables(c("a", "othervar"))

globalVariables(c("pems.scheme", "ref.unit.conversions", "ref.chem", "ref.diesel", "ref.petrol",
                  "panel.surfaceSmooth", "data", "exh.flow.rate", "exh.press", "exh.temp", 
                  "local.time", "pems_speedEm2", "add", "grp", "..id..",
                  "temp", "listUpdate", "panel.number"))
# added these to above for now... listUpdate, panel.number

#might move following into function
#      exh.flow.rate exh.press exh.temp local.time
#panel.surfaceSmooth only needed until new version of loa gets on CRAN...




#setup

setup <- function(){
             print("Set up for dev:pems.utils")
             print("(this should run without errors)")

             #replace this?
             need.pack = c("lattice", "loa", "methods", "utils", "grid", "RColorBrewer", 
                           "latticeExtra", "baseline", "ggplot2", "lazyeval", "rlang", 
                           "dplyr") 
             inst.pack <- need.pack %in% installed.packages()
             if(length(need.pack[!inst.pack]) > 0) install.packages(need.pack[!inst.pack])
             
             #then after installing package, if library or require fails 
             #pems.utils:::setup()                 
         }

# c handling
#.onUnload <- function (libpath) {
#  library.dynam.unload("pems.utils", libpath)
#}

