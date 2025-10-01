versions <- function(){
    tryCatch(rversions::r_versions(), error = function(e){NA})
}
