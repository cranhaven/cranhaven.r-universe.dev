#Returns KEEL jar files path
getExePath <- function(){

  exePath = system.file("exe", "", package="RKEEL")

  return(exePath)
}
