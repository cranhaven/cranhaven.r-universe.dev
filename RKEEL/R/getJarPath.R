#Returns RunKeel.jar path
getJarPath <- function(){

  jarPath = system.file("exe", "", package="RKEEL")

  return(jarPath)
}
