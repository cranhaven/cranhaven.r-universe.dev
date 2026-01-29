require(downloader)
require(utils)
require(openssl)

#Downloads a file from a mirror, and stores it in the file_path. It also checks the md5sum
downloadFromMirror <- function(mirror, file_path, md5_sum) {
  tryCatch( 
    {
      downloader::download(url = mirror, destfile = file_path, mode = "wb", timeout=120)
      
      # Check md5 sum
      if(as.character(md5(file(file_path, open = "rb"))) == md5_sum){
        return(1)
      }
      else{
        unlink(file_path)
        return(-1)
      }
    }, 
    error = function(e) {
      return(-1)
    }
  )
}


.onLoad <- function(libname = find.package("RKEEL"), pkgname = "RKEEL") {

  #List of jar files to download
  downloadList <- c()

  jarList <- getJarList()

  for(jar in jarList){

    jarPath <- getJarPath()
    if(substr(jarPath, nchar(jarPath), nchar(jarPath)) != "/"){
      jarFile <- paste0(jarPath, "/", jar)
    }
    else{
      jarFile <- paste0(jarPath, jar)
    }

    if(!file.exists(jarFile)){
      downloadList <- c(downloadList, jar)
    }
  }

  #If any jar file is missing, download them
  if(length(downloadList) > 0){

    # libPath = system.file("exe", package="RKEEL") # paste(.libPaths()[1], "/RKEEL", sep="")
    downloadedJarFile <- file.path(system.file("exe", package="RKEEL"), "RKEELjars_1.1.zip") #  "/tmp/RKEELjars.zip" # system.file("exe", package="RKEEL")
    md5_sum = "d09bda2a58378012dcf1fe02125a4442"
    
    #packageStartupMessage("Download RKEEL jars")

    #Try to download from three different mirrors: US, UCO, and UGR
    dCode = downloadFromMirror("https://personal.us.es/jmoyano1/RKEELjars_1.1.zip", downloadedJarFile, md5_sum)
    if(dCode < 0){
      dCode = downloadFromMirror("https://www.uco.es/users/jmoyano/RKEELjars_1.1.zip", downloadedJarFile, md5_sum)
      if(dCode < 0){
        dCode = downloadFromMirror("https://ugr.es/~jmoyano/RKEELjars_1.1.zip", downloadedJarFile, md5_sum)
        if(dCode < 0){
          warning("Jar files could not be downloaded. Please try again later.")
          return(0)
        }
      }
    }
  
    # tryCatch( 
    #   { 
    #     downloader::download(url = mirror1, destfile = downloadedJarFile, mode = "wb", timeout=120)
    #     }, 
    #   error = function(e) {
    #     tryCatch( 
    #       { 
    #         downloader::download(url = "https://ugr.es/~jmoyano/RKEELjars_1.1.zip", destfile = downloadedJarFile, mode = "wb", timeout=120)
    #         }, 
    #       error = function(e) {
    #         warning("Jar files could not be downloaded. Please try again later")
    #         }
    #       )
    #     },
    #   warning = function(w) {
    #     warning("Cannot download from UCO mirror. Trying other.")
    #     tryCatch( 
    #       {
    #         downloader::download(url = "https://ugr.es/~jmoyano/RKEELjars_1.1.zip", destfile = downloadedJarFile, mode = "wb", timeout=120)
    #         }, 
    #       error = function(e) {
    #         warning("Jar files could not be downloaded. Please try again later")
    #         }
    #     )
    #     }
    #   )

    # Unzip file and remove it
    utils::unzip(zipfile = downloadedJarFile, exdir = system.file("exe", package="RKEEL")) # "/tmp") #
    unlink(downloadedJarFile)
  } #If all jars are downloaded, not download again

}
