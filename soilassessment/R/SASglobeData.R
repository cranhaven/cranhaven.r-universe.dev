SASglobeData=function(dframe="ecse",ISO="ISO", Region=NULL){
  drive_deauth()
  temp <- tempfile(fileext = ".zip")
  if(is.null(Region)){
  if(missing(ISO)){stop("ISO code is missing")}

  {  if(dframe=="ecse"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:5,15,16)]
    dframe=dframe[dframe$ECse==1,]
    }
  else if(dframe=="ec2"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,6,15,16)]
    dframe=dframe[dframe$EC2==1,]
    }
  else if(dframe=="ec2.5"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,7,15,16)]
    dframe=dframe[dframe$EC2.5==1,]
  }
  else if(dframe=="ec5"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,8,15,16)]
    dframe=dframe[dframe$EC5==1,]
  }
  else if(dframe=="ph"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,9,15,16)]
    dframe=dframe[dframe$ph==1,]
  }
  else if(dframe=="phkcl"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,10,15,16)]
    dframe=dframe[dframe$phkcl==1,]
  }
  else if(dframe=="phcacl2"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,11,15,16)]
    dframe=dframe[dframe$phcacl2==1,]
  }
  else if(dframe=="sand"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,12,15,16)]
    dframe=dframe[dframe$Sand==1,]
  }
  else if(dframe=="silt"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,13,15,16)]
    dframe=dframe[dframe$Silt==1,]
  }
    else if(dframe=="clay"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:4,14,15,16)]
    dframe=dframe[dframe$Clay==1,]
  }} ##Region NUll and ISO not missing

  Ddata=dframe[dframe$ISO==ISO,]
  if(ISO=="All"){Ddata=dframe}

  }
else {
  if(missing(ISO)){stop("ISO code is missing")}
  {  if(dframe=="ecse"){
    patch <- drive_download(
      as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
    out <- unzip(temp, exdir = tempdir())
    dframe <- read.csv(out, sep = ",")
    dframe=dframe[,c(1:5,15,16)]
    dframe=dframe[dframe$ECse==1,]
  }
    else if(dframe=="ec2"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,6,15,16)]
      dframe=dframe[dframe$EC2==1,]
    }
    else if(dframe=="ec2.5"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,7,15,16)]
      dframe=dframe[dframe$EC2.5==1,]
    }
    else if(dframe=="ec5"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,8,15,16)]
      dframe=dframe[dframe$EC5==1,]
    }
    else if(dframe=="ph"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,9,15,16)]
      dframe=dframe[dframe$ph==1,]
    }
    else if(dframe=="phkcl"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,10,15,16)]
      dframe=dframe[dframe$phkcl==1,]
    }
    else if(dframe=="phcacl2"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,11,15,16)]
      dframe=dframe[dframe$phcacl2==1,]
    }
    else if(dframe=="sand"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,12,15,16)]
      dframe=dframe[dframe$Sand==1,]
    }
    else if(dframe=="silt"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,13,15,16)]
      dframe=dframe[dframe$Silt==1,]
    }
    else if(dframe=="clay"){
      patch <- drive_download(
        as_id("1o3jNM5dSy1svfiqKWTvD7qLFHCqDSM23"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
      out <- unzip(temp, exdir = tempdir())
      dframe <- read.csv(out, sep = ",")
      dframe=dframe[,c(1:4,14,15,16)]
      dframe=dframe[dframe$Clay==1,]
    }}
      Ddata=dframe[dframe$Regions==Region,]
      Ddata=Ddata[Ddata$ISO==ISO,]
  if (ISO=="All"){Ddata=dframe[dframe$Regions==Region,]}

  }
  return(Ddata)
}
