SASdata_densityInfo=function(data="ec"){
  drive_deauth()
  temp <- tempfile(fileext = ".zip")
  if(data=="ec"){
  patch <- drive_download(
    as_id("1AndDSIl5ZIvCXNu2B_EkBxbffCUAg6m_"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
  }
  else if(data=="ph"){
    patch <- drive_download(
      as_id("13OQFLxb4SiH7Fc2Fbqwtjg0j8TldlRdr"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
  }
    else if(data=="texture"){
       patch <- drive_download(
        as_id("1a2yhsGnoPRLEda0E7hVKSip9Z3eSdChW"), path = temp, overwrite = TRUE, options(googledrive_quiet = TRUE))
     }
  out <- unzip(temp, exdir = tempdir())
  img=readPNG(out)
  grid::grid.raster(img)
}


