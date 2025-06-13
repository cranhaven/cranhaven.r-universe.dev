# write a dummy function to parse string to odeModel class

writeDummy <- function(eqList) {
  


  funcStr = paste(paste0('dx', 1:length(eqList$reac)), eqList$reac, sep = ' = ')
  measStr = paste(paste0('y', 1:length(eqList$meas)), eqList$meas, sep = ' = ')

  writeDummyFile <- function(str) {
    formatStr <- gsub(pattern = "[d]{1}||\\d", '', strsplit(split = ' = ', str)[[1]][1])

    if (formatStr == 'x') {
      funcHead <- 'modelFunc <- function(t,x,para){ \n with (as.list(parameters),{ \n'
      funcBody <- paste0('\t', str)
      funcEnd <- paste('\n\tlist(c(', paste0('x', 1:length(str), collapse = ','), '))\n})\n}')
      tmpFileName <- 'modelFunc'
    } else {
      funcHead <- 'measFunc <- function(x) {\n'
      funcBody <- paste0('\t', gsub('(x{1})([0-9]+)', replacement = '\\1[,\\2]', str))
      funcEnd <- paste('\n\treturn(cbind(', paste0('y', 1:length(str), collapse = ','), ')) \n}')
      tmpFileName <- 'measFunc'
    }

    str <- c(funcHead, funcBody, funcEnd)
    if (.Platform$OS.type != "windows"){
      temp_dummy_file <- paste0(tempdir(),'/',paste0(tmpFileName,'_tmp.R'))
    } else {
      temp_dummy_file <- paste0(tempdir(),'\\',paste0(tmpFileName,'_tmp.R'))
    }
    file.create(temp_dummy_file)
    tmp <- file(temp_dummy_file)
    writeLines(str, tmp)
    close(tmp)

    e <- new.env()
    source(temp_dummy_file, local = e)
    
    if (formatStr == 'x') {
      modelFunc <- get('modelFunc', envir = e)
      return(modelFunc)
    } else {
      measFunc <- get('measFunc', envir = e) 
      return(measFunc)
    }


  }

  reacFunc <- writeDummyFile(funcStr)
  measFunc <- writeDummyFile(measStr)
  
  return(list("reac" = reacFunc, 'meas' = measFunc))
}
