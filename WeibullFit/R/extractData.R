extractData <- function (rename=FALSE, pGroupNewName="parcela", sGroupNewName="Idade",primaryGroup="parcela", secondaryGroup="idadearred", subsetting=TRUE, restrVal,
                         pValue="dap", dataFrame,
                         newDataFile=NA) {

  dataFrameName  <-  deparse(substitute(dataFrame))
  filteredDF <- data.frame()
  if(rename) {
    filteredDF <- sqldf(glue("SELECT {primaryGroup} AS {pGroupNewName},
		                         {secondaryGroup} AS {sGroupNewName},
		                         {pValue} FROM {dataFrameName}
		                         WHERE {pValue} > 0
		                         order by {primaryGroup}, {pGroupNewName}, {pValue}")
    )
    dataFrameName <-  as.character(quote(filteredDF))
    if(subsetting) {
      filteredDF <- sqldf(glue("SELECT * FROM {dataFrameName} WHERE {sGroupNewName} = {restrVal}"))
    }
  } else {
    filteredDF <- sqldf(glue("SELECT {primaryGroup},{secondaryGroup},
		                         {pValue} FROM {dataFrameName}
		                         WHERE {pValue} > 0
		                         order by {primaryGroup}, {secondaryGroup}, {pValue}")
    )
    if(subsetting) {
      dataFrameName  <-  as.character(quote(filteredDF))
      filteredDF <- sqldf(glue("SELECT * FROM {dataFrameName} WHERE {secondaryGroup} = {restrVal}"))
    }

  }
  if(!is.na(newDataFile)) {
    save(filteredDF, file=paste0(newDataFile,".RData"))
  }

  return (filteredDF)
}
