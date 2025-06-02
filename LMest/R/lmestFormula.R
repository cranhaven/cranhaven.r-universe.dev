
lmestFormula <- function(data, response, manifest = NULL,
                          LatentInitial = NULL, LatentTransition = NULL,
                          AddInterceptManifest = FALSE, AddInterceptInitial = TRUE,
                          AddInterceptTransition = TRUE, responseStart = TRUE,
                          manifestStart = TRUE, LatentInitialStart = TRUE,
                          LatentTransitionStart = TRUE)
{

  nameData <- colnames(data)
  if(is.character(response))
  {
    resp <- vector("character",length = 0)
    if(responseStart)
    {
      for(i in 1:length(response))
      {
        resp <- c(resp,nameData[startsWith(nameData,response[i])])
      }
    }else{
      for(i in 1:length(response))
      {
        resp <- c(resp,nameData[endsWith(nameData,response[i])])
      }
    }

    if(length(resp)==0)
    {
      stop("No names match with responses")
    }
  }else{
    resp <- nameData[response]
  }


  if(is.null(manifest))
  {
    if(AddInterceptManifest)
    {
      mani <- 1
    }else{
      mani <- "NULL"
    }

  }else if(is.character(manifest))
  {

    mani <- vector("character",length = 0)

    if(manifestStart)
    {
      for(i in 1:length(manifest))
      {
        mani <- c(mani,nameData[startsWith(nameData,manifest[i])])
      }
    }else{
      for(i in 1:length(manifest))
      {
        mani <- c(mani,nameData[endsWith(nameData,manifest[i])])
      }
    }
    if(length(mani)==0)
    {
      stop("No names match with manifest")
    }
  }else{
    mani <- nameData[manifest]
  }
  if(!AddInterceptManifest & length(manifest)!=0)
  {
    mani <- c("-1",mani)
  }

  if(is.null(LatentInitial))
  {
    if(AddInterceptInitial)
    {
      initial <- 1
    }else{
      initial <- NULL
    }
  }else if(is.character(LatentInitial))
  {

    initial <- vector("character",length = 0)

    if(LatentInitialStart)
    {
      for(i in 1:length(LatentInitial))
      {
        initial <- c(initial,nameData[startsWith(nameData,LatentInitial[i])])
      }
    }else{
      for(i in 1:length(manifest))
      {
        initial <- c(initial,nameData[endsWith(nameData,LatentInitial[i])])
      }
    }
    if(length(LatentInitial)==0)
    {
      stop("No names match with covariate for initial")
    }

  }else{
    initial <- nameData[LatentInitial]
  }
  if(!AddInterceptInitial & length(initial)!=0)
  {
    initial <- c("-1",initial)
  }

  if(is.null(LatentTransition))
  {
    if(AddInterceptTransition)
    {
      transition <- 1
    }else{
      transition <- NULL
    }
  }else if(is.character(LatentTransition))
  {
    transition <- vector("character",length = 0)

if(LatentTransitionStart)
{
  for(i in 1:length(LatentTransition))
  {
    transition <- c(transition,nameData[startsWith(nameData,LatentTransition[i])])
  }
}else{
  for(i in 1:length(manifest))
  {
    transition <- c(transition,nameData[endsWith(nameData,LatentTransition[i])])
  }
}
    if(length(LatentTransition)==0)
    {
      stop("No names match with covariate for transition")
    }

  }else{
    transition <- nameData[LatentTransition]
  }
  if(!AddInterceptTransition & length(transition)!=0)
  {
    transition <- c("-1",transition)
  }

  formulaResponse <- as.formula(paste(paste(resp, collapse= "+"),paste(mani, collapse= "+"),sep = "~"))

  if(length(transition)==0 & length(initial)==0)
  {
    formulaLatent <- NULL
  }else{
    if(is.null(transition))
    {
      transition = "NULL"
    }
    if(is.null(initial))
    {
      initial = "NULL"
    }
    formulaLatent <- as.formula(paste("~", paste(paste(initial, collapse= "+"),
                                                 paste(transition, collapse= "+"),sep = "|")))
  }


  return(list(responsesFormula = formulaResponse,
              latentFormula = formulaLatent))

}
