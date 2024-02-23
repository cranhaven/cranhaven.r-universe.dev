# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




#'\code{ObtainCandidate} Asks for a vector and returns a value along with the range it is contained in the attribute
#' Is used alongside other functions when generating a new sample
#' @param Dado vector containing an attribute of your dataframe
#' @return Obtains a sample of an attribute based on their possible values, it is a part of the \code{Generate} function
#' #' \itemize{
#'   \item Retorno - A value of the same class as the function input
#' }
ObtainCandidate<-function(Dado){

  if(class(Dado) %in% c('factor','character'))
  {
    Distribuicao=cumsum(table(Dado)/sum(table(Dado )))
    JogarDado=stats::runif(1,0,1)
    Candidates=which(Distribuicao>=JogarDado)
    Retorno=rep(names(Distribuicao)[Candidates[1]],1)
    #Retorno=rep(names(Distribuicao)[Candidates[1]],3)
  }else{
    if(length(unique(Dado))>1){
      Histograma=graphics::hist(Dado,plot=FALSE)
      Intervalos=Histograma$breaks
      Distribuicao=cumsum(Histograma$counts/sum(Histograma$counts))
      JogarDado=stats::runif(1,0,1)
      Candidates=which(Distribuicao>=JogarDado)[1]
      if(Candidates==1 | length(Candidates )==0 )
        Candidates=2
      Retorno=c(stats::runif(1,Intervalos[Candidates-1],Intervalos[Candidates]),Intervalos[Candidates-1],Intervalos[Candidates] )
    }else{
      Retorno=c(unique(Dado),unique(Dado),unique(Dado))
    }
  }
  return(Retorno)

}


#'\code{Generate} Asks for a dataframe and generates a new sample
#' returns novel sample along with intervals it contained to revalidate it using confidence levels
#' @param data Dataframe
#' @param regression if we are to generate data for regression or classification (will the data be conditioned on a certain class)
#' @return if regression is true returns a dataframe, if false returns a list.
#' #' \itemize{
#'   \item Frame - Datafrane containing generated sample
#'   \item Info information to be used by GenerateASingleCandidate to check if the sample satisfies the confidence condition
#' }
#' @examples
#' # basic usage of Generate
#' Generate(iris,regression=TRUE)


#'@export

Generate<-function(data,regression=FALSE){

  l=list()
  l2=list()
  Frame=as.data.frame(t(data.frame(1:ncol(data))) )

  for(i in 1:ncol(data)){
    l[[i]]=ObtainCandidate(data[,i])
    if(length(l[[i]])==1){
      data=data[data[,i]==l[[i]],]
      Frame[1,i]=l[[i]]
      l2[[i]]=rep(l[[i]] ,2  )
    }else{
      data=data[data[,i]>=l[[i]][2] & data[,i]<=l[[i]][3],]
      #l[[i]]=l[[i]][1]
      Frame[1,i]=l[[i]][1]
      l2[[i]]=l[[i]][2:3]
    }
  }
  #return(data.frame(Reduce(cbind,l) ) )
  if(regression)
    return(Frame)

  return(list(Frame=Frame,Info=l2) )
}



#'\code{GenerateASingleCandidate} Generates a novel sample from a target class and evaluate it against the other classes to check if it satisfies the confidence level
#'returns NA if the generated sample does not satisfy the condition, otherwise returns novel sample
#' @param data A dataframe containing available data
#' @param Class The target class
#' @param col Column the target class is located in the dataframe
#' @param Prob Minimum confidence level to generate sample
#' @return A dataframe containing a novel sample if it satisfies the confidence given in Prob, otherwise NA
#' #' \itemize{
#'   \item Alvo - dataframe containing a novel sample
#' }

GenerateASingleCandidate<-function(data,Class,col,Prob){ #data se refere ao teu conjunto de treinamento, Class a classe que vai Generate dados
  #data[,col]=as.numeric(as.character(data[,col]))
  Cand=Generate(data[data[,col]==Class,]  )
  Alvo=Cand$Frame
  Probs=Cand$Info
  filtro=data
  for(i in 1:(length(Probs)) ){
    if(i!=col){
      if(class(filtro[,i]) %in% c('factor','character') ){
        filtro=filtro[ filtro[,i]==Probs[[i]][1], ]
      }else{
        filtro=filtro[Probs[[i]][1] <= filtro[,i] & Probs[[i]][2] >= filtro[,i],]
      }
    }
  }
  Distribuicao=table(filtro[,col] )/sum(table(filtro[,col ] ) )
  if(dim(filtro)[1]==0  )
    return(NA)
  if(length(Distribuicao)==0)
    return(NA)
  if(is.na(sum(Distribuicao)) )
    return(NA)
 # print(Distribuicao)
  if(as.numeric(Distribuicao[which(names(Distribuicao)==Class)]>=Prob)  )
    return(Alvo)
  return(NA)

}




#'\code{GenerateMultipleCandidates} Asks for a dataframe and some parameters and returns multiple novel samples from the target class
#' @param data Reference dataframe
#' @param Class Value of the target class
#' @param col column of the dataframe which contains the class
#' @param Prob Minimum confidence level to generate the sample
#' @param amount Number of novel samples to be generated
#' @return A dataframe containing novel samples of the class \code{Class} that satisfies \code{Prob} confidence.
#' #' \itemize{
#'   \item Dados - dataframe containing the novel samples
#' }
#' @examples
#' # basic usage of GenerateMultipleCandidates
#' GenerateMultipleCandidates(iris,Class='virginica',col=5,Prob=0.3,amount=10)

#'@export

GenerateMultipleCandidates<-function(data,Class,col,Prob,amount){
  Dados=list()
  for(i in 1:amount){
    Dados[[i]]=GenerateASingleCandidate(data,Class,col,Prob)
  }
  Dados=do.call(rbind,Dados)
  Dados=Dados[stats::complete.cases(Dados),]
  if(class(Dados) != 'data.frame')
    return (NA)
  names(Dados)=names(data)
  return(Dados)
}
