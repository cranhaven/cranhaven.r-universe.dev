#' @import utils
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",">"))
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' Computation of the autocorrelation
#'
#' The function autocor computes the autocorrelation.(function from localscore)
#' @param x A numeric vector.
#' @return the autocorrelation.

autocor <- function(x){abs(cor(x[-1],x[-length(x)]))}


#' Computation of the lindley process from scores.
#'
#' The function lindley computes the lindley process from scores.(function from localscore)
#' @param scores A numeric vector.
#' @return the lindley.

lindley <- function(scores){
  L=length(scores)
  sl=rep(0,L+1)
  for (i in 1:L){
    sl[i+1]= max(0, (sl[i]+scores[i]))
  }
  return(sl[-1])
}

#' Computation of the significance threshold
#'
#' The function thresUnif computes the significance threshold.(function from localscore)
#' @param L The length of the chromosome
#' @param cor The autocorrelation of the chromosome
#' @param xi The threshold of the score, xi = 1,2,3 or 4.
#' @param alpha The nominal threshold.
#' @details The distribution of the p-values is uniform, the local score follows a Gumbel distribution under the null.
#' @return the significance threshold.
thresUnif <- function(L, cor, xi, alpha = 0.05){
  coefs=list(list('a'=c(-5.5,6.76,-5.66,-2.51),'b'=c(-1.22,3.17,-1.99)),
             list('a'=c(2.47,-4.16,-1.82,-4.58),'b'=c(0.37,2.14,-2.35)),
             list('a'=c(2.04,-5.76,1.04,-6.95),'b'=c(2.55,-0.02,-2.31)),
             list('a'=c(0.22,-4.08,1.16,-9.16),'b'=c(3.45,-0.98,-2.33))
  )
  cors=c(cor^3,cor^2,cor,1)
  if (missing(xi) | !(xi %in% 1:4)){
    stop('xi should be 1, 2, 3 or 4')
  }else{
    a=log(L)+ coefs[[xi]]$a%*%cors
    b=coefs[[xi]]$b %*%cors[-1]
    #then compute the threshold:
    thres = ( log(-log(1-alpha)) - a ) / b
    return(thres)
  }
}



#' Computation of the significative regions
#'
#' The function sig_sl computes the significative regions from a lindley process given a significance threshold.(function from localscore)
#' @param lind The lindley
#' @param pos The position
#' @param th The threshold
#' @return the significance threshold.
#' @importFrom data.table data.table
#'
#'

sig_sl<- function(lind,pos, th){
  zones=c(0,0,0)
  list=lind
  auxpos=pos
  while(max(list)>=th){
    M_loc=which.max(list)
    if(length(which(list[1:M_loc]==0))==0){ #the peak is at the beginning of the chrom
      m_loc=1
      zones=rbind(zones, c(auxpos[m_loc],auxpos[M_loc],max(list)))
      tmp=min(which(list[(M_loc+1):length(list)]==0)) #first 0 score after peak
      list=list[(M_loc+tmp):length(list)]
      auxpos=pos[(M_loc +tmp):length(list)]
    }else{
      m_loc=max(which(list[1:M_loc]==0))
      max=max(list)
      zones=rbind(zones, c(auxpos[m_loc+1],auxpos[M_loc],max))
      tmp=which(list[(M_loc+1):length(list)]==0) #first 0 score after peak
      if (length(tmp)>0){
        auxpos=auxpos[c(1:m_loc,(min(tmp)+M_loc):length(list))]
        list=list[c(1:m_loc, (min(tmp)+M_loc):length(list))]
      }else{ #if the peak is at the end of the chromosome
        auxpos=auxpos[1:m_loc]
        list=list[1:m_loc]
      }
    }
  }
  zones=matrix(zones, ncol=3)
  zones=data.table::data.table(beg=zones[,1],end=zones[,2],peak=zones[,3])
  if (nrow(zones)>1){zones=zones[-1,]}
  return(zones)
}

#' Compute the local score from a set of pvalues.
#'
#' The function metaGE.lscore computes the local score and the significant regions from
#' a set of pvalues.
#' @param Data A dataset containing the following columns: CHR, POS, MARKER and \code{PvalName}.
#' @param PvalName The name of the column containing the p-value.
#' @param xi The threshold of the score, \code{xi} = 1,2,3 or 4.
#' @details  This function is directly inherited from the scorelocalfunctions.R R code file of Fariello MI, Boitard S, Mercier S, et al.,
#' as available on the <https://forge-dga.jouy.inra.fr/projects/local-score> website.
#' The technical details of the computation can be found in Fariello MI, Boitard S, Mercier S, et al.
#' Accounting for linkage disequilibrium in genome scans for selection without individual genotypes: The local score approach.
#' \doi{10.1111/mec.14141}.
#' The function computes a local score for the detection of significant regions based on the hypothesis that the H0 distribution of the pvalues is
#' uniform. Under this hypothesis the local score follows a Gumbel distribution (under H0) whose parameters depend
#' on the threshold \code{xi} and on the autocorrelation between pvalues within each chromosome. The threshold has
#' to be selected in 1,2,3,4 and the autocorrelation is computed internally.
#' @return A list with the following elements:
#'\tabular{ll}{
#' \code{Data} \tab The dataset \code{Data} with the local score as supplementary column.\cr
#' \code{SigZones} \tab A dataset containing information about the significant regions.\cr
#' \code{SigMarker} \tab A dataset containing the significant markers. \cr
#' \code{ChrThreshold} \tab A dataset containing the chromosome-wide significance thresholds.
#' }
#' 
#' @importFrom data.table as.data.table setkey
#' @export
#' @examples
#' require(dplyr)
#' # Import the data
#' data("metaData")
#'
#' # Compute the inter-environment correlation matrix
#' matCorr <- metaGE.cor(metaData, Threshold = 0.8)
#'
#' # Fit the Fixed Effect model
#' FeDF <- metaGE.fit(metaData, matCorr, Method = "Fe")
#'
#' # Compute the score local
#' xi <- 2
#' FeScore <- metaGE.lscore(FeDF,"PVALUE", xi)
#' #FeScore$SigZones


metaGE.lscore <- function(Data,PvalName,xi){

  ## Shape a bit
  mydata <- Data %>%
    rename(chr = .data$CHR,
           pos = .data$POS,
           pval = all_of(PvalName)) %>%
    select(.data$chr, .data$pos, .data$MARKER, .data$pval) %>% arrange(.data$chr, .data$pos)
  mydata <- data.table::as.data.table(mydata)

  ## Handling of NA
  NA.pval <- sum(is.na(mydata$pval))!=0
  if(NA.pval){
    mydata_all <- mydata
    notNA.index <- which(!is.na(mydata_all$pval))
    mydata <- mydata[notNA.index,]
  }

  ## Sort the dataset
  mydata <- mydata %>% data.table::setkey("chr")
  Nchr <- length(factor(mydata$chr) %>% levels())

  ## Computation of absolute position in the genome.
  #This is useful for doing genomewide plots.
  chrInfo <- mydata %>% group_by(.data$chr) %>% summarise(L=n(), cor=autocor(.data$pval))

  chrInfo <- chrInfo %>% as.data.table()
  chrInfo %>% setkey("chr")

  # Computation of score and lindley
  mydata <- mydata %>% mutate(score = -log10(.data$pval)-xi,
                              lindley = NA)
  map(1:Nchr,~{
    chrIndex <- which(mydata$chr==chrInfo$chr[.x])
    mydata$lindley[chrIndex] <<- lindley(mydata$score[chrIndex])
  })

  # Compute significance threshold for each chromosome
  ## Uniform distribution of p-values
  thresUnif_v <- Vectorize(thresUnif)
  chrInfo <- chrInfo %>% mutate(th =as.numeric(thresUnif_v(.data$L,.data$cor, xi)))
  mydata=mydata[chrInfo]

  ## Find regions
  sigZones <- map_df(1:nrow(chrInfo), function(ii){
    data_chr <- mydata %>% filter(.data$chr== chrInfo$chr[ii])
    sigZones <-sig_sl(data_chr$lindley, data_chr$pos, unique(data_chr$th))
    return(sigZones %>% mutate(chr = chrInfo$chr[ii]))
  })

  sigZones <- sigZones %>% filter(.data$peak!=0)
  
  if(nrow(sigZones)>0){
    Cand <- lapply(1:nrow(sigZones), function(ii){
      mydata %>% filter(.data$pos>=sigZones$beg[ii] & .data$pos<=sigZones$end[ii] & .data$chr==sigZones$chr[ii]) %>% select(.data$chr, .data$pos,.data$MARKER,.data$pval,.data$lindley)
    })
    DT.cand <-  map(1:length(Cand), ~mutate(Cand[[.x]],Region=.x, Peak=sigZones$peak[.x],
                                            chr=sigZones$chr[.x])) %>% bind_rows()
    
    Cand <- DT.cand %>%  select(-.data$Peak) %>% rename(CHR=.data$chr,POS=.data$pos,SCORE=.data$lindley,PVALUE=.data$pval,REGION=.data$Region)
    
    DT.cand <- DT.cand %>%
      group_by(.data$Region) %>%
      summarise(CHR=mean(.data$chr),Start = min(.data$pos),
                End=max(.data$pos),Size=n(),
                PvalMin=min(.data$pval),
                PvalMax=max(.data$pval),
                PosPvalMin=.data$pos[which.min(.data$pval)],
                MarkerPvalMin=.data$MARKER[which.min(.data$pval)],
                LocalScoreMax=max(.data$lindley),
                LocalScoreMin=min(.data$lindley),
                PosLSMax=.data$pos[which.max(.data$lindley)],
                MarkerLSMax=.data$MARKER[which.max(.data$lindley)]) %>%
      as.data.frame()
  }else{
    DT.cand <- NULL
    Cand <- NULL
  }
 

  ## Handling of NA
  if(NA.pval){
    mydata_all$lindley <- NA
    mydata_all$lindley[notNA.index] <- mydata$lindley
    mydata <- mydata_all
  }

  mydata <- merge.data.frame(Data, mydata %>% select(.data$MARKER, .data$lindley), by = "MARKER")
  mydata <- mydata %>% rename(SCORE = lindley) %>% arrange(.data$CHR, .data$POS)
  return(list(Data = mydata, SigZones = DT.cand, SigMarker = Cand, ChrThreshold = (chrInfo %>% select(.data$chr,.data$th))))
}
