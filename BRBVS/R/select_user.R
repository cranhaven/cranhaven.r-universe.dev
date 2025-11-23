#' Variable Selection Based on User-Defined Measures
#'
#' @param m.rank A list containing rankings for the criteria selected. It is typically obtained from the `rbvs.user` function.
#' @param kmax The maximum number of variables to be selected.
#' @param tau A user-defined threshold for variable selection.
#'
#' @return A vector of selected variables.
#'
#' @noRd
select.user=function(m.rank, kmax, tau){



  #lo fisso qui sennò da probemi
  # prova, va eliminato
  # questa funzione consente di selezionare il numero di variabili in relazione alle probability stimate quando la measure è "user"
  # kmax # numero massimo di variabili che vengono selezionate
  frequencies=vector("numeric", length=kmax)
  var.k <- rep(0, length=1)


  #tab.sort=sort(table(m.rank$CE[[i]][kk,]), decreasing=TRUE)
  kk=1

  #m.rankNthCE <- data.frame(matrix(0, ncol=2, nrow=length(m.rank)))
  m.rank1st <- numeric()

  for(i in 1:length(m.rank)){
    m.rank1st[i]<- m.rank[[i]][kk,2]
    #tab.sort=sort(table(m.rankNthCE[,2]), decreasing=TRUE)
    #v.n=names(tab.sort)
    #var.k[kk]=v.n[1] #inizializza l'algoritmo con la variabile che presenta massima frequenza
    #frequencies[kk]=as.numeric(tab.sort)[1]
  }
  tab.sort=sort(table(m.rank1st), decreasing=TRUE)
  v.n=names(tab.sort)
  var.k[kk]=v.n[1] #inizializza l'algoritmo con la variabile che presenta massima frequenza
  frequencies[kk]=as.numeric(tab.sort)[1]



  for(kk in 2:kmax){
    m.rank1st <- numeric()
    for(i in 1:length(m.rank)){
      m.rank1st[i]<- m.rank[[i]][kk,2]

    }
    tab.sort=sort(table(m.rank1st), decreasing=TRUE)
    v.n=names(tab.sort)
    #var.k[kk]=v.n[1]
    #frequencies[kk]=as.numeric(tab.sort)[1]
    varkk=1
    while(sum(var.k==v.n[varkk])!=0){ # questo controllo mi evita di prendere in considerazione una variabile già selezionata
      varkk=varkk+1}
    var.k=c(var.k, v.n[varkk]) # aggiunge a var.k la covariata diversa dalle top
    cnt=0
    ##ok
    for (vv in 1:length(m.rank)){
      if(sum( m.rank[[vv]][1:kk,2] %in% var.k)==kk)
        cnt=cnt+1}
    if(cnt>0) frequencies[kk]=cnt else frequencies[kk]=1

  }

  ###mi manca tutta la parte in cui elimino i doppioni etc...
  ### rivedere bene dall' originale

  scores=frequencies/c(100, frequencies[1:(kmax-1)])  # frequenze relative (la prima è divisa per il numero dei subsamples come in rbvs)
  score.r=scores[2:kmax]^tau/scores[1:(kmax-1)]  # 0.5 è il valore di tau (come indicato nell'algoritmo Baranowski et al.)
  active=var.k[1: which.min(score.r)]


  return(list(frequencies=frequencies, active=active, score.r=score.r, scores.rel=score.r))
}
