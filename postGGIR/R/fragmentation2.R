 
#' @title Fragmentation Metrics
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.
#'
#' @param x \code{integer} \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#'
#' @return A list with elements
#' \item{mean_r}{mean sedentary bout duration}
#' \item{mean_a}{mean active bout duration}
#' \item{SATP}{sedentary to active transition probability}
#' \item{ASTP}{bactive to sedentary transition probability}
#' \item{Gini_r}{Gini index for active bout}
#' \item{Gini_a}{Gini index for sedentary bout}
#' \item{h_r}{hazard function for sedentary bout}
#' \item{h_a}{hazard function for active bout}
#' \item{alpha_r}{power law parameter for sedentary bout}
#' \item{alpha_a}{power law parameter for active bout}
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr %>% as_data_frame filter
#' @importFrom accelerometry bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
#'
#' @references Junrui Di, Andrew Leroux, Jacek Urbanek, Ravi Varadhan, Adam P. Spira, Jennifer Schrack, Vadim Zipunnikov.
#' Patterns of sedentary and active time accumulation are associated with mortality in US adults: The NHANES study. bioRxiv 182337; doi: https://doi.org/10.1101/182337
#'
#' @details Metrics include
#' mean_bout (mean bout duration),
#' TP (between states transition probability),
#' Gini (gini index),
#' power (alapha parameter for power law distribution)
#' hazard (average hazard function)
#'
#'
 


fragmentation2 = function(
  x,
  w,
  thresh ,
  bout.length = 1,
  metrics = c("mean_bout","TP","Gini","power","hazard","all")
){
  value = NULL
  rm(list = c("value"))

  metrics = match.arg(metrics)

  if(!is.integer(x)){
    # print("Activity counts are not integers for ENMO data!") #gw 5/4/2021
  }

  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension!")
  }


  if(length(x) != length(w)){
    stop("x and w should have the same length!")
  }

  uwear = as.integer(unique(c(w)))

  if (!all(uwear %in% c(0, 1, NA))) {
    stop("w has non 0-1 data!")
  }

  x = na.omit(as.integer(x))
  w = na.omit(w)

  w[w == 0] = NA
  y = bouts(counts = x, thresh_lower  = thresh, bout_length = bout.length)
  yw = y * w

  uy = unique(na.omit(yw))
  if (length(uy) == 1) {
    #stop("Only one state found in the activity, no transition defined.")

      if(metrics == "mean_bout"){
       frag = list(mean_r = NA, mean_a = NA)
      }

      if(metrics == "TP"){
       frag = list(SATP = NA, ASTP = NA)
      }

      if(metrics == "Gini"){
        frag = list(Gini_r = NA, Gini_a = NA)
      }

      if(metrics == "power"){
        frag = list(alpha_r = NA, alpha_a = NA)
      }

      if(metrics == "hazard"){
        frag = list(h_r = NA, h_a = NA)
      }

      if (metrics == "all"){
      frag = list(mean_r = NA, mean_a = NA,
                  SATP = NA, ASTP = NA,
                  Gini_r = NA,
                  Gini_a = NA,
                  alpha_r = NA,
                  alpha_a = NA,
                  h_r =  NA,
                  h_a = NA
      )
      }
  }


  if (length(uy) > 1) {
  mat = as_data_frame(rle2(yw)) %>%
    filter(!is.na(value))

  A = mat$length[which(mat$value == 1)]
  R = mat$length[which(mat$value == 0)]

  if(metrics == "mean_bout"){
    frag = list(mean_r = mean(R), mean_a = mean(A))
  }

  if(metrics == "TP"){
    frag = list(SATP = 1/mean(R), ASTP = 1/mean(A))
  }

  if(metrics == "Gini"){
    frag = list(Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T))
  }


  if(metrics == "power"){
    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    frag = list(alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))))

  }

  if(metrics == "hazard"){
    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk))
  }

  if(metrics == "all"){

    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(mean_r = mean(R), mean_a = mean(A),
                SATP = 1/mean(R), ASTP = 1/mean(A),
                Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T),
                alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))),
                h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk)
                )
  }}

  return(frag)
}



#' @title Fragmentation Metrics for Whole Dataset
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.This function is a whole dataset wrapper for \code{fragmentation}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minutes of activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.ID can be either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequencey of days within each subject.
#'
#' @param thresh threshold to define the two states.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#' @param by Determine whether fragmentation is calcualted by day or by subjects (i.e. aggregate bouts across days).
#' by-subject is recommended to gain more power.
#'
#'
#'
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{\code{numeric} vector indicating the sequencey of days within each subject. }
#' \item{mean_r}{mean sedentary bout duration}
#' \item{mean_a}{mean active bout duration}
#' \item{SATP}{sedentary to active transition probability}
#' \item{ASTP}{bactive to sedentary transition probability}
#' \item{Gini_r}{Gini index for active bout}
#' \item{Gini_a}{Gini index for sedentary bout}
#' \item{h_r}{hazard function for sedentary bout}
#' \item{h_a}{hazard function for active bout}
#' \item{alpha_r}{power law parameter for sedentary bout}
#' \item{alpha_a}{power law parameter for active bout}
#'
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>%
#' @importFrom dplyr do as_data_frame filter
#' @importFrom accelerometry bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
#' @details Metrics include
#' mean_bout (mean bout duration),
#' TP (between states transition probability),
#' Gini (gini index),
#' power (alapha parameter for power law distribution)
#' hazard (average hazard function)
#'
#'
#'




fragmentation_long2 = function(
  count.data,
  weartime,
  thresh,
  bout.length = 1,
  metrics = c("mean_bout","TP","Gini","power","hazard","all"),
  by = c("day","subject")
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))


  metrics = match.arg(metrics)
  by = match.arg(by)


  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }
    n1440 <- ncol(count.data) - 2 #gw 5/4/21 for 30 seconds data etc
    n1441 <- n1440 + 1            #gw 5/4/21 for 30 seconds data etc 
    n1442 <- n1440 + 2            #gw 5/4/21 for 30 seconds data etc 
    n2880 <- 2 * n1440            #gw 5/4/21 for 30 seconds data etc



  if(by == "day"){
    mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))

    result.list =  apply(mat,1,function(x){
      fragmentation2(x[1:n1440],x[n1441:n2880],thresh = thresh,bout.length = bout.length, metrics = metrics) #gw 5/4/2021
    })

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }

    if(metrics != "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }

    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","Day","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","Day","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Day","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","Day","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","Day","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID","Day","mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }
  }

  if(by == "subject"){

    long.count = reshape(count.data, varying = names(count.data)[3:n1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
    long.count = long.count[
      with(long.count, order(ID, Day, MIN)),
      ]


    long.wear = reshape(weartime, varying = names(weartime)[3:n1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
    long.wear= long.wear[
      with(long.wear, order(ID, Day,MIN)),
      ]


    longdata = data.frame(ID = long.count$ID, count = long.count$values, wear = long.wear$values)

    result= longdata  %>% group_by(ID) %>% do(out = fragmentation2(.$count,.$wear,thresh = thresh,
     bout.length = bout.length, metrics = metrics))   #gw 5/4/21 for fragmentation2

    if (NA %in% as.numeric(as.character(result$ID))){
      idlist =  as.character(result$ID)} else  idlist = as.numeric(as.character(result$ID))      #gw 5/4/21 for non-numeric ID


    result.list = result$out

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(idlist,
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }
    if(metrics != "all"){
      frag_all = as.data.frame(cbind(idlist,
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }



    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID", "mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }

    row.names(frag_all) = c(1:length(idlist))
  }

  return(frag_all)

}



# edited on 5/4/21 based ActFrag