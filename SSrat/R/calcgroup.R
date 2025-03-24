#' Calculates sociometric status determinations of a specified single group
#' from a SSrat compliant dataframe 
#' 
#' In a group, all group members are asked to rate all other group members on a
#' rating scale. This rating scale can have 3 (1..3), 5 (1..5), 7 (1..7) or 9
#' (1..9) rating points. The rating scale has a neutral mid point (respectively
#' 2, 3, 4 and 5). \cr Application of SSrat::calcgroup calculates a
#' classification into five categories of sociometric status, which is labeled
#' as follows: (1) popular, (2) rejected, (3) negelected, (4) controversial and
#' (5) average. 
#' 
#' It is assumed that the data are gathered on a single (2R+1)-point rating
#' scale. For a 7-point scale, R = 3. The scale midpoint must represent a
#' neutral judgment. The data are arranged in a matrix P, with rows belonging
#' to assessors and columns belonging to the assessed.Let P[i, k] denote the
#' rating given by assessor i to group member k.\cr First P* scores are
#' calculated by subtracting R+1 from the values of P. The Sympathy scores S
#' are calculated by replacing all negative scores of P* by zero. The Antipathy
#' score A are calculated by replacing all positive scores of P* by zero and
#' taking the absolute value. P* = S - A.The Preference score P are equal to
#' the ratings obtained. The Impact scores I are calculated by taking the
#' absolute values of P*. I = S + A.\cr In the next step, sum scores are
#' calculated over columns. Then the distributions of these sumscores are
#' calculated, using the principle of convolution. Lastly, the positions in the
#' distributions are calculated to identify persons with scores in the areas of
#' the lower and higher alpha percent. This allows us to translate the criteria
#' of Coie et al. (1983) into probability terms. A person k obtains the
#' following social determinations (E is expected value): Popular = sum(P[,k])
#' significantly high, sum(S[,k]) > E(sum(S[,k])) and sum(A[,k]) <
#' E(sum(A[,k])); Rejected = sum(P[,k]) significantly low, sum(S[,k]) <
#' E(sum(S[,k])) and sum(A[,k]) > E(sum(A[,k])); Neglected = sum(I[,k])
#' significantly low, sum(S[,k]) < E(sum(S[,k])) and sum(A[,k]) <
#' E(sum(A[,k])); Controversial = sum(I[,k]) significantly high, sum(S[,k]) >
#' E(sum(S[,k])) and sum(A[,k]) > E(sum(A[,k])); Average = remaining group
#' members.
#' 
#' When the criteria of Newcomb & Bukowski (1993) are applied, the most liked
#' nominations LM are the ratings > R and the least liked nominations LL are
#' the ratings < R, and the impact score SI = LM + LL. The criteria for a
#' person k are: Popular = sum(LM[,k]) significantly high, sum(LL[,k]) <
#' E(sum(LL[,k])); Rejected = sum(LL[,k]) significantly high, sum(LM[,k]) <
#' E(sum(LM[,k])); Neglected = sum(SI[,k]) significantly low; Controversial =
#' sum(LM[,k]) significantly high and sum(LL[,k] > E(sum(LL[,k])) or
#' sum(LL[,k]) significantly high and sum(LM[,k] > E(sum(LM[,k])); Average =
#' remaining group members.
#' 
#' @param schoolid The schoolnumber that identifies the school. Default = 1. 
#' @param groupid The groupnumber that identifies the group. Default = 1. 
#' @param dataframe The dataframe with the rating data. This dataframe should
#' have columns schoolid, groupid, respid, and for n raters columns r01 to rn,
#' with a maximum of r99. Function readratdatfixed can be used to create such a
#' dataframe from a prepared text file.
#' @param scalelength Either 3, 5, 7 or 9. Default = 5.
#' @param alpha The significance levels to be applied to the probability
#' distributions of the four total scores that have been derived from the
#' ratings. By choosing an appropriate alpha, the user can fine tune the
#' criterion for the status determination. A list of various alphas can be
#' supplied. Default is the list (.10, .05, .01).
#' @param NBcriteria A boolean. When TRUE, the classification criteria of
#' Newcomb & Bukowski (1983) will be applied, in stead of the SSrat criteria.
#' These criteria are applicable to rating scales of length 3. When this option
#' is selected with longer scales, the midscore is conversed to 2, alls cores
#' larger than the midscore are conversed to 3 and all scores lower than the
#' midscore are conversed to 1. When another recoding scheme of the scores is
#' preferred, the input ratings should be recoded to 1, 2 and 3 before the use
#' of this function (use \code{car::recode}).
#' @param printresult Boolean which identifies whether the calculated results
#' should be shown. Default is False.
#' @return \item{dataframe}{dataframe with the most relevant results, as
#' calculated for each respondent by SSrat} \item{dataframe$schoolid}{school id
#' as entered} \item{dataframe$groupid}{group id as entered}
#' \item{dataframe$respid}{respondent id as entered}
#' \item{dataframe$nrAss}{number of assessors who have given a valid rating}
#' \item{dataframe$tr.S}{total rating Sympathy} \item{dataframe$tr.A}{total
#' rating Antipathy} \item{dataframe$tr.P}{total rating Preference}
#' \item{dataframe$tr.I}{total rating Impact} \item{dataframe$SS.xx}{Social
#' Determination as attributed by SSrat, applying alpha = .xx. Defaults to
#' SS.10, SS.05 and SS.01 } \item{S}{matrix of Sympathy ratings}
#' \item{A}{matrix of Antipathy ratings} \item{P}{matrix of Preferences}
#' \item{I}{matrix of Impact scores} \item{intermediate$pls}{probability
#' referring to left-sided testing of tr.S} \item{intermediate$prs}{probability
#' referring to right-sided testing of tr.S} \item{intermediate$es}{expected
#' value of tr.S} \item{intermediate$pla}{probability referring to left-sided
#' testing of tr.A} \item{intermediate$pra}{probability referring to
#' right-sided testing of tr.A} \item{intermediate$ea}{expected value of tr.A}
#' \item{intermediate$plp}{probability referring to left-sided testing of tr.P}
#' \item{intermediate$prp}{probability referring to right-sided testing of
#' tr.P} \item{intermediate$ep}{expected value of tr.P}
#' \item{intermediate$pli}{probability referring to left-sided testing of tr.I}
#' \item{intermediate$pri}{probability referring to right-sided testing of
#' tr.I} \item{intermediate$ei}{expected value of tr.I}
#' @author Hans Landsheer
#' @seealso \code{\link{readratdatafixed}} 
#' @references Coie, J.D., & Dodge, K.A. (1983). Continuities and changes in
#' children's social status: A five-year longitudinal study. Merril-Palmer
#' Quarterly, 29, 261-282.\cr Newcomb, A. F., & Bukowski, W. M. (1983). Social
#' impact and social preference as determinants of children's peer group
#' status. Developmental Psychology, 19, 856-867.\cr Maassen, G. H. and
#' Landsheer, J. A. (1998). SSRAT: The processing of rating scales for the
#' determination of two-dimensional sociometric status. Behavior Research
#' Methods Instruments &amp; Computers, 30(4), 674-679.
#' @keywords list List
#' @examples
#' 
#' data(example5.rat)
#' # calc SSRAT results fot this group
#' out =calcgroup (school=1, group=23, data=example5.rat, scalelength="3")
#' out$dataframe
#' 
#' # calc Newcomb & Bukowski results for this group
#' out =calcgroup (school=1, group=23, data=example5.rat, scalelength="3", NBcriteria=TRUE)
#' out$dataframe
#' 
#' # calc Newcomb & Bukowski results for example1
#' data(example1.rat)
#' out =calcgroup (school=1, group=1, data=example1.rat, scalelength="7", NBcriteria=TRUE)
#' out$dataframe
#' 
#' # calc SSrat results for example1
#' out =calcgroup (school=1, group=1, data=example1.rat, scalelength="7")
#' out$dataframe
#'
#' @importFrom stats convolve
#' 
#' @export
calcgroup <- function(schoolid = 1, groupid=1, dataframe, scalelength = c(5, 3, 7, 9), 
                      alpha = c(0.10, 0.05, 0.01), NBcriteria = F, printresult = F) {
  
  getsapi <- function(schoolid = 1, groupid=1, dataframe, scalelength , NBcriteria = FALSE) {
    #sc = as.numeric(match.arg(as.character(scalelength)))  #sc=7;schoolid=1 ;groupid=16
    sc=as.numeric(scalelength)
    sel = dataframe$schoolid == schoolid & dataframe$groupid == groupid  #dataframe=DF; school=3; group=2
    lastcol = ncol(dataframe)
    # count columns with name equal rn
    maxrated = sum(grepl("^r\\d", names(dataframe)))
    # ratings may be padded with NA
    maxnrrated = maxrated - 1
    # get ratings
    grr = dataframe[sel, (lastcol - maxnrrated):lastcol]
    lastrow = nrow(grr)
    if (lastrow == 0) {
      stop("Empty data selection. Please check school and group identification.")
    }

    grr=grr[ , ! apply( grr , 2 , function(x) all(is.na(x)) ) ] #remove cols with only NA
    rownames(grr) <- dataframe[sel, ]$respid
    
    if ((max(grr, na.rm = T) > sc) || (min(grr, na.rm = T) < 1) || (ncol(grr) == 
                                                                      0)) {
      stop("Invalid ratings found in dataframe")
    }
    nrresp = nrow(grr)
    if (nrresp <= 1) 
      stop(cat("Number of assessors (", nrresp, ") in this group <= 1.\n"))
    
    R = (sc + 1)/2
    
    P = grr
    nrAssessed <- apply(P, 1, function(x) {
      sum(!is.na(x))
    })  # nr ratings given
    nrAssessors <- apply(P, 2, function(x) {
      sum(!is.na(x))
    })  # nr ratings received
    
    if (NBcriteria) {
      P[(P > 1) & (P < sc)] <- 2
      P[P == sc] <- 3
      sc = 3
      R = 2
    }
    
    S = P - R
    S[S < 0] <- 0
    
    A = P - R
    A[A > 0] <- 0
    A = abs(A)
    
    I = A + S
    
    list(names = dataframe[sel, ]$names, S = S, A = A, P = P, I = I, 
         nrAssessed = nrAssessed, nrAssessors = nrAssessors)
  }  #end getsapi
  
  assessors <- function(subject) {
    if (subject <= ncol(SAPI$P)) {
      which(!is.na(SAPI$P[, subject]))
    } else NULL
  }
  # subject=1
  probtotrating <- function(subject, probmatrix) {
    a = assessors(subject)
    pr = probmatrix[a[1], ]
    for (i in a[2:length(a)]) {
      pr = convolve(pr, rev(probmatrix[i, ]), type = "o")
    }
    return(pr)
  }
  
 
#start
  sslabel = c("Popular", "Rejected", "Neglected", "Controversial", 
              "Average")
  sc = match.arg(as.character(scalelength), choices=c(3,5,7,9)) #sc=7 
  R = (sc + 1)/2
  if (!all(alpha < 1) || !all(alpha > 0)) 
    warning("Invalid number in alpha vector")
  sel = dataframe$schoolid == schoolid & dataframe$groupid == groupid
  SAPI = getsapi(schoolid, groupid, dataframe[sel, ], sc, NBcriteria=NBcriteria)  #dataframe=dataframe[sel,]
  
  # get probabilities Sp, Ap, Pp, Ip
  nrAors = length(SAPI$nrAssessed)
  Sp = matrix(0, nrAors, R)  #i=0
  for (i in 0:(R - 1)) {
    Sp[, i + 1] = apply(SAPI$S, 1, function(x) {
      mean(x == i, na.rm = T)
    })
  }
  Ap = matrix(0, nrAors, R)  #i=0
  for (i in 0:(R - 1)) {
    Ap[, i + 1] = apply(SAPI$A, 1, function(x) {
      mean(x == i, na.rm = T)
    })
  }
  
  Pp = matrix(0, nrAors, sc + 1)
  for (i in 0:sc) {
    Pp[, i + 1] = apply(SAPI$P, 1, function(x) {
      mean(x == i, na.rm = T)
    })
  }

Ip = matrix(0, nrAors, R)  #i=0
  for (i in 0:(R - 1)) {
    Ip[, i + 1] = apply(SAPI$I, 1, function(x) {
      mean(x == i, na.rm = T)
    })
  }
  
  # get total scores
  tr.S = colSums(SAPI$S, na.rm = T)
  tr.A = colSums(SAPI$A, na.rm = T)
  tr.P = colSums(SAPI$P, na.rm = T)
  tr.I = colSums(SAPI$I, na.rm = T)
  hscores = matrix(c(0:(R - 1)), ncol = 1, nrow = R)
  scores = matrix(c(0:(sc)), ncol = 1, nrow = sc + 1)
  
  # imr = structure for intermediate results: probabilty leftsided
  # testing (pl), probability rightsided testing (pr) and expected
  # value (e) for S, A, P and I
  nrAed = length(SAPI$nrAssessors)
  imr = matrix(0, nrow = nrAed, ncol = 12)
  colnames(imr) = c("pls", "prs", "es", "pla", "pra", "ea", "plp", 
                    "prp", "ep", "pli", "pri", "ei")
  
  status = matrix(0, nrow = nrAed, ncol = length(alpha))
  colnames(status) <- 1:dim(status)[2]
  
  # i=7; i=8; SAPI$assessed
  for (i in (1:nrAed)) {
    ass = assessors(i)
    if (is.null(ass)) {
      break
    }
    # sympathy
    cptr.Sp = cumsum(probtotrating(i, Sp))
    imr[i, "pls"] = cptr.Sp[tr.S[i] + 1]
    imr[i, "prs"] = ifelse(tr.S[i] == 0, 1, 1 - cptr.Sp[tr.S[i]])
    imr[i, "es"] = sum(Sp[ass, ] %*% hscores)
    
    # antipathy
    cptr.Ap = cumsum(probtotrating(i, Ap))
    imr[i, "pla"] = cptr.Ap[tr.A[i] + 1]
    imr[i, "pra"] = ifelse(tr.A[i] == 0, 1, 1 - cptr.Ap[tr.A[i]])
    imr[i, "ea"] = sum(Ap[ass, ] %*% hscores)
    
    # social preference
    cptr.Pp = cumsum(probtotrating(i, Pp))
    imr[i, "plp"] = cptr.Pp[tr.P[i] + 1]
    imr[i, "prp"] = ifelse(tr.P[i] == 0, 1, 1 - cptr.Pp[tr.P[i]])
    imr[i, "ep"] = sum(Pp[ass, ] %*% scores)
    
    # social impact
    cptr.Ip = cumsum(probtotrating(i, Ip))
    imr[i, "pli"] = cptr.Ip[tr.I[i] + 1]
    imr[i, "pri"] = ifelse(tr.I[i] == 0, 1, 1 - cptr.Ip[tr.I[i]])
    imr[i, "ei"] = sum(Ip[ass, ] %*% hscores)
    
    # j=2
    colnames(status) <- paste("SS", substr(sprintf("%.2f", alpha), 
                                           2, 4), sep = "")
    for (j in 1:length(alpha)) {
      if (NBcriteria) {
        status[i, j] = sslabel[5]
        if ((imr[i, "prs"] <= alpha[j]) & (tr.A[i] < imr[i, "ea"])) {
          status[i, j] = sslabel[1]
        } else if ((imr[i, "pra"] <= alpha[j]) & (tr.S[i] < imr[i, 
                                                                "es"])) {
          status[i, j] = sslabel[2]
        } else if (((imr[i, "prs"] <= alpha[j]) & (tr.A[i] > imr[i, 
                                                                 "ea"])) | ((imr[i, "pra"] <= alpha[j]) & (tr.S[i] > 
                                                                                                             imr[i, "es"]))) {
          status[i, j] = sslabel[4]
        } else if (imr[i, "pli"] <= alpha[j]) {
          status[i, j] = sslabel[3]
        }
      } else {
        status[i, j] = sslabel[5]
        if ((imr[i, "prp"] <= alpha[j]) & (tr.S[i] > imr[i, "es"]) & 
              (tr.A[i] < imr[i, "ea"])) {
          status[i, j] = sslabel[1]
        } else if ((imr[i, "plp"] <= alpha[j]) & (tr.S[i] < imr[i, 
                                                                "es"]) & (tr.A[i] > imr[i, "ea"])) {
          status[i, j] = sslabel[2]
        } else if ((imr[i, "pri"] <= alpha[j]) & (tr.S[i] > imr[i, 
                                                                "es"]) & (tr.A[i] > imr[i, "ea"])) {
          status[i, j] = sslabel[4]
        } else if ((imr[i, "pli"] <= alpha[j]) & (tr.S[i] < imr[i, 
                                                                "es"]) & (tr.A[i] < imr[i, "ea"])) {
          status[i, j] = sslabel[3]
        }
      }
    }
  }
  schoolid = rep(schoolid,nrAed) #create vectors
  groupid = rep(groupid, nrAed)
  respid = as.numeric(gsub("r",'',colnames(SAPI$P)))
  nrAss = SAPI$nrAssessors
  out = data.frame(schoolid, groupid, respid, nrAss, tr.S, 
                            tr.A, tr.P, tr.I, status, stringsAsFactors=FALSE)
  if ("resplabel" %in% colnames(dataframe[sel, ])) {
    resplabel = dataframe[sel, ]$resplabel
    out = cbind(resplabel, out)
  }
  row.names(out) <- NULL
  outlist = list(dataframe = out, S = SAPI$S, A = SAPI$A, P = SAPI$P, 
                 I = SAPI$I, intermediate = imr)
  if (printresult) {
    print(out)
  }
  invisible(outlist)
  
}
