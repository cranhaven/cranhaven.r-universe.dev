

#' Test_Leaf
#' The function is used to generate Leaf P-values for case-control study.
#' Users can input the leaf information through argument struct_map.
#' If there is not leaf information e.g. struct_map=NULL, Test_Leaf will automatically construct leaf.
#' Argument thresh_val specifies the leaf size constructed from the function.
#' When the argument Gmat_case and Gmat_ctrl is null, Test_Leaf will automatically generate those matrices.
#' @import tidyverse dplyr
#' @importFrom tibble rowid_to_column
#' @importFrom methods as
#' @importFrom data.table ":=" uniqueN data.table setkey setDT rleid
#' @importFrom stats uniroot pchisq splinefunH na.omit integrate pchisq pnorm qnorm dhyper glm model.matrix
#' @importFrom reshape2 colsplit
#' @import Matrix
#' @param snp_dat an optional data frame containing patients mutation information. If \code{snp_dat=NULL}, the mutation information should be taken from \code{Gmat_case}, \code{Gmat_ctrl} and \code{glm_input}. See vignettes for detail.
#' @param thresh_val a positive integer for leaf size.
#' @param teststat the statistic used to derive p-value. Must be one of "FET" (default) or "score".
#' @param covars an optional vector about the name of covariates to be considered in the fitting process. Should be NULL (default) or a character vector.
#'
#' @return a dataframe of rejected leafs with snp information.
#' @export
#'
#' @examples
#' data("snp_dat")
#'
#' # Set leaf size M
#' M <- 5
#'
#' #Construct leaves and generate leaf p-value.
#' p.leaf <- Test_Leaf(snp_dat=snp_dat,thresh_val=M)
#' summary(p.leaf)
#'
Test_Leaf <- function(snp_dat=NULL,thresh_val=10,covars=NULL,teststat="FET"){
  struct_map <- construct_leafs(snp_dat=snp_dat,thresh_val=thresh_val)
  total_leaves <- uniqueN(struct_map$L1)
  #Global objects
  D_approx_prev = FD_approx_prev = 0

  glm_input <- snp_dat%>%
    dplyr::select(c("Sample.Name","Sample.Type",covars))%>%
    distinct_all()%>%
    arrange(Sample.Name)%>%
    mutate(types=ifelse(Sample.Type=="case",1,0))%>%
    dplyr::select(c("types",covars))

  N <- uniqueN(snp_dat$Sample.Name)
  nsnp <- max(as.integer(snp_dat$snpID))
  # Get Gmat case and control
  mat_all <- new("dgTMatrix",
                 i = as.integer(snp_dat$Sample.Name-1),
                 j = as.integer(snp_dat$snpID-1), x=rep(1,nrow(snp_dat)),
                 Dim = c(N, nsnp))
  N1=sum(glm_input$types)
  N0=N-N1

  leaf_mat_all <- create_leaf_attribute(mat_all,struct_map)
  leaf_mat_all@x <- ifelse(leaf_mat_all@x>0,1,0)

  if(teststat=="FET"){
    #Get marginals - qualifying variants for cases and all
    case_colSums = Matrix::colSums(leaf_mat_all[1:N1,])
    all_colSums = Matrix::colSums(leaf_mat_all)

    pvals.1 <- calcFETpval_per_leaf(N1=N1,N0=N0,
                                    case_colSums = case_colSums,
                                    all_colSums = all_colSums,
                                    midp=TRUE)
    pvals.1 <- pmin(pvals.1,1)

  } else{
    if(ncol(glm_input)>1){
      #With covariates
      score.test <- ScoreTest_fastSPA_sparse(genomat=leaf_mat_all, #leaf_mat_all is an Nxm matrix of leaf attributes
                                             pheno=glm_input$types,cov=glm_input[,covars],
                                             minmac=1,Cutoff=2)
    } else{
      #Without covariates
      score.test <- ScoreTest_fastSPA_sparse(genomat=leaf_mat_all, #leaf_mat_all is an Nxm matrix of leaf attributes
                                             pheno=glm_input,cov=NULL,
                                             minmac=1,Cutoff=2)
    }
    score.1 <- score.test$Tstat.sign
    pvals.1 <- score.test$p.value
  }

  Z.1 <- qnorm(1-pvals.1)
  pvals.1[is.na(pvals.1)] <- 1
  Z.1[is.na(pvals.1)] <- -100
  Z.1 <- pmin(Z.1,100)
  Z.1 <- pmax(Z.1,-100)
  S <- NULL
  #Do not test the leaf with pvals.1=1###########check!!!!
  struct_map0 <- data.table("L1"=seq_along(pvals.1),"pvals"=pvals.1)%>%
    left_join(struct_map,by="L1")%>%filter(pvals!=1)%>%#mutate(L0=L1)%>%
    #mutate(L0=rleid(L1))%>%
    mutate("Test"=teststat)

  return(struct_map0)
}


#' DYNATE
#' Function to conduct hierarchical mutiple testing based on the leaf p-values
#' @import tidyverse dplyr
#' @importFrom tibble rowid_to_column
#' @importFrom methods as
#' @importFrom data.table ":=" uniqueN data.table setkey setDT rleid
#' @importFrom stats uniroot pchisq splinefunH na.omit integrate pchisq pnorm qnorm dhyper glm model.matrix
#' @importFrom reshape2 colsplit
#' @import Matrix
#' @param struct_map a data frame with both leaf information and P-value information.
#' @param L maximum number of layers
#' @param alpha desired FDR
#'
#' @return a data frame with testing results.
#' @export
#'
#' @references
#' Li, Xuechan, Anthony Sung, and Jichun Xie. "Distance Assisted Recursive Testing." arXiv preprint arXiv:2103.11085 (2021).
#' Pura, John, et al. "TEAM: A Multiple Testing Algorithm on the Aggregation Tree for Flow Cytometry Analysis." arXiv preprint arXiv:1906.07757 (2019).
#'
#' @examples
#' data("p_leaf")
#'
#' # Set tuning parameters
#' L <- 3 # layer number
#' alpha <- 0.05 # desired FDR
#'
#' # conduct dynamic and hierarchical testing based on the leaf level p values.
#' out <- DYNATE(struct_map=p_leaf,L=L,alpha=alpha)
#' summary(out)
DYNATE <- function(struct_map,
                  L=5,
                  alpha=0.05){

  total_leaves <- uniqueN(struct_map$L1)

  struct_map <- data.table(struct_map)%>%mutate(L0=L1,L1=rleid(L1))
  pvals.1 <- struct_map[,c("pvals","L1")]%>%distinct_all()
  pvals.1 <- c(struct_map$pvals)

  # get the estimated mixed node structure
  total_leaves <- uniqueN(struct_map$L1)
  hatn1 <- ceiling(sqrt(total_leaves))
  pn1 <- pvals.1[rank(pvals.1,ties.method="first")==hatn1]

  setkey(struct_map,snpID)
  struct_map <- struct_map %>% mutate(hatm1=(pvals<=pn1),pvals1=pvals) %>%
    group_by(L1)%>%mutate(wt=1/n())%>%ungroup()

  struct_map_ <- struct_map

  Sps <- NULL
  p1s <- p0s <- NULL
  smap_res <- NULL
  S<-NULL
  D_approx_prev = FD_approx_prev = 0
  for(l in seq(L)){
    Ll <- paste0("L",l)
    Lm1 <- paste0("L",l-1)
    if(l>1){
      #First remove domains with fewer than 2^(l-1) L1 leaves
      removed_map <- struct_map%>%
        group_by(domainID)%>%filter_at(Lm1,any_vars(uniqueN(.)<2))%>%
        ungroup()
      struct_map <- struct_map%>%
        group_by(domainID)%>%filter_at(Lm1,any_vars(uniqueN(.)>=2))%>%
        ungroup()

      setDT(struct_map)
      setkey(struct_map, domainID)
      struct_map <- struct_map[, (Ll) := {
        ## modify rle values
        x <- ceiling(rleid(get(Lm1)) / 2)
        n <- uniqueN(get(Lm1))
        if(n %% 2==1){
          x[x == x[.N]] <- x[.N] - 1
        }
        x
      }, by = .(domainID)][, (Ll) := rleid(domainID,get(Ll))] #reassign groups by domain and Ll

      struct_map <- struct_map%>%group_by_at(Ll)%>%#mutate(wt=)%>%
        mutate(pvals=stouffer.p2(pvals1,L1,wt))%>%ungroup()
      Sps <- samp.pvals.leaf_L1(struct_map,Ll,p1s=p1s,p0s=p0s)
    }

    m.l = uniqueN(data.frame(struct_map)[,Ll])
    pvals.l=struct_map%>%dplyr::select(c("pvals",all_of(Ll)))%>%distinct_all()
    #Obtain layer specific-threshold
    p.hat = est.p.hat_samp(l=l,
                           D_prev = D_approx_prev,
                           FD_prev = FD_approx_prev,
                           pvals_l=c(pvals.l$pvals),
                           alpha=alpha,
                           alpha1=NULL,
                           Sps=Sps)
    p.hat.l=p.hat$p.hat
    D_approx_prev = p.hat$D_approx_prev
    FD_approx_prev = p.hat$FD_approx_prev
    if(l==1){
      pvals.1 <- c(pvals.l$pvals)
      S.l1 <- which(pvals.1 <= p.hat$p.hat1)
      if(length(S.l1)==0){
        p1s=min(pvals.1)
        p0s=pvals.1[-which.min(pvals.1)]
      }else{
        p1s=pvals.1[S.l1]
        p0s=pvals.1[-S.l1]

        #new (R(1-alpha)), more conservative, 02242022
        p1s1=sort(p1s)[seq(floor(length(p1s)*(1-alpha)))]
        p1s2=sort(p1s)[-seq(floor(length(p1s)*(1-alpha)))]
        p1s=p1s1
        p0s=c(p0s,p1s2)
        #new end
      }
    }
    rej_map <- struct_map%>%filter(pvals<=p.hat.l)%>%mutate(Layer=l)
    smap_res <- smap_res%>%bind_rows(rej_map)
    struct_map <- struct_map%>%filter(pvals>p.hat.l)
    if(nrow(struct_map)==0){next}
  }
  smap_res <- smap_res%>%mutate(L1=L0)%>%
    dplyr::select(-c("pvals","hatm1","wt","L0","L2","L3"))

  return(smap_res)
}


############# AUXILIARY FUNCTIONS #################
construct_leafs <- function(snp_dat,
                            thresh_val=10){
  ##Leaf construction
  t1 <- Sys.time()
  #function(thresh, ID, domain_end,snp_end)
  struct_map <- data.table(snp_dat) %>%
    arrange(domainID,snpID)%>%
    mutate_at("Sample.Name",as.character) %>%
    #Coerce ID from factor to character
    group_by(domainID) %>%
    mutate(lastObsFlagDomain = as.integer(row_number() == n())) %>%
    group_by(snpID,.add=TRUE) %>%
    mutate(lastObsFlagSnp = as.integer(row_number()==n())) %>%
    ungroup() %>%
    mutate(num_comb = compute_counts(thresh_val, Sample.Name,
                                     lastObsFlagDomain,lastObsFlagSnp)) %>%
    mutate(num_unique = colsplit(num_comb,",",c(1:2))[,1]) %>%
    mutate(num_all = colsplit(num_comb,",",c(1:2))[,2]) %>%
    mutate(group = cumsum(c(-1L, diff(num_all)) <= 0L)) %>%
    group_by(domainID) %>%
    mutate(group2 = ifelse(group==max(group) & last(num_unique) < thresh_val,
                           max(max(group)-1L,min(group)),group)) %>%
    ungroup() %>%
    mutate(L1 = rleid(group2)) %>%
    dplyr::select(-c(contains("group"),lastObsFlagDomain,num_unique,num_comb,num_all)) %>%
    mutate_at("Sample.Name",as.factor) %>% #coerce ID back to factor
    data.table() #15-37s, based on N0 size

  struct_map <- struct_map %>% dplyr::select(c(snpID,L1,domainID)) %>% distinct(snpID,L1,domainID)
  setkey(struct_map,snpID)
  t2 <- Sys.time()

  return(struct_map)
}


est.p.hat_samp <- function(l,D_prev,FD_prev,pvals_l,alpha,alpha1=NULL,Sps=NULL){

  ##print(paste("l:",l))
  if(is.null(alpha1)) {alpha1=alpha}
  m.l = length(pvals_l)

  #Threshold very small p-values based on constant
  p.m = ifelse(m.l==0,1,min(1/(m.l*sqrt(log(m.l))),0.05))

  filter <- which(pvals_l<=p.m|pvals_l>alpha1)

  if(length(filter)>0){
    p.vec = unique(sort(pvals_l[-filter],decreasing = FALSE))
  }else {
    p.vec = unique(sort(pvals_l,decreasing = FALSE))
  }
  if(length(p.vec)==0){p.vec=p.m}

  p.indx = 0
  emp.fdr = 0

  addi <- 0
  if(!is.null(Sps)){
    propct <- Sps$sps$ct*Sps$sps$altprop
    delnames <- intersect(c("type","altct","nullct","altprop","ct"),colnames(Sps$sps))
    sps <- Sps$sps%>%dplyr::select(-delnames)

    rows=nrow(sps);cols=ncol(sps)
    if(rows==1){
      sps0 <- sapply(sps,function(x){propct%*%outer(x,p.vec,"<=")})
      addi <- mean(sps0)
    }else{
      sps0 <- sapply(sps,function(x){colSums(diag(propct)%*%outer(x,p.vec,"<="))})
      if(length(p.vec)==1){
        addi <- mean(sps0)
      }else{
        addi <- rowMeans(sps0)
      }

    }
  }

  fdr.num = FD_prev + m.l*p.vec+addi
  fdr.denom = D_prev + sapply(p.vec,function(x){sum(pvals_l<=x)})
  emp.fdr = fdr.num/pmax(fdr.denom,1)
  p.vec <- c(p.m,p.vec)
  index <- max(which(c(0,emp.fdr)<=alpha))
  index1 <- max(which(c(0,emp.fdr)<=alpha1))
  p.hat <- p.vec[index]
  p.hat1 <- p.vec[index1]
  D_approx_prev=fdr.denom[index]
  FD_approx_prev=min(fdr.num[index],D_approx_prev)

  return(list("p.hat"=p.hat,"p.hat1"=p.hat1,
              "FD_approx_prev"=FD_approx_prev,
              "D_approx_prev"=D_approx_prev))
}



####### Functions to compute test-statistics

fisher.exact.test <- function(z,midp=TRUE){

  x <- z[1]
  sampTot <- z[2]
  pop1Tot <- z[3]
  pop2Tot <- z[4]

  lo <- max(0L, sampTot - pop2Tot)
  hi <- min(sampTot, pop1Tot)

  support <- lo : hi
  out <- dhyper(support, pop1Tot, pop2Tot, sampTot)

  if(midp){
    #mid p-val with minimum likelihood method
    return(sum(out[out < out[x - lo + 1]]) + sum(out[out==out[x-lo+1]])/2)
  } else{
    #minimum likelihood method
    return(sum(out[out <= out[x - lo + 1]]))
  }
}

calcFETpval_per_leaf <- function(N1,N0,case_colSums,all_colSums,
                                 midp=TRUE){

  #Order of input is: sample hit, sample size, pop1 size, pop2 size
  cont_tab_summ2x2 <- unname(cbind(case_colSums,all_colSums,N1,N0))

  # Apply row-wise
  FET_pvals <- apply(cont_tab_summ2x2, 1,
                     function(z) fisher.exact.test(z,midp = midp))

  #Vector of length m.l, where m.l is number of leaves/hypotheses at layer l
  return(FET_pvals)

}

compute_counts <- function(thresh, ID, domain_end,snp_end) {
  see_idss <- seen_ids <- NULL
  count <- 0L
  countall <- 0L
  adjust_count <- function(id, domain_end,snp_end) {
    if (!(id %in% seen_ids)) {
      seen_ids <<- c(seen_ids,id)
      count <<- count + 1L
    }
    countall <<- countall+ 1L

    if ((snp_end & (uniqueN(seen_ids) >= thresh))|domain_end) {
      count <- count # copy enclosed value locally
      countall <- countall
      seen_ids <<- NULL
      count <<- 0L
      countall <<- 0L
    }
    paste(count,countall,sep=",")
  }
  unlist(Map(adjust_count, ID, domain_end, snp_end))
}


create_leaf_attribute <- function(snp_mat,snp_leaf_map){

  snp_mat@Dimnames <- list(NULL,NULL)

  #Convert mat to dgTMatrix if not already
  if(!inherits(snp_mat,"dgTMatrix")){
    snp_mat <- as(snp_mat, "dgTMatrix")
  }

  snp_mat2 <- snp_mat #copy object

  #Replace column indices with new set of indices
  #Make sure initial indices start with zero
  snp_mat2@j <- as.integer(snp_leaf_map[.(snp_mat@j+1)]$L1-1)
  #Correct dimensions of new matrix
  smij <- distinct_all(data.frame(snp_mat2@i,snp_mat2@j))
  snp_mat2@i <- smij[,1]
  snp_mat2@j <- smij[,2]
  snp_mat2@Dim <- as.integer(c(nrow(snp_mat2),
                               length(unique(snp_leaf_map$L1))))

  #Convert to dgCMatrix
  y <- as(snp_mat2,"dgCMatrix")
  return(y)
}

stouffer.p <- function(x){
  pnorm(sum(x)/sqrt(length(x)),lower.tail=FALSE)
}

stouffer.p2 <- function(y,g,wt){
  x <- qnorm(1-y)
  out=pnorm(sum(x*wt)/sqrt(uniqueN(g)),lower.tail=FALSE)
  return(rep(out,length(x)))
}




samp.pvals.leaf_L1 <- function(struct_map,Ll,Nsamp=100,p1s=NULL,p0s=NULL){
  # change to a more efficient algorithm
  # sampling (check the probability of the entile mixed population)
  delname <- intersect(c("snpID","alt_snps"),colnames(struct_map))
  struct_map2=struct_map%>%group_by_at(Ll)%>%
    mutate(mixed=((prod(hatm1)+prod(!hatm1))==0),
           altprop=sum(!hatm1)/length(hatm1),#altprop here means the estimated proportion of false rejection
           altct=sum(hatm1),
           nullct=sum(!hatm1))%>%ungroup()%>%
    dplyr::select(-delname)%>%distinct_all()
  if(any(sum(struct_map2$mixed)==0,length(p1s)==0)){return(NULL)}else{
    struct_map2 <- struct_map2%>%filter(mixed)%>%arrange(hatm1)
    struct_map3 <- struct_map2%>%group_by(altct,nullct,altprop)%>%
      summarize(ct=n())%>%ungroup()%>%rowid_to_column(var="type")
    sp1s <- sample(p1s,Nsamp*sum(struct_map3$altct),replace=TRUE)
    sp0s <- sample(p0s,Nsamp*sum(struct_map3$nullct),replace=TRUE)
    Sp1s <- data.frame(matrix(qnorm(sp1s,lower.tail=FALSE),ncol=Nsamp))%>%
      mutate(type=rep(struct_map3$type,time=struct_map3$altct))
    Sp0s <- data.frame(matrix(qnorm(sp0s,lower.tail=FALSE),ncol=Nsamp))%>%
      mutate(type=rep(struct_map3$type,time=struct_map3$nullct))
    sps <- rbind(Sp1s,Sp0s)%>%group_by(type)%>%
      summarise_at(paste0("X",seq(Nsamp)), stouffer.p)%>%ungroup()%>%
      left_join(struct_map3,by="type")
    return(list("sps"=sps,"M"=nrow(struct_map2),
                "altprop"=struct_map2$altprop))
  }
}




