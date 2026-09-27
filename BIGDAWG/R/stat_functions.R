#' Case-Control Odds ratio calculation and graphing
#'
#' cci function port epicalc version 2.15.1.0 (Virasakdi Chongsuvivatwong, 2012)
#' @param caseexp Number of cases exposed
#' @param controlex Number of controls exposed
#' @param casenonex Number of cases not exosed
#' @param controlnonex Number of controls not exposed
#' @param cctable A 2-by-2 table. If specified, will supercede the outcome and exposure variables
#' @param graph If TRUE (default), produces an odds ratio plot
#' @param design Specification for graph; can be "case control","case-control", "cohort" or "prospective"
#' @param main main title of the graph
#' @param xlab label on X axis
#' @param ylab label on Y axis
#' @param xaxis two categories of exposure in graph
#' @param yaxis two categories of outcome in graph
#' @param alpha level of significance
#' @param fisher.or whether odds ratio should be computed by the exact method
#' @param exact.ci.or whether confidence limit of the odds ratio should be computed by the exact method
#' @param decimal number of decimal places displayed
#' @note This function is for internal BIGDAWG use only.
cci <- function (caseexp, controlex, casenonex, controlnonex, cctable = NULL, graph = FALSE, design = "cohort", main, xlab, ylab, xaxis, yaxis, alpha = 0.05, fisher.or = TRUE, exact.ci.or = TRUE, decimal = 2) {

  if (is.null(cctable)) {
    frame <- cbind(Outcome <- c(1, 0, 1, 0), Exposure <- c(1, 1, 0, 0), Freq <- c(caseexp, controlex, casenonex, controlnonex))
    Exposure <- factor(Exposure)
    expgrouplab <- c("Non-exposed", "Exposed")
    levels(Exposure) <- expgrouplab
    Outcome <- factor(Outcome)
    outcomelab <- c("Negative", "Positive")
    levels(Outcome) <- outcomelab
    table1 <- xtabs(Freq ~ Outcome + Exposure, data = frame)
  }
  else {
    table1 <- as.table(get("cctable"))
  }

  fisher <- fisher.test(table1)
  caseexp <- table1[2, 2]
  controlex <- table1[1, 2]
  casenonex <- table1[2, 1]
  controlnonex <- table1[1, 1]
  se.ln.or <- sqrt(1/caseexp + 1/controlex + 1/casenonex + 1/controlnonex)

  if (!fisher.or) {
    or <- (caseexp * controlnonex) / (casenonex * controlex)
    #p.value <- chisq.test(table1, correct = FALSE)$p.value
    p.value <- chisq.test(table1,correct=TRUE)$p.value
  } else {
    or <- fisher$estimate
    p.value <- fisher$p.value
  }

  if (exact.ci.or) {
    ci.or <- as.numeric(fisher$conf.int)
  } else {
    #ci.or <- or * exp(c(-1, 1) * qnorm(1 - alpha/2) * se.ln.or)
    ci.or <- c( exp(log(or) - ( qnorm(1 - alpha/2) * se.ln.or) ), exp(log(or) + ( qnorm(1 - alpha/2) * se.ln.or) ) )
  }

  if (graph == TRUE) {
    caseexp <- table1[2, 2]
    controlex <- table1[1, 2]
    casenonex <- table1[2, 1]
    controlnonex <- table1[1, 1]
    if (!any(c(caseexp, controlex, casenonex, controlnonex) <
             5)) {
      if (design == "prospective" || design == "cohort" ||
          design == "cross-sectional") {

        if (missing(main))
          main <- "Odds ratio from prospective/X-sectional study"
        if (missing(xlab))
          xlab <- ""
        if (missing(ylab))
          ylab <- paste("Odds of being", ifelse(missing(yaxis),
                                                "a case", yaxis[2]))
        if (missing(xaxis))
          xaxis <- c("non-exposed", "exposed")
        axis(1, at = c(0, 1), labels = xaxis)
      }
      else {

        if (missing(main))
          main <- "Odds ratio from case control study"
        if (missing(ylab))
          ylab <- "Outcome category"
        if (missing(xlab))
          xlab <- ""
        if (missing(yaxis))
          yaxis <- c("Control", "Case")
        axis(2, at = c(0, 1), labels = yaxis, las = 2)
        mtext(paste("Odds of ", ifelse(xlab == "", "being exposed",
                                       paste("exposure being", xaxis[2]))), side = 1,
              line = ifelse(xlab == "", 2.5, 1.8))
      }
      title(main = main, xlab = xlab, ylab = ylab)
    }
  }

  if (!fisher.or) {
    results <- list(or.method = "Asymptotic", or = or, se.ln.or = se.ln.or,
                    alpha = alpha, exact.ci.or = exact.ci.or, ci.or = ci.or,
                    table = table1, decimal = decimal, p.value = p.value)
  } else {
    results <- list(or.method = "Fisher's", or = or, alpha = alpha,
                    exact.ci.or = exact.ci.or, ci.or = ci.or, table = table1,
                    decimal = decimal, p.value = p.value)
  }

  class(results) <- c("cci", "cc")
  return(results)

}

#' Creation of a 2x2 table using the indicated orientation.
#'
#' make2x2 function port epicalc version 2.15.1.0 (Virasakdi Chongsuvivatwong, 2012)
#' @param caseexp Number of cases exposed
#' @param controlex Number of controls exposed
#' @param casenonex Number of cases not exosed
#' @param controlnonex Number of controls not exposed
#' @note This function is for internal BIGDAWG use only.
make2x2 <- function (caseexp, controlex, casenonex, controlnonex)  {

  table1 <- c(controlnonex, casenonex, controlex, caseexp)
  dim(table1) <- c(2, 2)
  rownames(table1) <- c("Non-diseased", "Diseased")
  colnames(table1) <- c("Non-exposed", "Exposed")
  attr(attr(table1, "dimnames"), "names") <- c("Outcome", "Exposure")
  table1

}

#' Table Maker
#'
#' Table construction of per haplotype for odds ratio, confidence intervals, and pvalues
#' @param x Contingency table with binned rare cells.
#' @note This function is for internal BIGDAWG use only.
TableMaker <- function(x) {
  grp1_sum <- sum(x[,'Group.1'])
  grp0_sum <- sum(x[,'Group.0'])
  grp1_exp <- x[,'Group.1']
  grp0_exp <- x[,'Group.0']
  grp1_nexp <- grp1_sum - grp1_exp
  grp0_nexp <- grp0_sum - grp0_exp
  cclist <- cbind(grp1_exp, grp0_exp, grp1_nexp, grp0_nexp)
  tmp <- as.data.frame(t(cclist))
  names(tmp) <- row.names(x)
  return(tmp)
}

#' Case Control Odds Ratio Calculation from Epicalc
#'
#' Calculates odds ratio and pvalues from 2x2 table
#' @param x List of 2x2 matrices for calculation, output of TableMaker.
#' @note This function is for internal BIGDAWG use only.
cci.pval <- function(x) {
  tmp <- list()
  caseEx <- x[1]
  controlEx <- x[2]
  caseNonEx <- x[3]
  controlNonEx <- x[4]
  table1 <- make2x2(caseEx, controlEx, caseNonEx, controlNonEx)
  tmp1 <- cci(cctable=table1, design = "case-control")
  tmp[['OR']] <- round(tmp1$or,digits=2)
  tmp[['CI.L']] <- round(tmp1$ci.or[1],digits=2)
  tmp[['CI.U']] <- round(tmp1$ci.or[2],digits=2)
  tmp[['p.value']] <-  format.pval(tmp1$p.value)
  tmp[['sig']] <- ifelse(tmp1$p.value <= 0.05,"*","NS")
  return(tmp)
}

#' Case Control Odds Ratio Calculation from Epicalc list variation
#'
#' Variation of the cci.pval function
#' @param x List of 2x2 matrices to apply the cci.pval function. List output of TableMaker.
#' @note This function is for internal BIGDAWG use only.
cci.pval.list <- function(x) {
  tmp <- lapply(x, cci.pval)
  tmp <- do.call(rbind,tmp)
  colnames(tmp) <- c("OR","CI.lower","CI.upper","p.value","sig")
  return(tmp)
}

#' Strict Chi-squared Contingency Table Test
#'
#' Calculates chi-squared contingency table tests and bins all rare cells.
#' @param x Contingency table.
#' @note This function is for internal BIGDAWG use only.
RunChiSq <- function(x) {

  ### get expected values for cells
  ExpCnts <- chisq.test(as.matrix(x))$expected

  ## pull out cells that don't need binning, bin remaining
  #unbinned
  OK.rows <- as.numeric(which(apply(ExpCnts,min,MARGIN=1)>=5))

  if(length(OK.rows)==0) {

    # All rows have cells with expected less than 5.
    tmp.chisq <- data.frame(rbind(rep("NCalc",4)))
    colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")
    chisq.out <- list(Matrix = NA,
                      Binned = NA,
                      Test = tmp.chisq,
                      Flag = FALSE)

  } else {

    if(length(OK.rows)>=2) {
      unbinned <- x[OK.rows,]
    } else {
      unbinned <- do.call(cbind,as.list(x[OK.rows,]))
      rownames(unbinned) <- rownames(x)[OK.rows]
    }

    #binned
    Rare.rows <- as.numeric(which(apply(ExpCnts,min,MARGIN=1)<5))
    if(length(Rare.rows)>=2) {
      binned <- x[Rare.rows,]
      New.df <- rbind(unbinned,colSums(x[Rare.rows,]))
      rownames(New.df)[nrow(New.df)] <- "binned"
    } else {
      binned <- cbind(NA,NA)
      colnames(binned) <- c("Group.0","Group.1")
      New.df <- x
    }

    if(nrow(New.df)>1) {

      # flag if final matrix fails Cochran's rule of thumb (more than 20% of exp cells are less than 5)
      # True = OK ; False = Not good for Chi Square
      ExpCnts <- chisq.test(New.df)$expected
      if(sum(ExpCnts<5)==0){
        # all expected are greater than 5
        flag <- TRUE
      } else if( sum(ExpCnts<5)/sum(ExpCnts>=0)<=0.2 && sum(ExpCnts>=1)==length(ExpCnts) ){
        # expected counts < 5 are greater than or equal to 20% of cells
        # all individual counts are >= 1
        flag <- TRUE
      } else {
        # else flag contingency table
        # invalid
        flag <- FALSE
      }

      ## chi square test on binned data
      df.chisq <- chisq.test(New.df)
      Sig <- if(df.chisq$p.value > 0.05) { "NS" } else { "*" }


      ## show results of overall chi-square analysis
      tmp.chisq <- data.frame(cbind(round(df.chisq$statistic,digits=4),
                                    df.chisq$parameter,
                                    format.pval(df.chisq$p.value),
                                    Sig))
      colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")

      chisq.out <- list(Matrix = New.df,
                        Binned = binned,
                        Test = tmp.chisq,
                        Flag = flag)


    } else {

      tmp.chisq <- data.frame(rbind(rep("NCalc",4)))
      colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")
      chisq.out <- list(Matrix = New.df,
                        Binned = binned,
                        Test = tmp.chisq,
                        Flag = FALSE)

    }

  }

  return(chisq.out)

}

#' Contextual Binning Chi-squared Contingency Table Test
#'
#' Calculates chi-squared contingency table tests and bins rare cells at 20% capture rate.
#' @param x Contingency table.
#' @note This function is for internal BIGDAWG use only.
RunChiSq_c <- function(x) {

  ### get expected values for cells
  ExpCnts <- chisq.test(as.matrix(x))$expected

  # Order Counts
  getOrder <- order(ExpCnts[,1],ExpCnts[,2],decreasing=T)
  ExpCnts <- ExpCnts[getOrder,]
  x.sub <- x[getOrder,]

  # Define Rows
  Safe.cells.rows <- as.numeric(which(apply(ExpCnts,min,MARGIN=1)>=5))
  Rare.cells.rows <- as.numeric(which(apply(ExpCnts,min,MARGIN=1)<5))

  # Define Flags
  Check.Rebinned <- FALSE
  No.Bin <- FALSE

  if(length(Safe.cells.rows)==0) {

    # All rows have cells with expected less than 5.
    tmp.chisq <- data.frame(rbind(rep("NCalc",4)))
    colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")
    chisq.out <- list(Matrix = NA,
                      Binned = NA,
                      Test = tmp.chisq,
                      Flag = FALSE)

  } else {

    ### pull out cells that don't need binning, bin remaining
    #unbinned
    if(length(Safe.cells.rows)>=2) {

        unbinned <- x.sub[Safe.cells.rows,]

    } else {

        unbinned <- do.call(cbind,as.list(x.sub[Safe.cells.rows,]))
        rownames(unbinned) <- rownames(x.sub)[Safe.cells.rows]

    }

    unbinned.tmp <- unbinned

    # Iterate through rows -- adding back rows until threshold exceeds 0.2 (20%)
    if( length(Rare.cells.rows)>=3 ) {


      threshold=0 ; i=1
      repeat {

        # Process through adding back rows until threshold exceeds 0.2 (20%)
        get.putRow <- Rare.cells.rows[seq(1,i)]
        get.binRow <- Rare.cells.rows[seq(i+1,length(Rare.cells.rows))]

        if ( length(get.binRow)==1 ) { Stop = i - 1 ; break }

        unbinned.test <- rbind(unbinned.tmp,
                               x.sub[get.putRow,],
                               rbind(colSums(x.sub[get.binRow,]))
        )

        unbinned.test.cs <- chisq.test(unbinned.test)$expected
        threshold <- sum(unbinned.test.cs<5)/ length(unbinned.test.cs)

        if( threshold<=0.2 ) { i = i + 1 } else { Stop = i - 1 ; break }

      }# End repeat

      if( Stop>0 ) {

        # Set up which rows to rescue and bin
        getRescued <- Rare.cells.rows[1:Stop]
        putBinned <- Rare.cells.rows[seq(i,length(Rare.cells.rows))]

        # binning must be more than 1 row
        if( length(putBinned)==1 ) {
          putBinned <- c(getRescued[length(getRescued)],putBinned)
          getResuced <- getRescued[-length(getRescued)]
        }

        # Reclaim any rescued rows to unbinned matrix
        if( length(getRescued) > 1 ) {
          unbinned <- rbind(unbinned, x.sub[getRescued,])
          rownames(unbinned)[getRescued] <- rownames(x.sub)[getRescued]
        } else {
          rebin.tmp <- do.call(cbind,as.list(x.sub[getRescued,]))
          rownames(rebin.tmp) <- rownames(x.sub)[getRescued]
          unbinned <- rbind(unbinned, rebin.tmp)
        }

        # Bin remaining rows
        binned <- x.sub[putBinned,]
        rownames(binned) <- rownames(x.sub)[putBinned]

        Check.Rebinned <- TRUE

      } else {

        # Stop == 0
        # No rows identified to rescue, bin all rare cell containing rows
        binned <- x.sub[Rare.cells.rows,]
        rownames(binned) <- rownames(x.sub)[Rare.cells.rows]

      }

    } else if ( length(Rare.cells.rows)==2 ) {

      # For Rare cells in only 2 rows
      threshold <- sum(ExpCnts<5) / length(x.sub)
      if( threshold > 0.2 ) {

        # must bin both
        binned <- x.sub[Rare.cells.rows,]
        rownames(binned) <- rownames(x.sub)[Rare.cells.rows]

      } else {

        # no binning required
        No.Bin <- TRUE

      }

    } else {

      # Rare.cells.rows == 1
      # No binning possible
      No.Bin <- TRUE


    }

    # Playing no favorites
    # Check if rescued cell expected counts overlap binned expected counts
    if(Check.Rebinned) {

      # If check.rebinned = T
      # getRescued = rescued rows ... can be 1 row
      # putBinned = binned rows ... must be greater than 1 row


      # Rescued rows expected counts
      if(length(getRescued)>1) {
        rescue.expcnts <- apply(ExpCnts[getRescued,],MARGIN=1,paste,collapse=":")
      } else {
        rescue.expcnts <- paste(ExpCnts[getRescued,],collapse=":")
      }

      # Binned rows expected counts
      bin.expcnts <- apply(ExpCnts[putBinned,],MARGIN=1,paste,collapse=":")
      bin.expcnts.rev <- apply(ExpCnts[putBinned,c(2,1)],MARGIN=1,paste,collapse=":")

      rebin.hits <- unique(c(which((rescue.expcnts %in% bin.expcnts)==T),
                             which((rescue.expcnts %in% bin.expcnts.rev)==T)))

      if ( length(rebin.hits)>0 ) {

        rebin.names <- names(rescue.expcnts[rebin.hits])
        rebin.rows <- which((row.names(unbinned) %in% rebin.names)==T)
        binned <- rbind(binned,unbinned[rebin.rows,,drop=F])
        unbinned <- unbinned[-rebin.rows,]

      }

    }

    # Create final matrix New.df
    if ( No.Bin || nrow(unbinned)==0 ) {

      binned <- cbind(NA,NA)
      colnames(binned) <- c("Group.0","Group.1")
      New.df <- x.sub

    } else {

      # merge unbinned and column sums of binned
      New.df <- rbind(unbinned,colSums(binned))
      rownames(New.df)[nrow(New.df)] <- "binned"

      # Reorder binned by row names
      binned <- binned[order(rownames(binned)),]

    }

    # Reorder New.df by row names
    putOrder <- order(row.names(New.df))
    New.df <- New.df[putOrder,]


    if(nrow(New.df)>1) {

      ExpCnts <- chisq.test(New.df)$expected
      # flag if final matrix fails Cochran's rule of thumb (more than 20% of exp cells are less than 5)
      # True = OK ; False = Not good for Chi Square
      if(sum(ExpCnts<5)==0){
        # all expected are greater than 5
        flag <- TRUE
      } else if( sum(ExpCnts<5)/sum(ExpCnts>=0)<=0.2 && sum(ExpCnts>=1)==length(ExpCnts) ){
        # expected counts < 5 are greater than or equal to 20%
        # all individual counts are >= 1
        flag <- TRUE
      } else {
        # else flag contingency table
        flag <- FALSE
      }

      ## chi square test on binned data
      df.chisq <- chisq.test(New.df)
      Sig <- if(df.chisq$p.value > 0.05) { "NS" } else { "*" }


      ## show results of overall chi-square analysis
      tmp.chisq <- data.frame(cbind(round(df.chisq$statistic,digits=4),
                                    df.chisq$parameter,
                                    format.pval(df.chisq$p.value),
                                    Sig))
      colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")

      chisq.out <- list(Matrix = New.df,
                        Binned = binned,
                        Test = tmp.chisq,
                        Flag = flag)


    } else {

      flag <- FALSE
      tmp.chisq <- data.frame(rbind(rep("NCalc",4)))
      colnames(tmp.chisq) <- c("X.square", "df", "p.value", "sig")
      chisq.out <- list(Matrix = New.df,
                        Binned = binned,
                        Test = tmp.chisq,
                        Flag = FALSE)
    }


  }

return(chisq.out)

}
