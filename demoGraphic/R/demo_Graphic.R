



#' Mean, var function
#'
#' @param x variable
#'
#' @return mean table
#' @export
#'
#' @examples
#' get_mean(round(abs(rnorm(500)*10),0))
get_mean <- function(x) {
  n <- length(x)
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  var <- var(x, na.rm = TRUE)
  n.miss <- sum(is.na(x))
  result <- data.frame(n, mean, sd, var, n.miss)
  return(result)
}



#' t.test to calculate p value
#'
#' @importFrom stats t.test
#'
#' @param ... variables
#'
#' @return p value
#' @export
#'
#' @example
#'t.test.p.value(round(abs(rnorm(500)*10),0))
t.test.p.value <- function(...) {
  is <- NULL
  obj <- try(t.test(...), silent = TRUE)
  if (is(obj, "try-error"))
    return(NaN) else return(data.frame(obj$p.value))
}


#' smd value for continuous variable.
#'
#' @param mean1 mean of a baseline variable in the treatment group.
#' @param mean2 mean of a baseline variable in the control group.
#' @param var1 variance a baseline variable in the treatment group.
#' @param var2 variance of a baseline variable in the control group.
#'
#' @return smd value
#' @export
#'
#' @examples
#' cont_smd(10,11,2,3)
cont_smd <- function(mean1, mean2, var1, var2) {
  smd <- (mean1 - mean2)/sqrt((var1 + var2)/2)
  return(smd)
}




#' Confident interval for smd
#'
#' @param n1 length of a baseline variable in the treatment group.
#' @param n2 length of a baseline variable in the control group.
#' @param smd smd value
#'
#' @return vector of 95% ci
#' @export
#'
#' @examples
#' smd_ci(10,12,0.3)
smd_ci <- function(n1, n2, smd) {
  se <- sqrt((n1 + n2)/(n1 * n2) + smd^2/(2 * (n1 + n2)))
  smd.l <- smd - 1.96 * se
  smd.u <- smd + 1.96 * se
  smd.ci <- c(smd.l, smd.u)
  return(smd.ci)
}


#' DemoGraphic table for continuous variables
#'
#' @param var variables
#' @param strata group variable with 1 = treatment and 0 = control
#' @param data data
#'
#' @return mean, standard deviation of treatmant and control group, smd, and p value.
#' @export
#'
#' @examples
#' set.seed(2018)
#' group <-round(abs(rnorm(500)*10),0) %% 2
#' cont_1 <-round(abs(rnorm(500)*10),0)
#' cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
#' data_check <-data.frame(group, cont_1, cat_multi_1)
#' data_check$group <- factor(data_check$group, levels = c(0,1), labels = c("Control","Treatment"))
#' data_check$cat_multi_1 <- factor(data_check$cat_multi_1)
#' cont_table("cont_1","group", data_check )
cont_table <- function(var, strata, data) {

  dt <- data[, c(var, strata)]
  dt[,strata] <- as.factor(dt[,strata])

  result <- lapply(var, function(v) {

    dat <- split(dt[, v], dt[, strata])

    # get mean, sd, var
    s <- lapply(dat, function(i) get_mean(i))

    # p value using t test
    p.value <- t.test.p.value(dat[[1]], dat[[2]])

    # smd
    smd.value <- cont_smd(s[[2]]$mean,s[[1]]$mean,s[[2]]$var,s[[1]]$var)
    names(smd.value) <- "smd.value"

    # smd ci
    smd.ci <- smd_ci(s[[2]]$n,s[[1]]$n,smd.value)

    result <- data.frame(paste0(v, " Mean (sd)"), paste0(sprintf("%.2f", s[[1]]$mean), " (",sprintf("%.2f", s[[1]]$sd), ")"),
                         paste0(sprintf("%.2f", s[[2]]$mean), " (",sprintf("%.2f", s[[2]]$sd), ")"),
                         sprintf("%.3f", p.value), sprintf("%.3f",smd.value),v,smd.value,smd.ci[[1]],smd.ci[[2]])

    smd_table <- result
  })

  result <- data.frame(do.call("rbind", result))
  colnames(result) <- c("Variable", levels(dt[,strata]), "p value", "smd","Variable","smd.value","smd.lo","smd.up")
  row.names(result) <- NULL


  demo_table <- result[,1:5]
  smd_table <- result[,c(6:9)]

  cont_tb <- list(demo_table,smd_table)
  names(cont_tb) <- c("demo_table","smd_table")

  class(cont_tb) <- "cont_tb"

  attributes(cont_tb) <- c(attributes(cont_tb),
                           list(demo = demo_table),
                           list(smd   = smd_table))

  return(cont_tb)
}



#' write smd table or demographic table into docx file
#'
#' @param smd_table smd table or demo graphic table.
#' @param name file name to save
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'mydocx(data.frame(smd.value <- 3.4, smd.lo <- 1.1, smd.up <- 5.6),"smd_table")
mydocx <- function(smd_table,name){
  '%>%' <- magrittr::"%>%"
  my_doc <- officer::read_docx() #styles_info(my_doc)
  my_doc <- my_doc %>% officer::body_add_table(smd_table, style = "table_template")
  file_name <- paste0(name, ".docx", sep="")
  print(my_doc, target = file_name)

}

#' smd value for categorical variables
#'
#' @param ntable propotion table of baseline categorical variable and group variable
#'
#' @param var baseline categorical variable
#' @param data data
#'
#' @export
#'
#' @examples
#' set.seed(2018)
#' group <-round(abs(rnorm(500)*10),0) %% 2
#' cont_1 <-round(abs(rnorm(500)*10),0)
#' cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
#' data_check <-data.frame(group, cont_1, cat_multi_1)
#' data_check$group <- factor(data_check$group, levels = c(0,1), labels = c("Control","Treatment"))
#' data_check$cat_multi_1 <- factor(data_check$cat_multi_1)
#' cat_smd(table(data_check$cat_multi_1, data_check$group),"cat_multi_1",data_check )
cat_smd <- function(ntable, var, data) {
  # Binary variable
   var <- data[, var]
   ntable <- ntable

  if (length(levels(var)) == 2) {
    mean_group <- prop.table(ntable, 2)[-1, , drop = FALSE]
    Treatment <- mean_group[, 2]
    Control <- mean_group[, 1]
    variance_group <- mean_group * (1 - mean_group)
    Treatment_variance <- variance_group[, 2]
    Control_variance <- variance_group[, 1]
    if (Treatment - Control != 0) {
      smd <- (Treatment - Control)/sqrt((Treatment_variance + Control_variance)/2)
    } else {
      smd <- NaN
    }
  } else {
    # Multinomial variables
    prop_Table <- prop.table(ntable, 2)[-1, , drop = FALSE]
    Treatment <- prop_Table[, 2]
    Control <- prop_Table[, 1]
    T_C <- Treatment - Control
    # Get Covariance of Treatment Group and Control Group
    # https://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
    # https://en.wikipedia.org/wiki/Multinomial_distribution
    # The covariance matrix is as follows. Each diagonal entry is the variance of a binomially distributed random variable, and is therefore

    # {\displaystyle \operatorname {var} (X_{i})=np_{i}(1-p_{i}).\,} \operatorname{var}(X_i)=np_i(1-p_i).\,
    # The off-diagonal entries are the covariances:
    #
    # {\displaystyle \operatorname {cov} (X_{i},X_{j})=-np_{i}p_{j}\,} \operatorname{cov}(X_i,X_j)=-np_i p_j\,
    # for i, j distinct.
    # https://people.cs.kuleuven.be/~raf.vandebril/homepage/publications/phd/node38.html

    covs <- lapply(list(Treatment, Control), function(j) {
      p <- j
      variance <- p * (1 - p)
      covs <- -outer(p, p)
      diag(covs) <- variance
      return(covs)
    })
    S <- (covs[[1]] + covs[[2]])/2
    if (all(S[!is.na(S)] %in% 0)) {
      smd <- NaN
    } else {
      smd <- sqrt(t(T_C) %*% MASS::ginv(S) %*% T_C)
    }
  }
  return(smd)
}

#' chi square test to get expected value and p value
#'
#' @importFrom stats chisq.test
#'
#' @param ... variables
#'
#' @export
#'
#' @examples
#' set.seed(2018)
#' group <-round(abs(rnorm(500)*10),0) %% 2
#' cont_1 <-round(abs(rnorm(500)*10),0)
#' cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
#' data_check <-data.frame(group, cont_1, cat_multi_1)
#' data_check$group <- factor(data_check$group, levels = c(0,1), labels = c("Control","Treatment"))
#' data_check$cat_multi_1 <- factor(data_check$cat_multi_1)
#' my.chi.sq(table(data_check$cat_multi_1, data_check$group))
my.chi.sq <- function(...) {
  is <- NULL
  op <- options(warn = (-1))  # suppress warnings
  obj <- try(chisq.test(...), silent = TRUE)
  if (is(obj, "try-error"))
    return(NaN) else return(list(obj$expected, obj$p.value))
}

#' fisher exact test to get p value if any cell in propotion table of expect value less than 5
#'
#' @importFrom stats fisher.test
#'
#' @param ... variables
#'
#' @export
#'
#' @examples
#' set.seed(2018)
#' data_check <-data.frame(
#'   group <-round(abs(rnorm(500)*10),0) %% 2,
#'   cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3)
#' my.fisher(table(data_check$cat_multi_1, data_check$group))
my.fisher <- function(...) {
  is <- NULL
  obj <- try(fisher.test(...), silent = TRUE)
  if (is(obj, "try-error"))
    return(NaN) else return(obj$p.value)
}


#' DemoGraphic table for categorical variables
#'
#' @param var baseline variables
#'
#' @param strata group variable with 1 = treatment and 0 = control
#' @param data data
#'
#' @export
#'
#' @examples
#' set.seed(2018)
#' group <-round(abs(rnorm(500)*10),0) %% 2
#' cont_1 <-round(abs(rnorm(500)*10),0)
#' cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
#' data_check <-data.frame(group, cont_1, cat_multi_1)
#' data_check$group <- factor(data_check$group, levels = c(0,1), labels = c("Control","Treatment"))
#' data_check$cat_multi_1 <- factor(data_check$cat_multi_1)
#' cat_table("cat_multi_1","group",data_check )
cat_table <- function(var, strata, data) {

  dt <- data[, c(var, strata)]
  dt[,strata] <- as.factor(dt[,strata])

  # if (length(var) == 1 & length(levels(dt[, var])) < 2) {
  #
  #   print(paste0(var, " has level less than two"))
  #
  #   } else {

  result <- lapply(var, function(v) {

    if (class(dt[, v]) %in% c("integer", "numeric") & length(levels(factor(dt[,v]))) == 2) {
      dt[, v] <- as.factor(dt[, v])
    }

    if (class(dt[, v]) %in% c("factor", "character") & length(levels(dt[,v])) >= 2){

      # get propotion table
      ntable <- table(dt[, v], dt[, strata])
      propTable <- 100 * prop.table(ntable, 2)
      names <- attr(propTable, "dimnames")[[2]]

      # using chisquare or fisher test
      p.vale <- ifelse(any(my.chi.sq(ntable)[[1]]) < 5, my.fisher(ntable, workspace = 2e+07),
                       my.chi.sq(ntable)[[2]])

      # get smd
      smd.value <- cat_smd(ntable, v, dt)

      # smd ci
      smd.ci <- smd_ci(colSums(ntable)[1], colSums(ntable)[2], smd.value)
      smd <- data.frame(smd.value,smd.ci[[1]],smd.ci[[2]])
      smd$var <- v

      result <- data.frame(rbind(c(rep("", 4)), as.matrix(cbind(paste0(ntable[,1], " (", sprintf("%.2f", propTable[, 1]), ")"),
                                                                paste0(ntable[, 2]," (", sprintf("%.2f", propTable[, 2]), ")"),
                                                                c(sprintf("%.3f", p.vale),rep("", nrow(ntable) - 1)),
                                                                c(sprintf("%.3f", smd.value), rep("", nrow(ntable) - 1))))))

      result$var <- c(paste0(v, " n (%)"), paste0("   ", levels(dt[, v])))
      smd_table <- list(result, smd)

    }else{

      smd <- NULL
      result <- NULL

      smd_table <- list(result, smd)
    }

  })

  smd_table <- data.frame(do.call("rbind", lapply(result, function(i) rbind(i[[2]][!is.null(i[[2]])]))))

  if(nrow(smd_table) >= 1){

    smd_table <- smd_table[,c(4,1:3)]
    names(smd_table) <- c("Variable","smd.value","smd.lo","smd.up")
    row.names(smd_table) <- NULL


    result <- data.frame(do.call("rbind", lapply(result, function(i) rbind(i[[1]][!is.null(i[[1]])]))))
    result <- result[, c(5, 1:4)]
    colnames(result) <- c("Variable", levels(dt[,strata]), "p value", "smd")
    row.names(result) <- NULL
    demo_table <- result


    cat_tb <- list(demo_table,smd_table)
    names(cat_tb) <- c("demo_table","smd_table")

    class(cat_tb) <- c("cont_tb")

    attributes(cat_tb) <- c(attributes(cat_tb),
                            list(demo = demo_table),
                            list(smd   = smd_table))

    return(cat_tb)

  }else{

    print(paste0(var, " has level less than two"))
  }

}


######################################

#' Demographic Table for continuous and categorical variables
#'
#' @param var list of baseline variables
#'
#' @param strata group variable with 1 = treatment and 0 = control
#' @param data data
#'
#' @export
#'
#' @examples
#' set.seed(2018)
#' group <-round(abs(rnorm(500)*10),0) %% 2
#' cont_1 <-round(abs(rnorm(500)*10),0)
#' cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
#' data_check <-data.frame(group, cont_1, cat_multi_1)
#' data_check$group <- factor(data_check$group, levels = c(0,1), labels = c("Control","Treatment"))
#' data_check$cat_multi_1 <- factor(data_check$cat_multi_1)
#' demo_table(c("cont_1","cat_multi_1"),"group", data_check )
demo_table <- function(var, strata, data) {

  result <- lapply(var, function(v) {

    dt <- data[, c(v, strata)]

    cl <- class(dt[, v])

    # sometimes user code binary as numeric or integer, make sure convert it to factor
    if (cl %in% c("integer", "numeric") & length(levels(factor(dt[, v]))) == 2) {
      dt[, v] <- as.factor(dt[, v])
    }
    # Continuous Variables
    if (cl %in% c("numeric", "integer") & length(levels(factor(dt[, v]))) > 2) {
      result <- cont_table(v, strata, dt)
      # Categorical Variables
    } else if (cl %in% c("factor", "character") & length(levels((dt[, v]))) >= 2) {
      result <- cat_table(v, strata, dt)
      # Categorical variable has only one level, it will be NULL
    } else {
      result <- NULL
    }
    result
  })

  smd_table <- data.frame(do.call("rbind", lapply(result, function(i) rbind(i$smd_table[!is.null(i$smd_table)]))))

  if(nrow(smd_table)< 1){
    smd_table <- NULL
    demo_table <- NULL
    print_out <- print(paste0(var, " has level less than two"))
    cont_tb <- list(print_out,demo_table,smd_table)

  }else{
    # Remove row with NULL (that means removing Categorical variable has only one level)
    demo_table <- do.call("rbind", lapply(result, function(i) rbind(i$demo_table[!is.null(i$demo_table)])))

    cont_tb <- list(demo_table,smd_table)
    names(cont_tb) <- c("demo_table","smd_table")

    class(cont_tb) <- c("cont_tb")

    attributes(cont_tb) <- c(attributes(cont_tb),
                             list(demo = demo_table),
                             list(smd   = smd_table))
  }
  return(cont_tb)
}
