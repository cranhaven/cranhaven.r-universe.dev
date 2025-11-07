#' Display a table used in paper
#'
#' Display count, frequency or mean, standard deviation and test of normality, etc.

#' @param data A data.frame
#' @param variables  Column indices or names of the variables in the dataset to display, the default columns are all the variables except group variable
#' @param group Column indices or names of the first subgroup variables. Must provide.
#' @param super_group Column indices or names of the further subgroup variables.
#' @param group_combine A logical, subgroup analysis for combination of variables or for each variable. The default is FALSE (subgroup analysis for each variable)
#' @param mean_or_median A character to specify mean or median to used for continuous variables, either "mean" or "median". The default is "mean"
#' @param addNA Whether to include NA values in the table, see \code{\link{table}} for more details
#' @param table_margin Index of generate margin for, see \code{\link{prop.table}} for more details
#' @param discrete_limit Defining the minimal of unique value to display the variable as count and frequency, the default is 10
#' @param exclude_discrete Logical, whether to exclude discrete variables with more unique values specified by discrete_limit
#' @param save_to_file  A character, containing file name or path
#' @param normtest  A character indicating test of normality, the default method is \code{\link{shapiro.test}} when sample size no more than 5000, otherwise \code{\link[nortest]{lillie.test}} {Kolmogorov-Smirnov} is used, see package \strong{nortest} for more methods.Use 'shapiro.test', 'lillie.test', 'ad.test', etc to specify methods.
#' @param fill_variable A logical, whether to fill the variable column in result, the default is FALSE
#' @import nortest dplyr rlang
#' @importFrom stats binomial confint glm lm
#' @importFrom stats anova  as.formula  chisq.test  complete.cases confint.default fisher.test  kruskal.test median na.omit quantile sd xtabs
#' @importFrom utils write.table
#' @export
#' @note The return table is a data.frame.
#' @note - P.value1 is ANOVA P value for continuous variables and chi-square test P value for discrete variables
#' @note - P.value2 is Kruskal-Wallis test P value for continuous variables and fisher test P value for discrete variables if expected counts less than 5
#' @note - normality is normality test P value for each group
#' @examples
#' \dontrun{
#' data(diabetes)
#' head(diabetes)
#' library(dplyr);library(rlang)
#' result_1<-diabetes %>%
#'  group_by(sex) %>%
#'  do(display_table(data=.,variables=c("age","smoking"),group="CFHrs2230199")) %>%
#'  ungroup()
#' result_2<-display_table_group(data=diabetes,variables=c("age","smoking"),
#' group="CFHrs2230199",super_group = "sex")
#' identical(result_1,result_2)
#' result_3<-display_table_group(data=diabetes,variables=c("age","education"),
#' group=c("smoking"),super_group = c("CFHrs2230199","sex"))
#' result_4<-display_table_group(data=diabetes,variables=c("age","education"),
#' group=c("smoking"),super_group = c("CFHrs2230199","sex"),group_combine=TRUE)
#' }



display_table <- function(data = NULL, variables  = NULL,group=NULL, mean_or_median="mean",addNA = TRUE,table_margin=2, discrete_limit = 10, exclude_discrete=TRUE, save_to_file=NULL,normtest = NULL,fill_variable=FALSE) {


    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )

    if (is.null(variables ))
      variables  = seq_along(1:NCOL(data))
    if (!is.character(variables )) variables <-names(data)[variables ]
    if(length(group)!=1) stop("`group` is not one length.", call. = FALSE)
    if (!is.character(group)) group<-names(data)[group]

    data<-filter(data,!is.na(!!sym(group)))
    n_group<-NROW(unique(data[,group,drop=FALSE]))

    data[, group] <- as.factor(data[, group])


    ## data[,group]<-as.factor(data[,group])

    if (any(group %in% variables )) {
      warning(paste0("group varibale indice: `", group, "` is also in `variables `, will be removed from them, please check.\n"),
              call. = FALSE)
      variables  = setdiff(variables , group)
    }

    result <- vector(mode = "list", length = length(variables ))


    ## functions used in display


    format_sprint<-function(x) {
      x<-ifelse(abs(x)<0.005,sprintf(fmt="%.2E",x),sprintf(fmt="%.2f",x))
      x
    }

    ## using appropriate method, table or mean

    is_discrete<-function(var_value,discrete_limit_=discrete_limit,exclude_discrete_=exclude_discrete) {
      if((length(unique(var_value)) >= discrete_limit_ & is.numeric(var_value))) {
        return(FALSE)
      } else if (length(unique(var_value)) >= discrete_limit_ && exclude_discrete_) {
        return(NA)
      } else return(TRUE)
    }


    ### for continuous variables
    continuous_stat<-function (data=data,x=x,group=group,mean_or_median_=mean_or_median) {

      result_1<-data %>%
        group_by(!!sym(group)) %>%
        summarise(mean=mean(!!sym(x),na.rm=TRUE),sd=sd(!!sym(x),na.rm=TRUE),
                  median=median(!!sym(x),na.rm = TRUE),Q1=quantile(!!sym(x),probs=c(0.25),na.rm = TRUE),
                  Q3=quantile(!!sym(x),probs=c(0.75),na.rm = TRUE),NA_=sum(is.na(!!sym(x)))) %>%
        ungroup()
      result_1[,2:6]<-lapply(result_1[,2:6],format_sprint)
      result_1$mean_sd<-paste0(result_1$mean," +- ",result_1$sd)
      result_1$median_IQR<-paste0(result_1$median," (",result_1$Q1,", ",result_1$Q3,")")
      result_1<- as.data.frame(t(result_1),stringsAsFactors = FALSE)
      mean_sd_1<-result_1[c("mean_sd","NA_"),]
      median_IQR_1<-result_1[c("median_IQR","NA_"),]


      result_2<-data %>%
        summarise(mean=mean(!!sym(x),na.rm=TRUE),sd=sd(!!sym(x),na.rm=TRUE),
                  median=median(!!sym(x),na.rm = TRUE),Q1=quantile(!!sym(x),probs=c(0.25),na.rm = TRUE),
                  Q3=quantile(!!sym(x),probs=c(0.75),na.rm = TRUE),NA_=sum(is.na(!!sym(x)))) %>%
        ungroup()

      result_2[,1:5]<-lapply(result_2[,1:5],format_sprint)
      result_2$mean_sd<-paste0(result_2$mean," +- ",result_2$sd)
      result_2$median_IQR<-paste0(result_2$median," (",result_2$Q1,", ",result_2$Q3,")")
      result_2<- as.data.frame(t(result_2),stringsAsFactors = FALSE)
      mean_sd_2<-result_2[c("mean_sd","NA_"),]
      median_IQR_2<-result_2[c("median_IQR","NA_"),]

      mean_sd<-cbind(c("mean +- sd","NA"),mean_sd_2,mean_sd_1,stringsAsFactors=FALSE)
      names(mean_sd)<-c("level",paste0("V",1:( n_group+1)))
      median_IQR<-cbind(c("median (Q1,Q3)","NA"),median_IQR_2,median_IQR_1,stringsAsFactors=FALSE)
      names(median_IQR)<-c("level",paste0("V",1:( n_group+1)))

      if(mean_or_median_=="mean") {
        return(mean_sd)
      } else if (mean_or_median_=="median") {
        return(median_IQR)
      } else  stop("Only Mean or Meidian for continuous variables. ", call. = FALSE)

    }



    continuous_normality<-function(data=data,x=x,group=group,normtest_=normtest) {
      if (is.null(normtest_)) {
        normtest_ <- ifelse(length(na.omit(data[,x])) <= 10000, "shapiro.test", "lillie.test")
      }
      p.normality<-data %>%
        group_by(!!sym(group)) %>%
        #select_at(vars(x)) %>%
        do(data.frame(p.normality=do.call(normtest_, list(.$UQ(x)))$p.value)) %>%
        ungroup()
      is.normality<-all(p.normality$p.normality>0.05)
      p.normality<-paste(format_sprint(p.normality$p.normality),collapse ="; ")
      return(list(p.normality=p.normality,is.normality=is.normality))
    }

    continuous_test<-function(data=data,x=x,group=group) {
      var_formula<-as.formula(paste0(x,"~",group,sep="",collapse = "+"))
      fit<-anova(lm(var_formula,data = data))
      p.anova<-format_sprint(fit[1,5])

      fit<-kruskal.test(var_formula,data = data)
      p.rank<-format_sprint(fit$p.value)
      return(list(p.anova=p.anova,p.rank=p.rank))
    }

    continuous_table<-function(data=data,x=x,group=group,mean_or_median_ = mean_or_median,normtest_=normtest) {
      result<-continuous_stat(data=data,x=x,group=group,mean_or_median_)
      row_fill<-rep("",NROW(result)-1)
      if(fill_variable) {
        result<-cbind(variable=x,result,P.value1=c(continuous_test(data=data,x=x,group=group)$p.anova,row_fill),P.value2=c(continuous_test(data=data,x=x,group=group)$p.rank,row_fill),normality=c(continuous_normality(data=data,x=x,group=group,normtest_)$p.normality,row_fill),stringsAsFactors=FALSE)
      } else {
        result<-cbind(variable=c(x,row_fill),result,P.value1=c(continuous_test(data=data,x=x,group=group)$p.anova,row_fill),P.value2=c(continuous_test(data=data,x=x,group=group)$p.rank,row_fill),normality=c(continuous_normality(data=data,x=x,group=group,normtest_)$p.normality,row_fill),stringsAsFactors=FALSE)
    }

      return(result)
    }


    ### for discrete variables
    discrete_test<-function (data=data,x=x,group=group,table_margin_=table_margin) {
      #var_tab<-table(data[,x],data[,group])
      var_tab<-xtabs(as.formula(paste0("~",x,"+",group)),data=data)
      fit<-suppressWarnings(chisq.test(var_tab))
      p.chisq<-format_sprint(fit$p.value)
      if(any(fit$expected<5)) {
        p.fisher<-fisher.test(var_tab)$p.value
        p.fisher<-format_sprint(p.fisher)
      } else p.fisher<-NA
      #print(var_tab)
      var_prop=round(100*prop.table(var_tab,margin=table_margin_),2)
      var_tab=cbind(table(data[,x]),var_tab)
      var_NCOl<-NCOL(var_tab)
      level=row.names(var_tab)
      var_prop=cbind(round(100*prop.table(table(data[,x])),2),var_prop)
      var_tab=as.data.frame(matrix(paste(var_tab," (",var_prop,"%)",sep=""),ncol =var_NCOl),stringsAsFactors = FALSE)


      if(addNA) {
        data[,x]<-ifelse(is.na(unlist(data[,x])),"NA",unlist(data[,x]))
        var_tab_NA<-xtabs(as.formula(paste0("~",x,"+",group)),data=data)
        var_tab_NA<-cbind(table(data[,x]),var_tab_NA)
        var_tab_NA<-as.data.frame(var_tab_NA,stringsAsFactors = FALSE)
        names(var_tab_NA)<-names(var_tab)
        var_tab_NA$row_name<-row.names(var_tab_NA)
        var_tab_NA<-var_tab_NA[var_tab_NA$row_name=="NA",1:var_NCOl]
        if(NROW(var_tab_NA)==0) var_tab_NA<-rep(0,var_NCOl)
        var_tab<-rbind(var_tab,var_tab_NA)
        var_tab<-cbind(level=c(level,"NA"),var_tab,stringsAsFactors=FALSE)
      } else var_tab<-cbind(level=level,var_tab,stringsAsFactors=FALSE)
      names(var_tab)[-1]<-c(paste0("V",1:(n_group+1)))

      row_fill<-rep("",NROW(var_tab)-1)
      if(fill_variable) {
        result<-cbind(variable=x,var_tab,P.value1=c(p.chisq,row_fill),P.value2=c(p.fisher,row_fill),normality="",stringsAsFactors=FALSE)
      } else {
        result<-cbind(variable=c(x,row_fill),var_tab,P.value1=c(p.chisq,row_fill),P.value2=c(p.fisher,row_fill),normality="",stringsAsFactors=FALSE)
      }

      return(result)

    }



   for (i in seq_along(variables )) {
      var_i<- variables[i]
      var_value<-unlist(data[,var_i])

      is_discrete_value<-is_discrete(var_value=var_value,discrete_limit,exclude_discrete)
      if(is.na(is_discrete_value)) {
        warning(paste0("`",var_i,"`"," is verbose. To display it using `exclude_discrete = FALSE` "),call. = FALSE)
      } else if(is_discrete_value) {
        tryCatch({
          result_one<-discrete_test(data=data,x=var_i,group=group,table_margin_=table_margin)
        },error = function(err) {
          warning(paste0("Variable: `", var_i, "` cannot display, please check.\n"),
                  call. = FALSE)
          result_one<<-c(var_i, rep(NA,n_group+5))

        },finally = {result[[i]]<-result_one})
      } else {
        tryCatch({
          result_one<-continuous_table(data=data,x=var_i,group=group,mean_or_median_ = mean_or_median,normtest_=normtest)
        },error = function(err) {
          warning(paste0("Variable: `", var_i, "` cannot display, please check.\n"),
                  call. = FALSE)
          result_one<<-c(var_i, rep(NA,n_group+5))

        },finally = {result[[i]]<-result_one})
      }

   }
    result<-as.data.frame(do.call(rbind,result), stringsAsFactors = FALSE)
    row.names(result)<-NULL
    result_name<-as.list(table(data[,group]))
    #names(result)[3]<-paste0("All sample","\n (N=",NROW(data),")")
    names(result)[3]<-"All sample"
    #names(result)[4:(3+length(result_name))]<-paste0(group," = ",names(result_name),"\n (N=",result_name,")")
    names(result)[4:(3+length(result_name))]<-paste0(group," = ",names(result_name))
    if (!addNA) {
      result<-result[result$level!="NA",]
    }

    if(!is.null(save_to_file)) {
      write.table(result, file = save_to_file, sep = ",", row.names = FALSE)
    }
    return(result)

}




#' @describeIn display_table Allow more subgroup analysis, see the package vignette for more details
#' @export
display_table_group <- function(data = NULL, variables  = NULL,group=NULL,super_group=NULL,group_combine=FALSE, mean_or_median="mean",addNA = TRUE,table_margin=2, discrete_limit = 10, exclude_discrete=TRUE, normtest = NULL,fill_variable=FALSE) {

    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )

  if (is.null(variables ))
    variables  = seq_along(1:NCOL(data))
  if (!is.character(variables )) variables <-names(data)[variables ]
  if(length(group)!=1) stop("`group` is not one length.", call. = FALSE)
  if (!is.character(group)) group<-names(data)[group]

  if (!is.character(super_group)) super_group<-names(data)[super_group]

  data<-filter(data,!is.na(!!sym(group)))

  if (any(c(group,super_group) %in% variables )) {
    warning(paste0("group varibale indice: `", group, "` is also in `variables `, will be removed from them, please check.\n"),
            call. = FALSE)
    variables  = setdiff(variables , group,super_group)
  }



  if ((!isTRUE(group_combine)) & length(super_group)>1 ) {
    result_dataframe<-vector(mode = "list", length = length(super_group))
    for (i in seq_along(super_group)) {
      super_group_i <- super_group[i]
      fit<-data %>%
        group_by_at(vars(super_group_i))  %>%
        do(display_table(data = ., variables  = variables,group=group, mean_or_median=mean_or_median,addNA = addNA,table_margin=table_margin, discrete_limit = discrete_limit, exclude_discrete=exclude_discrete, normtest = normtest,fill_variable=fill_variable)) %>%
        ungroup() %>%
        rename(super_group_level=!!super_group_i)
      result_dataframe[[i]]<-as.data.frame(base::cbind(super_group=super_group_i, fit),stringsAsFactors = FALSE)
    }
    result<-as.data.frame(do.call(rbind,result_dataframe), stringsAsFactors = FALSE)

  } else {
    result<-data %>%
      group_by_at(vars(super_group))  %>%
      do(display_table(data = ., variables  = variables,group=group, mean_or_median=mean_or_median,addNA = addNA,table_margin=table_margin, discrete_limit = discrete_limit, exclude_discrete=exclude_discrete, normtest = normtest,fill_variable=fill_variable)) %>%
      ungroup()
  }

  return(invisible(result))

}








utils::globalVariables(c("normtest", ".", "mean_or_median", "n_group", "table_margin", "discrete_limit",'exclude_discrete'))



