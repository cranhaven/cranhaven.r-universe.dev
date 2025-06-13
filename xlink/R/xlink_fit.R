#' Genetic association models for X-chromosome SNPs on continuous, binary and survival outcomes
#'
#' \code{xlink_fit} returns model fitting results for each SNP with the covariates.
#'
#' @param resp Response variable for continuous or binary model fitting.
#' @param os Survival indicator, 1 for death, 0 for censoring.
#' @param ostime Duration time of survival.
#' @param snps SNP name list for model fitting.
#' @param gender Gender information must be included in the data. Default setting is male=1 and female=0. If not as default setting, please provide male and female information in the option.
#' @param covars Covariates list if needed.
#' @param option There are three options. First, type has default 'all', which provides model fitting results for each SNP understanding as 'XCI', 'XCI-E' and 'XCI-S' type respectively. If type is chosen as 'XCI' or 'XCI-E', all the SNPS consider as 'XCI' or 'XCI-E' type in corresponding model. Secondly, if gender is not as default gender setting (male=1,female=0), male and female information should be provided here.The third one, MAF_v is the low bound of the minimum allele frequency, the SNP MAF below this value will not be used in xlink_fit.
#' @param model Fitting model. For 'linear', fitting linear model. For 'binary', fitting logistic regression model. For 'survival', fitting survival model.
#' @param data Data set.
#' @return It returns estimated parameters, confidence interval and P value for each variable in the chosen model. The baseline and full model maximum likelihood estimation are provided. If type is 'all', best model choice is provided by using AIC as an benchmark.
#' @examples
#' Covars<-c("Age","Smoking","Treatment")
#' SNPs<-c("snp_1","snp_2","snp_3")
#' xlink_fit(os="OS",ostime="OS_time",snps=SNPs,gender="gender",covars=Covars,
#'           option =list(MAF_v=0.05),model="survival",data = Rdata)
#' xlink_fit(resp="OS_time",snps=SNPs,gender="gender",option =list(type="XCI",MAF_v=0.05),
#'           model="linear",data = Rdata)
#' @seealso \code{\link{lm}{stats}} for linear model, \code{\link{glm}{stats}} for logistic regression model, and \code{\link{coxph}{survival}} for survival model.
#' @references Xu, Wei, and Meiling Hao. 'A unified partial likelihood approach for X-chromosome association on time-to-event outcomes.' Genetic epidemiology 42.1 (2018): 80-94.
#' @references Han, D., Hao, M., Qu, L., & Xu, W. (2019). A novel model for the X-chromosome inactivation association on survival data. Statistical Methods in Medical Research.
#' @export
#' @import  survival
#' @import  stats
xlink_fit <- function(resp = c(), os = c(), ostime = c(), snps = c(), gender = c(), covars = c(), option = c(type = c(), male = c(), female = c(), MAF_v = 0),
    model = c(), data) {
    requireNamespace("survival")
    if (length(model) == 0) {
        stop("Model type needed.")
    } else if (model == "survival") {
        if (length(os) == 0 || length(ostime) == 0) {
            stop("Survival information needed.")
        }
        if (length(option$type) != 0) {
            modeltype <- option$type
        } else {
            modeltype <- "all"
        }
    } else if (model %in% c("binary", "linear")) {
        if (length(resp) == 0) {
            stop("Response variable needed.")
        }
        if (length(option$type) != 0) {
            modeltype <- option$type
        } else {
            modeltype <- "all"
        }
    } else {
        stop("Model type incorrect.")
    }

    if (length(gender) == 0) {
        stop("Gender information needed.")
    } else {
        gender_Lv <- levels(as.factor(data[, gender]))
    }

    if (length(option$male) != 0) {
        male <- option$male
        female <- option$female
        if (sum(c(male, female) %in% gender_Lv) != 2) {
            stop("Male and female information incorrect.")
        }
    } else {
        if (sum(c("0", "1") %in% gender_Lv) != 2) {
            stop("Male and female information needed.")
        }
        male <- 1
        female <- 0
    }

    MAF_select <- function(x) {
        T <- MAF(snp = x, gender = gender, male = male, MAF_v = option$MAF_v, data = data)
        return(T)
    }

    infor_all <- function(x) {
        T <- fit_all_models(resp = resp, os = os, ostime = ostime, snp = x, gender = gender, male = male, female = female, covars = covars, model = model, data = data)
        return(T)
    }

    infor_XCI <- function(x) {
        T <- fit_XCI_model(resp = resp, os = os, ostime = ostime, snp = x, gender = gender, male = male, female = female, covars = covars, model = model, data = data)
        return(T)
    }

    infor_XCI_E <- function(x) {
        T <- fit_XCI_E_model(resp = resp, os = os, ostime = ostime, snp = x, gender = gender, male = male, female = female, covars = covars, model = model, data = data)
        return(T)
    }

    infor_snp <- unlist(lapply(snps, MAF_select))
    snp_select <- infor_snp[(1:length(infor_snp)%%2 == 1)]
    snp_select_MAF <- as.numeric(infor_snp[(1:length(infor_snp)%%2 == 0)])


    if (modeltype == "all") {
        results <- base::lapply(snp_select, infor_all)
    }

    if (modeltype == "XCI") {
        results <- base::lapply(snp_select, infor_XCI)
    }

    if (modeltype == "XCI-E") {
        results <- base::lapply(snp_select, infor_XCI_E)
    }

    names(results) <- snp_select
    return(results)

}
