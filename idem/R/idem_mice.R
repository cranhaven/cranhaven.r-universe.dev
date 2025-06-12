#' Impute missing data by mice
#'
#' Conduct imputation using the NARFCS model implemented in the \code{mice}
#' package
#'
#' @inheritParams summary.IDEMDATA
#' @inheritParams imImpAll
#' @inheritParams imFitModel
#'
#' @param ... Parameters for \code{mice}
#' @return
#'
#' A class \code{IDEMIMP} list with components \describe{ \item{lst.var}{List of
#' parameters} \item{complete}{A dataset with the original data for the subset
#' of subjects who died at the end of the study or had no missing outcomes and
#' the \code{n.imp} imputed missing outcomes for subjects who need missing value
#' imputation. } \item{n.imp}{Number of imputed complete datasets}
#' \item{deltas}{Imputation sensitivity parameters} \item{org.data}{Original
#' dataset} }
#'
#' @examples
#'
#' \dontrun{
#' rst.abc <- imData(abc, trt="TRT", surv="SURV", outcome=c("Y1","Y2"),
#'                  y0=NULL, endfml="Y2",
#'                  trt.label = c("UC+SBT", "SAT+SBT"),
#'                  cov=c("AGE"), duration=365, bounds=c(0,100));
#' rst.imp <- imImpAll_mice(rst.abc, deltas=c(-0.25,0,0.25));}
#'
#' @export
#'
imImpAll_mice <- function(im.data, deltas = 0, n.imp  = 5,
                          endponly = TRUE, seed = NULL, ...) {

    f.addcols <- function(dset) {
        cbind('ID'    = 1:nrow(dset),
              'DELTA' = 0,
              'IMP'   = NA,
              dset);
    }

    stopifnot(get.const("IDEM.CLASS") %in% class(im.data));

    if (is.numeric(seed)) {
        old_seed <- .Random.seed;
        set.seed(seed);
    }

    data.all <- im.data$data;
    lst.var  <- im.data$lst.var;

    if (is.null(deltas)) {
        deltas <- 0;
    } else {
        if (!(0 %in% deltas))
            deltas <- c(deltas, 0);
    }

    voutcome <- NULL
    vy0      <- NULL
    vcov     <- NULL
    vtrt     <- NULL
    eoutcome <- NULL
    vsurv    <- NULL
    duration <- NULL
    endfml   <- NULL
    tmp.endp <- NULL
    bounds   <- NULL

    ##get parameters in current enviroment
    get.para(lst.var, environment());

    ##save org voutcome
    p_v <- paste(get.const("ORG.PREFIX"), voutcome, sep="")
    data.all[, p_v]      <- data.all[, voutcome]
    data.all[, "__id__"] <- seq_len(nrow(data.all))
    need.imp             <- summary(im.data,
                                    opt = "missid",
                                    endponly = endponly)

    ## transform outcome
    for (i in seq_len(length(voutcome))) {
        data.all[, voutcome[i]] <- get.transfer(data.all[, voutcome[i]],
                                                 bounds)
    }

    ##get complete data. return if no imputation needed
    if (0 == length(need.imp)) {
        rst <- data.all
        eval(parse(text = paste("tmp.endp <- with(rst, {", endfml, "})")))
        rst[[get.const("TXT.ENDP")]] <- tmp.endp
        rst <- f.addcols(rst)
        class(rst) <- c(class(rst), get.const("IMP.CLASS"));
        return(rst);
    } else if (nrow(data.all) == length(need.imp)) {
        data.comp <- NULL
    } else {
        data.comp <- data.all[-need.imp, ]
        data.comp <- f.addcols(data.comp)
    }

    ## get subjects who survived and transfer outcome
    data_surv <- data.all[data.all[, vsurv] > duration,
                          c(voutcome, vy0, vcov, vtrt, "__id__")]


    ## mice imputation by trt
    vec_method <- c(
        rep("mnar.norm", length(voutcome)),
        rep("", length(c(vy0, vcov)))
    )

    rst    <- data.comp
    rec_id <- nrow(rst)
    a.trt  <- get.trt(data.all[,vtrt]);
    for (i in 1:length(a.trt)) {
        cur_data <- data_surv[which(a.trt[i] == data_surv[, vtrt]), ]
        cur_d    <- cur_data[, c(voutcome, vy0, vcov)]

        imp_id  <- cur_data[, "__id__"]
        imp_inx <- which(imp_id %in% need.imp)
        imp_id  <- imp_id[imp_inx]

        if (0 == length(imp_inx))
            next

        for (j in deltas) {
            ## add 0 * to avoid bug in mice because of %*% delta
            t_ums            <- paste(j, "+0*", c(vy0, vcov)[1], sep = "")
            mnar.blot        <- rep(list(ums = t_ums), length(voutcome))
            names(mnar.blot) <- voutcome

            cur_imp <- mice(cur_d, m = n.imp,
                            method = vec_method, blots = mnar.blot,
                            ...)

            ## print(cur_imp$logged)

            ## append to results
            cur_imp_data <- data.all[imp_id, ]
            for (k in seq_len(n.imp)) {
                cur_complete             <- mice::complete(cur_imp, k)
                cur_imp_data[, voutcome] <- cur_complete[imp_inx, voutcome]
                cur_imp_data$ID     <- rec_id + seq_len(nrow(cur_imp_data))
                cur_imp_data$DELTA  <- j
                cur_imp_data$IMP    <- k
                rst <- rbind(rst, cur_imp_data)
            }
            rec_id <- rec_id + length(imp_id)
        }
    }

    ## transform back outcome
    for (i in seq_len(length(voutcome))) {
        rst[, voutcome[i]] <- get.inv.transfer(rst[, voutcome[i]],
                                               bounds)
    }

    ## compute endpoint
    eval(parse(text = paste("tmp.endp <- with(rst, {", endfml, "})")))
    rst[[get.const("TXT.ENDP")]] <- tmp.endp

    ##reset seed
    if (is.numeric(seed)) {
        .Random.seed <- old_seed
    }

    ##return
    rownames(rst) <- NULL
    rtn.rst <- list(lst.var  = lst.var,
                    deltas   = deltas,
                    normal   = TRUE,
                    org.data = im.data$data,
                    n.imp    = n.imp,
                    imp.par  = list(...),
                    use_mice = TRUE,
                    complete = rst)

    class(rtn.rst) <- c(class(rtn.rst),
                        get.const("IMP.CLASS"))

    invisible(rtn.rst)
}
