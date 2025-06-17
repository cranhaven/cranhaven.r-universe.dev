BIC_BP <-
function(pixeldata,f_local,turning_no,f_sign,turning) {

        BICBP <- c()
        N <- length(pixeldata)
        
        DETREND_DS <- DETREND_SJ(pixeldata,'linear',c())
        detrend_data_series <- DETREND_DS$y
        param_no <- DETREND_DS$param_no
        RSS <- sum(detrend_data_series^2)
        BICBP <- c(BICBP,param_no*log(N)+N*(log(2*pi*RSS/N)+1))
        BICBP_Idx <- c(0)
        for(i in 1:turning_no) {
                
                DTSBP <- DBEST_timeseries_BP(pixeldata,i,f_local,turning_no,f_sign,turning)
                Param_no <- DTSBP$Param_no
                RSS <- DTSBP$RSS
                BICBP_Idx <- c(BICBP_Idx, i)
                BICBP <- c(BICBP,Param_no*log(N)+N*(log(2*pi*RSS/N)+1))
        }
        

        sortBICBP <- sort(BICBP)
        BIC_BP_ord <- order(match(BICBP,sortBICBP))
        BIC_BP_no <- BICBP_Idx[BIC_BP_ord[1]]
        
        return(BIC_BP_no)
}
