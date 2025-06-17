prepare_data <-
function(data, lower1, upper1, lower2, upper2) {
    # prepare_data creates a data structure for use of the functions
    # that estimate correlation coefficients and their confidence
    # intervals.
    #
    # Arguments
    #  data: A data frame containing variables as named in lower1, upper1,
    #        lower2, upper2
    #  lower1: The variable defining the lower limit of the interval containing
    #          the observed value of the first variable
    #  upper1: The variable defining the upper limit of the interval containing
    #          the observed value of the first variable
    #  lower2: The variable defining the lower limit of the interval containing
    #          the observed value of the second variable
    #  upper2: The variable defining the upper limit of the interval containing
    #          the observed value of the second variable
    
    Y1 <- prepare_variable(data, lower1, upper1)
    Y2 <- prepare_variable(data, lower2, upper2)

    Obs <- list(NULL, NULL)
    Miss <- list(NULL, NULL)
    Left <- list(NULL, NULL)
    Right <- list(NULL, NULL)
    Interval <- list(NULL, NULL)
    for (j in 1:2) {

        Y <- list(Y1, Y2)[[j]]

        Obs[[j]] <- which(is.finite(Y[,1]) & is.finite(Y[,2]) & (Y[,1]==Y[,2]))
        Miss[[j]] <- which(is.na(Y[,1]) & is.na(Y[,2]))
        Left[[j]] <- which(is.na(Y[,1]) & is.finite(Y[,2]))
        Right[[j]] <- which(is.finite(Y[,1]) & is.na(Y[,2]))
        Interval[[j]] <- which(is.finite(Y[,1]) & is.finite(Y[,2]) &
                               (Y[,1]<Y[,2]))
    }

    Q <- list(Obs=Obs, Miss=Miss, Left=Left, Right=Right,
              Interval=Interval, Y1=Y1, Y2=Y2)
        
    return(Q)
}
