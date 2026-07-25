triangularMatrix <- function(factorLvls, output, statType) {
    ## takes a bunch of output (from "multiplComp") and presents it as a matrix

    statsMatrix <- as.matrix(output)
    colNames <- colnames(statsMatrix)

    condition <- FALSE
    if(!is.null(colNames)) {
        if (all(colnames(statsMatrix) == c("Estimate", "Tukey.L", "Tukey.U", "Tukey.p"))) {
            condition <- TRUE

            Nlev <- length(factorLvls)
            rns <- c()
            for (i in 1:(Nlev-1))
                rns <- c(rns, paste(factorLvls[i], " - ", factorLvls[(i+1):Nlev]))


            output.df <- as.data.frame(statsMatrix, stringsAsFactors = TRUE)
            output.df$name <- rownames(output.df)

            ## No idea what these three lines are supposed to be doing ...
            ## fake <- data.frame(name = rns)
            ## statsMatrix <- as.matrix(merge(output.df, fake, by = "name", all.y = TRUE)[, -1])
            ## rownames(statsMatrix) <- rns

            statsMatrix <- output
        }
        else condition <- FALSE
    } else {
        condition <- FALSE
    }

    if (statType == "estimates") {
        if (condition)
            values <- statsMatrix[, 1]
        else
            values <- statsMatrix[1, ]
    } else if (statType == "p-values") {
        if (condition)
            values <- statsMatrix[, 4]
        else
            values <- statsMatrix[4, ]
    } else if (statType == "ci") {
        count <- 1
        i <- count

        if(condition) {
            values <- numeric(nrow(statsMatrix) * 2)
            while (count < nrow(statsMatrix) + 1) {
                values[c(i, i + 1)] <- c(statsMatrix[count, 2], statsMatrix[count, 3])
                count <- count + 1
                i <- i + 2
            }
        } else {
            values <- numeric(ncol(statsMatrix) * 2)
            while (count < ncol(statsMatrix) + 1) {
                values[c(i, i + 1)] <- c(statsMatrix[2, count], statsMatrix[3, count])
                count <- count + 1
                i = i + 2
            }
        }
    }

    num <- length(factorLvls)
    newMatrix <- matrix(NA, ncol <- num, nrow <- num)

    if (statType %in% c("estimates", "p-values")) {
        stopAt <- 0
        for(i in 1:num) {
            if (i == 1) startAt <- 1
            stopAt <- (stopAt - i) + num

            if (i == num) extra <- numeric(0)
            else extra <- values[startAt:stopAt]

            newMatrix[, i] <- format(c(rep("", num + i - num), signif(extra, 5)),
                                     width = 5, justify = "r")
            startAt <- stopAt + 1
        }
        rownames(newMatrix) <- factorLvls
        colnames(newMatrix) <- factorLvls
        x <- ncol(newMatrix)
        newMatrix <- as.matrix(newMatrix[-1, -x])

        if (x == 2) newMatrix <- t(newMatrix)
        rownames(newMatrix) <- factorLvls[2:length(factorLvls)]
        colnames(newMatrix) <- factorLvls[1:(length(factorLvls) - 1)]

    } else if (statType == "ci") {
        stopAt <- 0
        doubleNum <- num * 2
        newMatrix <- matrix(NA, ncol = num, nrow = doubleNum)

        for (i in 1:num) {
            if (i == 1) startAt <- 1
            stopAt <- (stopAt - i * 2) + doubleNum

            if (i == num) extra <- numeric(0)
            else extra <- values[startAt:stopAt]

            newMatrix[, i] <- format(c(rep("", (doubleNum + i - doubleNum) * 2),
                                       signif(extra, 5)), width = 5, justify = "r")
            startAt <- stopAt + 1
        }
        rowNames <- rep("", doubleNum)
        temp <- 1:doubleNum
        rowNames[(temp %% 2 != 0)] <- factorLvls

        x <- ncol(newMatrix)
        newMatrix <- as.matrix(newMatrix[-(1:2), -x])

        rownames(newMatrix) <- rowNames[-c(1, 2)]
        colnames(newMatrix) <- factorLvls[-x]
    }
    newMatrix
}
