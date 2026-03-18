#' overlapScore
#' @param datInput Data frame with column names: Experiment.id, Replicate, Bait,
#' Prey, and count (i.e., prey count).
#' @return Data frame containing bait-prey pairs with the overlap score,
#' a number between 0 and 1
#' @author Matineh Rahmatbakhsh, \email{matinerb.94@gmail.com}
#' @references Nepusz, T., Yu, H., and Paccanaro, A. (2012a).
#' Detecting overlapping protein complexes in protein-protein interaction networks. Nat. Methods 9, 471.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_each
#' @importFrom dplyr funs
#' @importFrom dplyr select
#' @importFrom utils combn
#' @importFrom tibble rownames_to_column
#' @description This function computes the overlap similarity scores for instances
#' (e.g., bait-prey interactions (BPIs)) in the data.frame.
#' @export
#' @examples
#' data(SampleDatInput)
#' datScoring <- overlapScore(SampleDatInput)
#' head(datScoring)

overlapScore <- function(datInput) {

    if(!is.data.frame(datInput)){
        stop("Input data should be data.frame")
    }


    if(!is.data.frame(datInput)){
        stop("Input data should be data.frame")
    }

    if(all(colnames(datInput) != "Experiment.id") == TRUE){
        stop("Experiment.id is absent from the data.frame")
    }

    if(colnames(datInput)[1] != "Experiment.id"){
        stop("Experiment.id must be included in the first column")
    }

    if(all(colnames(datInput) != "Bait") == TRUE){
        stop("Bait is absent from the data.frame")
    }

    if(colnames(datInput)[3] != "Bait"){
        stop("Bait must be included in the third column")
    }

    if(all(colnames(datInput) != "Prey") == TRUE){
        stop("Prey is absent from the data.frame")
    }

    if(colnames(datInput)[4] != "Prey"){
        stop("Prey must be included in the fourth column")
    }

    Experiment.id <- NULL
    Bait <- NULL
    Prey <- NULL
    protein <- NULL
    Cn <- NULL
    V1 <- NULL
    . <- NULL


    datInput <-
        datInput %>%
        dplyr::select(Experiment.id,Bait,Prey)

    datInput <- #remove the duplicates
        datInput[!duplicated(datInput),]


    suppressWarnings(
        datInput1 <-
            datInput %>%
            gather("key", "protein", 2:3) %>%
            dplyr::select(-2) %>%
            mutate(Cn = 1) %>%
            group_by(Experiment.id) %>%
            mutate(row = row_number()) %>%
            spread(protein, Cn) %>%
            dplyr::select(-row) %>%
            group_by(Experiment.id) %>%
            replace(is.na(.),0) %>%
            summarise_each(funs(max(.))) %>%
            dplyr::select(-1))


    names <-
        colnames(datInput1)

    m.list <-
        as.list(as.data.frame(datInput1))

    #Compute similarity using Dice coefficient

    pair.list <-
        combn(m.list, 2, simplify = FALSE)

    pair.list.name <-
        combn(names(m.list), 2,
            FUN = paste0, collapse = "~",simplify = FALSE)

    overlap.sim <-
        lapply(pair.list, function(x){
            (length(which(x[[1]] & x[[2]]))^2)/(length(which(x[[1]] != 0)) * length(which(x[[2]] != 0)))
        }
        )

    names(overlap.sim) <-
        pair.list.name

    output <-
        do.call(rbind, overlap.sim) %>%
        as.data.frame(.) %>%
        rownames_to_column("BPI") %>%
        rename(Overlap= V1)

    output[is.na(output)] <- 0


    return(output)
}

