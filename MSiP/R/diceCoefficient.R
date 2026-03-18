#' diceCoefficient
#'
#' @param datInput Data frame with column names: Experiment.id, Replicate, Bait,
#' Prey, and count (i.e., prey count).
#' @return Data frame containing bait-prey pairs with Dice coefficient score,
#' a number between 0 and 1
#'
#' @references Zhang, B., Park, B.-H., Karpinets, T., and Samatova, N. F. (2008).
#' From pull-down data to protein interaction networks and complexes with biological relevance.
#' Bioinformatics 24, 979-986.
#'
#' @author Matineh Rahmatbakhsh, \email{matinerb.94@gmail.com}
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
#' @description This function applies Dice coefficient to score instances
#' (e.g., bait-prey interactions (BPIs) in the data.frame.The Dice coefficient was first applied by Zhang et al., 2008
#' to score interactions between all identified proteins (baits and preys) in a given AP-MS experiment.
#' @export
#' @examples
#' data(SampleDatInput)
#' datScoring <- diceCoefficient(SampleDatInput)
#' head(datScoring)

diceCoefficient <- function(datInput) {

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

    dice.sim <-
        lapply(pair.list, function(x){
            (2 * length(which(x[[1]] & x[[2]])))/ (sum(x[[1]]) + sum(x[[2]]))
        }
        )

    names(dice.sim) <-
        pair.list.name

    output <-
        do.call(rbind, dice.sim) %>%
        as.data.frame(.) %>%
        rownames_to_column("BPI") %>%
        rename(Dice= V1)

    output[is.na(output)] <- 0


    return(output)
}

