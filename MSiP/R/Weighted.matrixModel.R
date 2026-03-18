#' Weighted.matrixModel
#' @param datInput Data frame with column names: Experiment.id, Replicate, Bait,
#' Prey, and count (i.e., prey count).
#' @return Data frame containing bait-prey pairs with k (i.e.,number of co-purifications) &
#' logHG (i.e., $-1$*log(P-val) of the hypergeometric test)
#'
#' @author Matineh Rahmatbakhsh, \email{matinerb.94@gmail.com}
#'
#' @references Drew, K., Lee, C., Huizar, R. L., Tu, F., Borgeson, B., McWhite, C. D., et al. (2017).
#' Integration of over 9,000 mass spectrometry experiments builds a global map of human protein complexes.
#' Mol. Syst. Biol. 13, 932.
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_each
#' @importFrom dplyr funs
#' @importFrom dplyr select
#' @importFrom utils combn
#' @importFrom tibble rownames_to_column
#' @importFrom stats phyper
#' @importFrom tidyr separate
#' @importFrom magrittr set_rownames
#' @description This function computes the weighted matrix model for instances
#' (e.g., bait-prey interactions (BPIs)) in the data.frame.The output of the weighted matrix model
#' includes the number of experiments for which the pair of proteins is co-purified
#' (i.e., k) and -1*log(P-value) of the hypergeometric test (i.e., logHG)
#' given the experimental overlap value, each protein's total number of observed experiments,
#' and the total number of experiments (Drew et al., 2017).
#' @export
#' @examples
#' data(SampleDatInput)
#' datScoring <- Weighted.matrixModel(SampleDatInput)
#' head(datScoring)


Weighted.matrixModel <- function(datInput) {

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
    Experiment.id <- NULL
    Bait <- NULL
    Prey <- NULL
    protein <- NULL
    Cn <- NULL
    . <- NULL
    k <- NULL
    gene_pair <- NULL
    InteractorA <- NULL
    UniprotID <- NULL
    InteractorB <- NULL
    n <- NULL
    N <- NULL
    m <- NULL
    HG <- NULL
    ObsExp <- NULL







    datInput <-
        datInput %>%
        dplyr::select(Experiment.id,Bait,Prey)

    datInput <-
        datInput[!duplicated(datInput),]


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
        as.data.frame(.) %>%
        set_rownames(.$Experiment.id)%>%
        dplyr::select(-1)


    names <-
        colnames(datInput1)

    m.list <-
        as.list(as.data.frame(datInput1))

    protInt <-
        lapply(m.list, function(x) sum(x))

    protInt <-
        data.frame(UniprotID=names(protInt),
            ObsExp=unlist(protInt))

    pair.list <-
        combn(m.list, 2, simplify = FALSE)
    pair.list.name <-
        combn(names(m.list),
            2, FUN = paste0,
            collapse = "~",simplify = FALSE)
    CoPurifed <-
        lapply(pair.list, function(x){
            length(which(x[[1]] & x[[2]]))
        })

    names(CoPurifed) <-
        pair.list.name

    CoPurifed.df <-
        data.frame(gene_pair=names(CoPurifed),
            k=unlist(CoPurifed)) %>%
        filter(k > 1) %>%
        separate(gene_pair,
            c("InteractorA", "InteractorB"), sep = "~", remove = F) %>%
        dplyr::select(-1)

    PPIscore <-
        CoPurifed.df %>%
        dplyr::rename(UniprotID=InteractorA) %>%
        left_join(., protInt) %>%
        dplyr::rename(n=ObsExp) %>%
        dplyr::rename(InteractorA=UniprotID) %>%
        dplyr::rename(UniprotID=InteractorB) %>%
        left_join(., protInt) %>%
        dplyr::rename(m=ObsExp) %>%
        dplyr::rename(InteractorB=UniprotID) %>%
        mutate(N = max(datInput$Experiment.id)) %>%
        mutate(HG = 1- phyper(k-1, n, N-n, m)) %>%
        mutate(logHG = -1 * log(HG)) %>%
        mutate(BPI = paste(InteractorA,InteractorB, sep = "~"))%>%
        dplyr::select(9,3,8)


    return(PPIscore)
}





