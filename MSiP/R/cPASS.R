#'cPASS
#' @param datInput Data frame with column names: Experiment.id, Replicate, Bait,
#' Prey, and count (i.e., prey count).
#'
#' @return Data frame containing bait-prey pairs with average peptide spectrum match (PSMs), total PSMs,
#' ratio total PSMs,Z-score,S-score,D-score and WD-score.
#'
#' @author Matineh Rahmatbakhsh, \email{matinerb.94@gmail.com}
#'
#' @references Huttlin, E. L., Ting, L., Bruckner, R. J., Gebreab, F., Gygi, M. P., Szpyt, J., et al. (2015).
#' The BioPlex Network: A Systematic Exploration of the Human Interactome. Cell 162, 425-440.
#' @references Sowa, M. E., Bennett, E. J., Gygi, S. P., and Harper, J. W. (2009).
#' Defining the human deubiquitinating enzyme interaction landscape. Cell 138, 389-403.
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_each
#' @importFrom dplyr funs
#' @importFrom dplyr select
#' @importFrom utils combn
#' @importFrom tibble rownames_to_column
#' @importFrom stats phyper
#' @importFrom tidyr separate
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom magrittr set_rownames
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom plyr desc
#' @importFrom stats quantile
#' @importFrom dplyr summarize
#' @description This function applies Comparative Proteomic Analysis Software Suite (CompPASS) model
#'to score instances (e.g., bait-prey interactions (BPIs) in the data.frame.
#'The CompPASS is a robust statistical scoring scheme for assigning confidence scores to bait-prey
#'interactions (Sowa et al., 2009).This function was based on the source code.
#' \url{https://github.com/dnusinow/cRomppass}
#' @export
#' @examples
#' data(SampleDatInput)
#' datScoring <- cPASS(SampleDatInput)
#' head(datScoring)




cPASS <- function(datInput) {


    if(!is.data.frame(datInput)){
        stop("Input data should be data.frame")
    }


    if(!is.data.frame(datInput)){
        stop("Input data should be data.frame")
    }

    if(colnames(datInput)[1] != "Experiment.id"){
        stop("Experiment.id must be included in the first column")
    }

    if(colnames(datInput)[2] != "Replicate"){
        stop("Replicate must be included in the second column")
    }

    if(colnames(datInput)[3] != "Bait"){
        stop("Bait must be included in the third column")
    }

    if(colnames(datInput)[4] != "Prey"){
        stop("Prey must be included in the fourth column")
    }

    if(colnames(datInput)[5] != "counts"){
        stop("counts must be included in the fifth column")
    }

    ObsExp <- NULL
    Experiment.id <- NULL
    Bait <- NULL
    Prey <- NULL
    Replicate <- NULL
    counts <- NULL
    MaxSpec <- NULL
    TotalPSM <- NULL
    intBait <- NULL
    . <- NULL
    meanPint <- NULL
    meandiff <- NULL
    freq_b <- NULL
    SD <- NULL
    WD_inner <- NULL
    WD_raw <- NULL





    ##function for entropy

    entropy <- function(xs) {
        p <- (xs + 1/length(xs)) / (sum(xs) + 1)
        ent <- sum(sapply(p, function(x) { -1*x*log(x, 2) }))
        return(ent)
    }



    #total number of experiment
    N <-
        length(unique(datInput$Experiment.id))



    #number prey captured across runs
    p_sum <-
        unique(datInput[, c("Bait", "Prey")])
    p_sum <-
        p_sum %>%
        group_by(Prey) %>%
        summarise(p_sum = n())


    #bait-prey pair captured across runs
    l <-
        unique(datInput[, c("Bait", "Prey")])

    l <-
        l %>%
        group_by(Bait, Prey) %>%
        summarise(l = n()) %>%
        mutate(b_p = paste(Bait, Prey, sep = "~")) %>%
        select(4,1,2,3)


    # compute Entropy using maximum count
    entropy <-
        datInput %>%
        group_by(Experiment.id, Prey, Replicate) %>%
        summarise(Bait = unique(Bait),
            MaxSpec = max(counts)) %>%
        ungroup() %>%
        group_by(Experiment.id,Prey) %>%
        summarise(Bait = unique(Bait),
            Entropy = entropy(MaxSpec)) %>%
        ungroup() %>%
        as.data.frame() %>%
        mutate(b_p = paste(Bait, Prey, sep = "~")) %>%
        dplyr::select(5,1,3,2,4)


    #average counts for bait_prey across replicates
    AvePSM <-
        datInput %>%
        group_by(Bait, Prey) %>%
        summarise(AvePSM = mean(counts)) %>%
        mutate(b_p = paste(Bait, Prey, sep = "~"))%>%
        dplyr::select(4,1,2,3)

    other_Stats <-
        datInput %>%
        group_by(Bait, Prey) %>%
        summarise(AvePSM = mean(counts)) %>%
        ungroup() %>%
        group_by(Prey) %>%
        mutate(TotalPSM = sum(AvePSM)) %>%
        mutate(Ratio_totalPSM =AvePSM/TotalPSM) %>%
        dplyr::mutate(Ratio = n()/N) %>%
        mutate(b_p = paste(Bait, Prey, sep = "~"))%>%
        dplyr::select(7,1,2,4:6)


    #number of prey that interact with one bait only
    P_int_a_Bait <-
        AvePSM %>%
        group_by(Prey) %>%
        dplyr::summarise(intBait = n()) %>%
        filter(intBait == 1)


    datStat <-
        AvePSM %>%
        left_join(., p_sum, by = "Prey") %>%
        left_join(., l[, c("b_p", "l")], by = "b_p") %>%
        left_join(., entropy[, c("b_p", "Entropy")], by = "b_p") %>%
        left_join(.,
            other_Stats[, c("b_p", "TotalPSM","Ratio_totalPSM","Ratio")], by = "b_p")%>%
        mutate(N = N) %>%
        mutate(freq_b =
                ifelse((Prey %in% P_int_a_Bait$Prey),N, N-p_sum))

    datStat1 <-
        AvePSM %>%
        dplyr::select(3,2,4) %>%
        spread(Bait, AvePSM) %>%
        as.data.frame(.) %>%
        set_rownames(.$Prey) %>%
        dplyr::select(-1)

    aveIntPrey <-
        rowSums(as.matrix(datStat1), na.rm = TRUE) / N



    Prey_Calc <-
        data.frame(
            Prey = names(aveIntPrey),
            meanPint = aveIntPrey,
            meandiff = rowSums((as.matrix(datStat1) - aveIntPrey)^2, na.rm = TRUE)) %>%
        left_join(.,datStat[, c("Prey", "freq_b")], by = "Prey") %>%
        distinct(Prey, meanPint,meandiff,freq_b, .keep_all = TRUE) %>%
        mutate(SD = sqrt((meandiff + ((meanPint^2) * (freq_b)))/(N-1)))

    dff <-
        left_join(datStat, Prey_Calc[, c("Prey", "meanPint", "SD")], by = "Prey")

    Scored_file <-
        dff %>%
        mutate(S_score = sqrt((AvePSM) * (N) / (p_sum))) %>%
        mutate(Z_score = (AvePSM - meanPint) / (SD)) %>%
        mutate(D_score = sqrt((AvePSM) * (((N) / (p_sum))^l))) %>%
        mutate(WD_inner = (N / p_sum) * (SD / meanPint)) %>%
        mutate(WD_raw = sqrt(AvePSM * (WD_inner^l)))


    # Weighted WD score
    s <-
        Scored_file %>%
        arrange(desc(WD_raw))
    WD_raw.factor <-
        as.numeric(quantile(s$WD_raw, 0.98)[1])

    Scored_file$WD_score <-
        Scored_file$WD_raw /WD_raw.factor


    Scored_file <-
        as.data.frame(Scored_file[, c("Bait", "Prey", "AvePSM",
            "TotalPSM","Ratio_totalPSM","Ratio",
            "S_score", "Z_score", "D_score",
            "WD_score", "Entropy")])
    return(Scored_file)
}






