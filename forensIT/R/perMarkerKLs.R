#' @title perMarkerKLs
#' @description perMarkerKLs
#'
#' @param ped Reference pedigree.
#' @param frequency Allele frequency database.
#' @param MP missing person
#' @return An object of class data.frame with KLs.
#' @export
#' @importFrom pedprobr oneMarkerDistribution
#' @import forrel
#' @importFrom mispitools getfreqs
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
#' library(forrel)
#' x = linearPed(2)
#' plot(x)
#' x = setMarkers(x, locusAttributes = NorwegianFrequencies[1:5])
#' x = profileSim(x, N = 1, ids = 2)
#' perMarkerKLs(x, MP = 5 , NorwegianFrequencies[1:5])


perMarkerKLs <- function(ped, MP, frequency) {
KLpedpop <- list()
KLpopped <- list()

Allele2 <- Allele1 <- Genotype <- NULL

for (i in 1:length(ped$MARKERS)) { #nolint
df <- as.data.frame(pedprobr::oneMarkerDistribution(ped, MP,i))
names(df) <- "CPT"
df$Genotype <- rownames(df)
rownames(df) <- NULL
df <- df %>%
  dplyr::mutate(Allele1 = sapply(strsplit(as.character(Genotype), "/"), `[`, 1),
         Allele2 = sapply(strsplit(as.character(Genotype), "/"), `[`, 2))

pop <- as.data.frame(frequency[i])

pop$Allele <- rownames(pop)

rownames(pop) <- NULL

names(pop) <- c("Allele","freq")

df <- df %>% mutate(RPT = ifelse(pop$freq[match(Allele1, pop$Allele)] == pop$freq[match(Allele2, pop$Allele)], 
                                 pop$freq[match(Allele1, pop$Allele)] * pop$freq[match(Allele2, pop$Allele)], 
                                 2 * pop$freq[match(Allele1, pop$Allele)] * pop$freq[match(Allele2, pop$Allele)]))
df <- replace(df, is.na(df) | df == 0, 1e-20)

KLpedpop[[i]]<-sum(df$CPT*(log10(df$CPT) - log10(df$RPT)))
KLpopped[[i]]<-sum(df$RPT*(log10(df$RPT) - log10(df$CPT)))
}
markName <- names(frequency)
data <- cbind(markName, as.data.frame(cbind(KLpopped)),as.data.frame(cbind(KLpedpop)))
return(data)}
