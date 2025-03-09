#' Create example data
#'
#' Example data for ProteomeDiscoverer, Spectronaut, DIA-NN and MaxQuant.
#'
#' Data for each software for testing functions of flowTraceR. Additional example data for Spectronaut and DIA-NN for analyzing retention time distribution on precursor level.
#'
#' @param example Choose between \code{"ProteomeDiscoverer"}, \code{"Spectronaut"}, \code{"DIA-NN"} and \code{"MaxQuant"} or for an example for downstream analysis \code{"RetentionTime"}. Default is MaxQuant.
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#' @import comprehenr
#'
#' @return This function returns example data as dataframe for the respective chosen \code{example}. For \code{"MaxQuant"} a list with evidence/proteingroup dataframe. For \code{"RetentionTime"} a list with Spectronaut/DIA-NN data including retention time information.
#'
#' @export
#'
#' @examples
#'
#' # Spectronaut example data
#' Spectronaut_data <- get_example(example = "Spectronaut")



#generate dummy data
get_example <- function(example = c("MaxQuant", "DIA-NN", "Spectronaut", "PD", "RetentionTime")) {

  if (example[1] == "DIA-NN") {
    tibble::tibble(
      Protein.Group = c("P05154", "P50406", "P02768", "P02768", "P02766", "P01008", "P01764", "Q14624", "Q14624", "P02671", "Q92496", "EXAMPLE1; EXAMPLE2"),
      Precursor.Id = c("AAAATGTIFTFR2", "AAAAVNFFNIDPAEPELRPHPLGIPTN4", "AAC(UniMod:4)LLPK1", "AAC(UniMod:4)LLPK2", "AADDTWEPFASGK1", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK2", "AEDTAVYYC(UniMod:4)AK2", "RLDYQEGPPGVEISC(UniMod:4)WSVEL2", "RLDYQEGPPGVEISC(UniMod:4)WSVEL3", "RLEVDIDIK2", "EGIVEYPR2", "COMMON2")
    )

  } else if (example[1] == "Spectronaut") {
    tibble::tibble(
      PG.ProteinGroups = c("P02768", "A0A0J9YY99", "A0A0B4J1X5", "Q14624", "Q14624", "Q02985", "P02671", "P02671", "P02671", "P02671", "P02671", "EXAMPLE2"),
      EG.PrecursorId = c("_AAC[Carbamidomethyl (C)]LLPK_.1", "_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_AEDTAVYYC[Carbamidomethyl (C)]AR_.2", "_AISGGSIQIENGYFVHYFAPEGLTTM[Oxidation (M)]PK_.3","_AISGGSIQIENGYFVHYFAPEGLTTMPK_.3", "_EGIVEYPR_.2", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAM[Oxidation (M)]DLGTLSGIGTLDGFR_.3", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAMDLGTLSGIGTLDGFR_.3", "_M[Oxidation (M)]KPVPDLVPGNFK_.2", "_M[Oxidation (M)]KPVPDLVPGNFK_.3", "_RLEVDIDIK_.2", "_COMMON_.2")
    )

  } else if (example[1] == "MaxQuant") {
    evidence <- tibble::tibble(
      "Modified sequence" = c("_AACLLPK_", "_AAFTECCQAADK_", "_AEAQAQYSAAVAK_", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "_ANTVQEATFQM(Oxidation (M))ELPK_", "_ANTVQEATFQMELPK_", "_AQLVDM(Oxidation (M))K_", "_ASGVPDRFSGSGSGTDFTLK_", "_DQFNLIVFSTEATQWRPSLVPASAENVNK_", "_DQFNLIVFSTEATQWRPSLVPASAENVNK_"),
      Charge = c(2, 2, 2, 2, 2, 3, 2, 3, 3, 4),
      "Protein group IDs" = c(26, 26, 202, 86, 202, 202, 86, 3, 202, 202)
    )

    proteinGroups <- tibble::tibble(
      "Protein IDs" = c("A0A075B6P5;P01615;A0A087WW87;P01614;A0A075B6S6", "P02768", "P02671;P02672", "Q14624"),
      id = c(3, 26, 86, 202)
    )

    list(
      "evidence" = evidence,
      "proteinGroups" = proteinGroups
    )

  } else if (example[1] == "PD") {
    tibble::tibble(
      "Protein Accessions" = c("Q14624", "P01764", "P02766", "P55056", "P02768", "A0A0B4J1V0; A0A0B4J1Y9; A0A0B4J1V6", "P05154", "P02787", "P02768"),
      "Annotated Sequence" = c("ANTVQEATFQMELPK", "GLEWVSAISGSGGSTYYADSVK", "AADDTWEPFASGK", "DGWQWFWSPSTFR", "AAcLLPK", "GLEWVGR", "DFTFDLYR", "DYELLcLDGTRKPVEEYANcHLAR", "LVRPEVDVmcTAFHDNEETFLK"),
      Modifications = c("", "", "", "", "C3(Dummy_Modification)", "", "", "C6(Carbamidomethyl); C20(Carbamidomethyl)", "M9(Oxidation); C10(Carbamidomethyl)"),
      Charge = c(2, 2, 2, 2, 2, 2, 2, 2, 2)
    )

  } else if (example[1] == "RetentionTime") {
    diann <- tibble::tibble(
      Protein.Group = c("P05154", "P50406", "P02768", "P02768", "P02766", "P01008", "P01764", "Q14624", "Q14624", "P02671", "Q92496"),
      Precursor.Id = c("AAAATGTIFTFR2", "AAAAVNFFNIDPAEPELRPHPLGIPTN4", "AAC(UniMod:4)LLPK1", "AAC(UniMod:4)LLPK2", "AADDTWEPFASGK1", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK2", "AEDTAVYYC(UniMod:4)AK2", "RLDYQEGPPGVEISC(UniMod:4)WSVEL2", "RLDYQEGPPGVEISC(UniMod:4)WSVEL3", "RLEVDIDIK2", "EGIVEYPR2"),
      RT = c(85.8966, 109.9670, 45.7275, 45.6332, 74.6573, 33.1148, 40.8000, 110.1100, 110.0940, 68.6853, 49.5694)
    )

    spectronaut <- tibble::tibble(
      PG.ProteinGroups = c("P02768", "A0A0J9YY99", "A0A0B4J1X5", "Q14624", "Q14624", "Q02985", "P02671", "P02671", "P02671", "P02671", "P02671"),
      EG.PrecursorId = c("_AAC[Carbamidomethyl (C)]LLPK_.1", "_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_AEDTAVYYC[Carbamidomethyl (C)]AR_.2", "_AISGGSIQIENGYFVHYFAPEGLTTM[Oxidation (M)]PK_.3", "_AISGGSIQIENGYFVHYFAPEGLTTMPK_.3", "_EGIVEYPR_.2", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAM[Oxidation (M)]DLGTLSGIGTLDGFR_.3", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAMDLGTLSGIGTLDGFR_.3", "_M[Oxidation (M)]KPVPDLVPGNFK_.2", "_M[Oxidation (M)]KPVPDLVPGNFK_.3", "_RLEVDIDIK_.2"),
      EG.ApexRT = c(45.72715, 40.81445, 43.18760, 107.83633, 109.47858, 49.56935, 107.60022, 110.35444, 71.27575, 71.32513, 68.68530)
    )

    list(
      "DIA-NN" = diann,
      "Spectronaut" = spectronaut
    )
  }
}
