#' Create example data
#'
#' Example data for ProteomeDiscoverer, Spectronaut, DIA-NN and MaxQuant.
#'
#' Example data is generated for each software for testing functions of mpwR. Each column is created in a randomized fashion. Connections between columns are not necessarily valid. E.g. column of Precursor Charges might not reflect charges of Precuror.IDs column.
#'
#' @author Oliver Kardell
#'
#' @return This function returns list with example data. Each list entry has filename and software information as well as a corresponding data set.
#'
#' @export
#'
#' @examples
#' data <- create_example()



#create dummy data
create_example <- function() {

 ordered_files <-   list(
    MQ = list(
      filename = "MaxQuant",
      software = "MaxQuant",
      data = list(
        "ev" = tibble::tibble(
          "Raw file" = rep(c("R01", "R02"), each = 5),
          "Proteins" = sample_replicates(c("P05154", "P50406", "P02768", "P02766", "P01008", "P01764", "Q14624")),
          "Modified sequence" = sample_replicates(c("_AAC(Carbamidomethyl (C))LLPK_", "_AAFTECCQAADK_", "_AEAQAQYSAAVAK_", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "_ANTVQEATFQM(Oxidation (M))ELPK_", "_AQLVDM(Oxidation (M))K_")),
          "Sequence" = sample_replicates(c("_AACLLPK_", "_AAFTECCQAADK_", "_AEAQAQYSAAVAK_", "_ALTDMPQMR_", "_ANTVQEATFQMELPK_", "_AQLVDMK_")),
          "Missed cleavages" = sample_replicates(c(0, 0, 2, 1, 2, 1, 3)),
          "Charge" =  sample_replicates(c(2, 2, 3, 2, 2, 3, 3)),
          "Retention time" = sample_replicates(c(44, 34, 43, 38, 37, 42, 45)),
          "Potential contaminant" = rep(c(""), times = 10),
          "Reverse" = rep(c(""), times = 10),
          "Intensity" = rep(1000, times = 10)
        ),
        "pep" = tibble::tibble(
          "Sequence" = sample(c("_AACLLPK_", "_AAFTECCQAADK_", "_AEAQAQYSAAVAK_", "_ALTDMPQMR_", "_ANTVQEATFQMELPK_", "_AQLVDMK_"), 5),
          "Missed cleavages" = sample(c(0, 0, 2, 1, 2, 1, 3), 5),
          "Last amino acid" = c("A", "B", "C", "D", "E"),
          "Amino acid after" = c("A", "B", "C", "D", "E"),
          "Potential contaminant" = rep(c(""), times = 5),
          "Reverse" = rep(c(""), times = 5),
          "Intensity R01" = c(4, 4, 3.9, 5, 6),
          "Intensity R02" = c(3, 3.5, 4, 5, 6),
          "LFQ intensity R01" = c(4, 4, 5, 5, 6),
          "LFQ intensity R02" = c(3, 3.5, 4, 4, 5)
        ),
        "pg" = tibble::tibble(
          "Protein IDs" = sample(c("P05154", "P50406", "P02768", "P02766", "P01008", "P01764", "Q14624"), 5),
          "Majority protein IDs" = sample(c("P05154", "P50406", "P02768", "P02766", "P01008", "P01764", "Q14624"), 5),
          "Peptide counts (all)" = c(1, 2, 2, 3, 3),
          "Potential contaminant" = rep(c(""), times = 5),
          "Reverse" = rep(c(""), times = 5),
          "Only identified by site" = rep(c(""), times = 5),
          "Intensity R01" = c(4, 4, 3.9, 5, 6),
          "Intensity R02" = c(3, 3.5, 4, 5, 6),
          "LFQ intensity R01" = c(4, 4, 5, 5, 6),
          "LFQ intensity R02" = c(3, 3.5, 4, 4, 5)
        )
      )
    ),
    DIANN = list(
      filename = "DIA-NN",
      software = "DIA-NN",
      data = list(
        "DIA-NN" = tibble::tibble(
          Run = rep(c("R01", "R02"), each = 5),
          Protein.Group = sample_replicates(c("P05154", "P50406", "P02768", "P02766", "P01008", "P01764", "Q14624")),
          Protein.Ids = sample_replicates(c("P05154", "P50406", "P02768", "P02768", "P02766", "P01008", "P01764", "Q14624")),
          Precursor.Id = sample_replicates(c("AAAATGTIFTFR2", "AAAAVNFFNIDPAEPELRPHPLGIPTN3", "AAC(UniMod:4)LLPK1", "AAC(UniMod:4)LLPK2", "AADDTWEPFASGK1", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK2")),
          Modified.Sequence = sample_replicates(c("AAAATGTIFTFR", "AAAAVNFFNIDPAEPELRPHPLGIPTN", "AAC(UniMod:4)LLPK", "AAC(UniMod:4)LLPK", "AADDTWEPFASGK", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK")),
          Stripped.Sequence = sample_replicates(c("AAAATGTIFTFR", "AAAAVNFFNIDPAEPELRPHPLGIPTN", "AACLLPK", "AACLLPK", "AADDTWEPFASGK", "ADGESCSASMMYQEGK")),
          PG.MaxLFQ = sample_replicates(c(5, 5, 7, 10, 12, 14, 12)),
          Precursor.Charge = sample_replicates(c(2, 2, 3, 2, 2, 3, 3)),
          RT = sample_replicates(c(21, 22, 25, 27, 27, 28, 20)),
          PG.Q.Value = rep(0.001, 10),
          Q.Value = rep(0.001, 10),
          Precursor.Quantity = rep(1000, 10)
        )
      )
    ),
    Spectronaut = list(
      filename = "Spectronaut",
      software = "Spectronaut",
      data = list(
        "Spectronaut" = tibble::tibble(
          EG.Identified = rep(TRUE, times = 10),
          R.FileName = rep(c("R01", "R02"), each = 5),
          PG.ProteinGroups = sample_replicates(c("P02768", "A0A0J9YY99", "A0A0B4J1X5", "Q14624", "Q14624", "Q02985", "P02671")),
          EG.PrecursorId = sample_replicates(c("_AAC[Carbamidomethyl (C)]LLPK_.1", "_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_AEDTAVYYC[Carbamidomethyl (C)]AR_.2", "_AISGGSIQIENGYFVHYFAPEGLTTM[Oxidation (M)]PK_.3", "_AISGGSIQIENGYFVHYFAPEGLTTMPK_.3", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAM[Oxidation (M)]DLGTLSGIGTLDGFR_.3")),
          EG.ModifiedPeptide = sample_replicates(c("_AAC[Carbamidomethyl (C)]LLPK_", "_AEDTAVYYC[Carbamidomethyl (C)]AK_", "_AEDTAVYYC[Carbamidomethyl (C)]AR_", "_AISGGSIQIENGYFVHYFAPEGLTTM[Oxidation (M)]PK_", "_AISGGSIQIENGYFVHYFAPEGLTTMPK_", "_EVVTSEDGSDC[Carbamidomethyl (C)]PEAM[Oxidation (M)]DLGTLSGIGTLDGFR_")),
          PEP.StrippedSequence = sample_replicates(c("_AACLLPK_", "_AEDTAVYYCAK_", "_AEDTAVYYCAR_", "_AISGGSIQIENGYFVHYFAPEGLTTMPK_", "_AISGGSIQIENGYFVHYFAPEGLTTMPK_", "_EVVTSEDGSDCPEAMDLGTLSGIGTLDGFR_")),
          FG.Charge = sample_replicates(c(2, 2, 3, 2, 2, 3, 3)),
          PEP.NrOfMissedCleavages = sample_replicates(c(0, 0, 2, 1, 2, 1, 3)),
          EG.ApexRT = sample_replicates(c(44, 34, 43, 38, 37, 42, 45)),
          PG.Quantity = sample_replicates(c(5, 5, 7, 10, 12, 14, 12)),
          PEP.Quantity = sample_replicates(c(5, 5, 7, 10, 12, 14, 12))
        )
      )
    ),
    PD = list(
      filename = "PD",
      software = "PD",
      data = list(
        "psm" = tibble::tibble(
          Confidence = rep("High", times = 10),
          `Spectrum File` = rep(c("R01", "R02"), each = 5),
          `Protein Accessions` = sample_replicates(c("Q14624", "P01764", "P02766", "P55056", "P02768", "A0A0B4J1V0; A0A0B4J1Y9; A0A0B4J1V6", "P05154")),
          `Annotated Sequence` = c("AAcLLPK", "GLEWVGR", "DFTFDLYR", "DYELLcLDGTRKPVEEYANcHLAR", "LVRPEVDVmcTAFHDNEETFLK", "AAcLLPK", "GLEWVGR", "DFTFDLYR", "DYELLcLDGTRKPVEEYANcHLAR", "LVRPEVDVMCTAFHDNEETFLK"),
          Modifications = c("C3(Dummy_Modification)", "", "", "C6(Carbamidomethyl); C20(Carbamidomethyl)", "M9(Oxidation); C10(Carbamidomethyl)", "C3(Dummy_Modification)", "", "", "C6(Carbamidomethyl); C20(Carbamidomethyl)", ""),
          `Number of Missed Cleavages` = sample_replicates(c(0, 0, 2, 1, 2, 1, 3)),
          Charge = sample_replicates(c(2, 2, 3, 2, 2, 3, 3)),
          `RT in min` = sample_replicates(c(44, 34, 43, 38, 37, 42, 45))
        ),
        "pep" = tibble::tibble(
           Confidence = rep("High", times = 5),
          `Number of Protein Groups` = c(1, 1, 1, 1, 1),
          `Number of Proteins`= c(1, 1, 1, 1, 1),
          `Number of PSMs`= c(1, 1, 1, 1, 1),
          `Sequence` = sample(c("AADDTWEPFASGK", "DGWQWFWSPSTFR", "AACLLPK", "GLEWVGR", "DFTFDLYR", "DYELLCLDGTRKPVEEYANCHLAR", "LVRPEVDVMCTAFHDNEETFLK"), 5),
          Modifications = sample(c("", "", "C3(Dummy_Modification)", "", "", "C6(Carbamidomethyl); C20(Carbamidomethyl)", "M9(Oxidation); C10(Carbamidomethyl)"), 5),
          `Number of Missed Cleavages` = sample(c(0, 0, 2, 1, 2, 1, 3), 5),
          `Found in Sample R01` = rep("High", times = 5),
          `Found in Sample R02` = rep("High", times = 5)
        ),
        "prot" = tibble::tibble(
          `Proteins Unique Sequence ID` = c(1, 2, 3, 4, 5),
          Description = c("A", "B", "C", "D", "E"),
          `Protein FDR Confidence Combined` = rep("High", times = 5),
          Accession = c("A", "B", "C", "D", "E"),
          `Found in Sample R01` = rep("High", times = 5),
          `Found in Sample R02` = rep("High", times = 5)
        ),
        "pg" = tibble::tibble(
          `Group Description` = c("A", "B", "C", "D", "E"),
          `Number of Proteins` = c(2, 2, 3, 2, 2),
          `Number of Unique Peptides` = c(3, 1, 4, 3, 4),
          `Protein Groups Protein Group ID` = c(1, 2, 3, 4, 5),
          `Found in Sample R01` = rep("High", times = 5),
          `Found in Sample R02` = rep("High", times = 5)
        )
      )
    ),
    Generic = list(
      filename = "Generic",
      software = "Generic",
      data = list(
        "Generic" = tibble::tibble(
          Run_mpwR = rep(c("R01", "R02"), each = 5),
          ProteinGroup.IDs_mpwR = sample_replicates(c("P02768", "A0A0J9YY99", "A0A0B4J1X5", "Q14624", "Q14624", "Q02985", "P02671")),
          Protein.IDs_mpwR = sample_replicates(c("P02768", "A0A0J9YY99", "A0A0B4J1X5", "Q14624", "Q14624", "Q02985", "P02671")),
          Peptide.IDs_mpwR = sample_replicates(c("AAAATGTIFTFR", "AAAAVNFFNIDPAEPELRPHPLGIPTN", "AAC(UniMod:4)LLPK", "AAC(UniMod:4)LLPK", "AADDTWEPFASGK", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK")),
          Precursor.IDs_mpwR = sample_replicates(c("AAAATGTIFTFR2", "AAAAVNFFNIDPAEPELRPHPLGIPTN3", "AAC(UniMod:4)LLPK1", "AAC(UniMod:4)LLPK2", "AADDTWEPFASGK1", "ADGESC(UniMod:4)SASM(UniMod:35)MYQEGK2")),
          Stripped.Sequence_mpwR = sample_replicates(c("AAAATGTIFTFR", "AAAAVNFFNIDPAEPELRPHPLGIPTN", "AACLLPK", "AACLLPK", "AADDTWEPFASGK", "ADGESCSASMMYQEGK")),
          Precursor.Charge_mpwR = sample_replicates(c(2, 2, 3, 2, 2, 3, 3)),
          Missed.Cleavage_mpwR = sample_replicates(c(0, 0, 2, 1, 2, 1, 3)),
          Retention.time_mpwR = sample_replicates(c(44, 34, 43, 38, 37, 42, 45)),
          ProteinGroup_LFQ_mpwR = sample_replicates(c(5, 5, 7, 10, 12, 14, 12)),
          Peptide_LFQ_mpwR = sample_replicates(c(5, 5, 7, 10, 12, 14, 12))
        )
      )
    )
  )

 for (i in seq_len(length(ordered_files))) {

   if (ordered_files[[i]][["software"]] == "DIA-NN") {
     ordered_files[[i]][["data"]][["DIA-NN"]] <- prepare_input(ordered_files[[i]][["data"]][["DIA-NN"]], software = "DIA-NN")
     next
   } else if (ordered_files[[i]][["software"]] == "Spectronaut") {
     ordered_files[[i]][["data"]][["Spectronaut"]] <- prepare_input(ordered_files[[i]][["data"]][["Spectronaut"]], software = "Spectronaut")
     next
   } else if (ordered_files[[i]][["software"]] == "MaxQuant") {
     ordered_files[[i]][["data"]][["ev"]] <- prepare_input(ordered_files[[i]][["data"]][["ev"]], software = "MaxQuant", MaxQuant_addon = "evidence")
     ordered_files[[i]][["data"]][["pep"]] <- prepare_input(ordered_files[[i]][["data"]][["pep"]], software = "MaxQuant", MaxQuant_addon = "peptide")
     ordered_files[[i]][["data"]][["pg"]] <- prepare_input(ordered_files[[i]][["data"]][["pg"]], software = "MaxQuant", MaxQuant_addon = "proteingroup")
     next
   } else if (ordered_files[[i]][["software"]] == "PD") {
     ordered_files[[i]][["data"]][["psm"]] <- prepare_input(ordered_files[[i]][["data"]][["psm"]], software = "PD", PD_addon = "psm")
     ordered_files[[i]][["data"]][["pep"]] <- prepare_input(ordered_files[[i]][["data"]][["pep"]], software = "PD", PD_addon = "peptide")
     ordered_files[[i]][["data"]][["prot"]] <- prepare_input(ordered_files[[i]][["data"]][["prot"]], software = "PD", PD_addon = "protein")
     ordered_files[[i]][["data"]][["pg"]] <- prepare_input(ordered_files[[i]][["data"]][["pg"]], software = "PD", PD_addon = "proteingroup")
     next
   } else if (ordered_files[[i]][["software"]] == "Generic") {
     ordered_files[[i]][["data"]][["Generic"]] <- prepare_input(ordered_files[[i]][["data"]][["Generic"]], software = "Generic")
     next
   }
 }

 return(ordered_files)
}
