#' `SurvStat` disease list
#'
#' Supported diseases:
#' `r paste0(lapply(sort(names(diseases)), function(nm) sprintf("* %s (key: %s)", nm, diseases[[nm]])), collapse = "\n\n")`
#'
#' @name diseases
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @export
diseases = list(
  Acinetobacter = "Acinetobacter-Infektion oder \u2013Kolonisation",
  Adenovirus = "Adenovirus (andere Form, Meldepflichtig gem\u00e4\u00df Landesmeldeverordnung)",
  Mpox = "Affenpocken",
  Amoebiasis = "Amoebiasis",
  Arbovirus = "Arbovirus-Erkrankung",
  Astrovirus = "Astrovirus-Infektion",
  "Lyme Disease" = "Borreliose",
  Botulism = "Botulismus",
  Bornavirus = "Bornavirus",
  Brucellosis = "Brucellose",
  "Candida auris (invasive)" = "Candida auris, invasive Infektion",
  Campylobacter = "Campylobacter-Enteritis",
  Chickungunya = "Chikungunya-Fieber",
  "Chlamydia Trachomatis" = "Chlamydia-trachomatis-Infektion",
  Cholera = "Cholera",
  CJD = "CJK",
  "Clostridium difficile / mild" = "Clostridium difficile, nicht schwerer Verlauf",
  "Clostridium difficile / moderate" = "Clostridium difficile, schwerer Verlauf",
  "COVID-19" = "COVID-19",
  Cytomegalovirus = "Cytomegalie",
  Dengue = "Denguefieber",
  Diptheria = "Diphtherie",
  "E. Coli, enteritis" = "E.-coli-Enteritis",
  Ebola = "Ebolafieber",
  Echinococcosis = "Echinokokkose",
  "E. Coli, enterohemorrhagic" = "EHEC-Erkrankung",
  "Enterobacteria colonisation" = "Enterobacteriaceae-Infektion oder \u2013Kolonisation",
  Enterovirus = "Enterovirus",
  "Varicella, congenital" = "Fetales (kongenitales) Varizellensyndrom",
  Typhoid = "Fleckfieber",
  "Tick bourne encephalitis" = "FSME (Fr\u00fchsommer-Meningoenzephalitis)",
  "Gas gangrene" = "Gasbrand",
  "Yellow fever" = "Gelbfieber",
  Giardia = "Giardiasis",
  Gonorrhoea = "Gonorrhoe",
  "Group B Streptococcus" = "Gruppe-B-Streptokokken",
  "Haemophilus influenza, invasive" = "Haemophilus influenzae, invasive Erkrankung",
  "Hand foot mouth disease" = "Hand-Fu\u00df-Mund-Krankheit",
  Hantavirus = "Hantavirus-Erkrankung",
  "Hepatitis (general)" = "Hepatitis (allgemein)",
  "Hepatitis A" = "Hepatitis A",
  "Hepatitis B" = "Hepatitis B",
  "Hepatitis C" = "Hepatitis C",
  "Hepatitis D" = "Hepatitis D",
  "Hepatitis E" = "Hepatitis E",
  "Hepatitis non A-E" = "Hepatitis Non A-E",
  "Herpes Zoster" = "Herpes Zoster",
  HIV = "HIV-Infektion",
  "Haemolytic-uraemic syndrome" = "HUS (H\u00e4molytisch-ur\u00e4misches Syndrom), enteropathisch",
  "Influenza, seasonal" = "Influenza, saisonal",
  "Influenza, zoonotic" = "Influenza, zoonotisch",
  "Keratoconjunctivitis (IfSG)" = "Keratokunjunktivitis (Meldepflicht gem\u00e4\u00df IfSG)",
  "Keratoconjunctivitis (state)" = "Keratokunjunktivitis (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  "Whooping cough (IfSG)" = "Keuchhusten (Meldepflicht gem\u00e4\u00df IfSG)",
  "Whooping cough (state)" = "Keuchhusten (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  "Head lice" = "Kopflausbefall",
  Scabies = "Kr\u00e4tzmilbenbefall",
  Cryptosporidiosis = "Kryptosporidiose",
  "Lassa fever" = "Lassafieber",
  "Relapsing fever" = "L\u00e4user\u00fcckfallfieber",
  Legionalla = "Legionellose",
  Leprousy = "Lepra",
  Leptospirosis = "Leptospirose",
  Listeriosis = "Listeriose",
  "Marburg virus" = "Marburgfieber",
  Measles = "Masern",
  "Meningitis (other)" = "Meningitis, andere",
  "Meningococcal, invasive" = "Meningokokken, invasive Erkrankung",
  Anthrax = "Milzbrand",
  "MRSA, invasive" = "MRSA, invasive Infektion",
  "Mumps (IfSG)" = "Mumps (Meldepflicht gem\u00e4\u00df IfSG)",
  "Mumps (state)" = "Mumps (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  Mycoplasma = "Mycoplasma",
  Norovirus = "Norovirus-Gastroenteritis",
  Orthinovirus = "Ornithose",
  Parainfluenze = "Parainfluenza",
  Paratyphus = "Paratyphus",
  Plague = "Pest",
  "Pneumococcus (IfSG)" = "Pneumokokken (Meldepflicht gem\u00e4\u00df IfSG)",
  "Pneumococcus (state)" = "Pneumokokken (Meldepflicht gem\u00e4\u00df Landesverordnung)",
  Smallpox = "Pocken",
  Poliomyelitis = "Poliomyelitis",
  "Q-fever" = "Q-Fieber",
  Ringworm = "Ringelr\u00f6teln",
  "Rotavirus gastroenteritis" = "Rotavirus-Gastroenteritis",
  "Rubella (state)" = "R\u00f6teln (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  "Rubella, congenital" = "R\u00f6teln, konnatal",
  Rubella = "R\u00f6teln, postnatal",
  "RSV (IfSG)" = "RSV (Meldepflicht gem\u00e4\u00df IfSG)",
  "RSV (state)" = "RSV (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  Salmonellosis = "Salmonellose",
  SARS = "SARS",
  "Scarlet fever" = "Scharlach",
  Shigellosis = "Shigellose",
  Syphilis = "Syphilis",
  Tetanus = "Tetanus",
  "Rabies (confirmed)" = "Tollwut",
  "Rabies (suspected)" = "Tollwutexpositionsverdacht",
  Toxoplasmosis = "Toxoplasmose",
  "Toxoplasmosis, congenital" = "Toxoplasmose, konnatal",
  Trichinellosis = "Trichinellose",
  Tuberculosis = "Tuberkulose",
  Tulareamia = "Tular\u00e4mie",
  "Typhoid, abdominal" = "Typhus abdominalis",
  "CJD, variant" = "vCJK",
  "Viral haemmorhagic fever" = "Virale h\u00e4morrhagische Fieber",
  "Sepsis (other)" = "Weitere bedrohliche Krankheit",
  "Gastroenteritis (other)" = "Weitere bedrohliche Krankheit (gastro)",
  Chickenpox = "Windpocken",
  "Chickenpox (state)" = "Windpocken (Meldepflicht gem\u00e4\u00df Landesmeldeverordnung)",
  Yersinia = "Yersiniose",
  Zika = "Zikavirus-Erkrankung",
  "Malaria (IfSG)" = "Malaria (\u00a77(3) IfSG)",
  "Malaria (state)" = "Malaria, L\u00e4nderverordnung",
  "MERS" = "Middle East Respiratory Syndrome",
  "Mpox" = "Mpox",
  "Orthopox" = "Orthopocken",
  "Subacute Sclerosing Panencephalitis" = "Subakute Sklerosierende Panenzephalitis",
  "Typhus/Paratyphus" = "Typhus/Paratyphus",
  "Vibria" = "Vibrionen",
  "West Nile Virus" = "West-Nil-Virus"
)


# stringi::stri_escape_unicode(c(
#   "Bornavirus",
#   "Candida auris, invasive Infektion",
#   "Hepatitis (allgemein)",
#   "Malaria (§7(3) IfSG)",
#   "Malaria, Länderverordnung",
#   "Middle East Respiratory Syndrome",
#   "Mpox",
#   "Orthopocken",
#   "Pneumokokken (Meldepflicht gemäß Landesverordnung)",
#   "Subakute Sklerosierende Panenzephalitis",
#   "Typhus/Paratyphus",
#   "Vibrionen",
#   "West-Nil-Virus"
# )) %>%
#   clipr::write_clip()

#   list(
#   `Acinetobacter` = "Acinetobacter-Infektion oder –Kolonisation",
#   `Adenovirus` = "Adenovirus (andere Form, Meldepflichtig gemäß Landesmeldeverordnung)",
#   `Mpox` = "Affenpocken",
#   `Amoebiasis` = "Amoebiasis",
#   `Arbovirus` = "Arbovirus-Erkrankung",
#   `Astrovirus` = "Astrovirus-Infektion",
#   `Lyme Disease` = "Borreliose",
#   `Botulism` = "Botulismus",
#   `Brucellosis` = "Brucellose",
#   `Campylobacter` = "Campylobacter-Enteritis",
#   `Chickungunya` = "Chikungunya-Fieber",
#   `Chlamydia Trachomatis` = "Chlamydia-trachomatis-Infektion",
#   `Cholera` = "Cholera",
#   `CJD` = "CJK",
#   `Clostridium difficile / mild` = "Clostridium difficile, nicht schwerer Verlauf",
#   `Clostridium difficile / moderate` = "Clostridium difficile, schwerer Verlauf",
#   `COVID-19` = "COVID-19",
#   `Cytomegalovirus` = "Cytomegalie",
#   `Dengue` = "Denguefieber",
#   `Diptheria` = "Diphtherie",
#   `E. Coli, enteritis` = "E.-coli-Enteritis",
#   `Ebola` = "Ebolafieber",
#   `Echinococcosis` = "Echinokokkose",
#   `E. Coli, enterohemorrhagic` = "EHEC-Erkrankung",
#   `Enterobacteria colonisation` = "Enterobacteriaceae-Infektion oder –Kolonisation",
#   `Enterovirus` = "Enterovirus",
#   `Varicella, congenital` = "Fetales (kongenitales) Varizellensyndrom",
#   `Typhoid` = "Fleckfieber",
#   `Tick bourne encephalitis` = "FSME (Frühsommer-Meningoenzephalitis)",
#   `Gas gangrene` = "Gasbrand",
#   `Yellow fever` = "Gelbfieber",
#   `Giardia` = "Giardiasis",
#   `Gonorrhoea` = "Gonorrhoe",
#   `Group B Streptococcus` = "Gruppe-B-Streptokokken",
#   `Haemophilus influenza, invasive` = "Haemophilus influenzae, invasive Erkrankung",
#   `Hand foot mouth disease` = "Hand-Fuß-Mund-Krankheit",
#   `Hantavirus` = "Hantavirus-Erkrankung",
#   `Hepatitis A` = "Hepatitis A",
#   `Hepatitis B` = "Hepatitis B",
#   `Hepatitis C` = "Hepatitis C",
#   `Hepatitis D` = "Hepatitis D",
#   `Hepatitis E` = "Hepatitis E",
#   `Hepatitis non A-E` = "Hepatitis Non A-E",
#   `Herpes Zoster`= "Herpes Zoster",
#   `HIV` = "HIV-Infektion",
#   `Haemolytic-uraemic syndrome` = "HUS (Hämolytisch-urämisches Syndrom), enteropathisch",
#   `Influenza, seasonal` = "Influenza, saisonal",
#   `Influenza, zoonotic` = "Influenza, zoonotisch",
#   `Keratoconjunctivitis (IfSG)` = "Keratokunjunktivitis (Meldepflicht gemäß IfSG)",
#   `Keratoconjunctivitis (state)` = "Keratokunjunktivitis (Meldepflicht gemäß Landesmeldeverordnung)",
#   `Whooping cough (IfSG)` = "Keuchhusten (Meldepflicht gemäß IfSG)",
#   `Whooping cough (state)` = "Keuchhusten (Meldepflicht gemäß Landesmeldeverordnung)",
#   `Head lice` = "Kopflausbefall",
#   `Scabies` = "Krätzmilbenbefall",
#   `Cryptosporidiosis` = "Kryptosporidiose",
#   `Lassa fever` = "Lassafieber",
#   `Relapsing fever` = "Läuserückfallfieber",
#   `Legionalla` = "Legionellose",
#   `Leprousy` = "Lepra",
#   `Leptospirosis` = "Leptospirose",
#   `Listeriosis` = "Listeriose",
#   `Malaria` = "Malaria",
#   `Marburg virus` = "Marburgfieber",
#   `Measles` = "Masern",
#   `Meningitis (other)` = "Meningitis, andere",
#   `Meningococcal, invasive` = "Meningokokken, invasive Erkrankung",
#   `Anthrax` = "Milzbrand",
#   `MRSA, invasive` = "MRSA, invasive Infektion",
#   `Mumps (IfSG)` = "Mumps (Meldepflicht gemäß IfSG)",
#   `Mumps (state)` = "Mumps (Meldepflicht gemäß Landesmeldeverordnung)",
#   `Mycoplasma` = "Mycoplasma",
#   `Norovirus` = "Norovirus-Gastroenteritis",
#   `Orthinovirus` = "Ornithose",
#   `Parainfluenze` = "Parainfluenza",
#   `Paratyphus` = "Paratyphus",
#   `Plague` = "Pest",
#   `Pneumococcus, invasive` = "Pneumokokken, invasive Erkrankung",
#   `Smallpox` = "Pocken",
#   `Poliomyelitis` = "Poliomyelitis",
#   `Q-fever` = "Q-Fieber",
#   `Ringworm` = "Ringelröteln",
#   `Rotavirus gastroenteritis` = "Rotavirus-Gastroenteritis",
#   `Rubella (state)` = "Röteln (Meldepflicht gemäß Landesmeldeverordnung)",
#   `Rubella, congenital` = "Röteln, konnatal",
#   `Rubella` = "Röteln, postnatal",
#   `RSV` = "RSV-Infektion",
#   `Salmonellosis` = "Salmonellose",
#   `SARS` = "SARS",
#   `Scarlet fever` = "Scharlach",
#   `Shigellosis` = "Shigellose",
#   `Syphilis` = "Syphilis",
#   `Tetanus` = "Tetanus",
#   `Rabies (confirmed)` = "Tollwut",
#   `Rabies (suspected)` = "Tollwutexpositionsverdacht",
#   `Toxoplasmosis` = "Toxoplasmose",
#   `Toxoplasmosis, congenital` = "Toxoplasmose, konnatal",
#   `Trichinellosis`="Trichinellose",
#   `Tuberculosis` = "Tuberkulose",
#   `Tulareamia` = "Tularämie",
#   `Typhoid, abdominal` = "Typhus abdominalis",
#   `CJD, variant` = "vCJK",
#   `Viral haemmorhagic fever` = "Virale hämorrhagische Fieber",
#   `Sepsis (other)` = "Weitere bedrohliche Krankheit",
#   `Gastroenteritis (other)` = "Weitere bedrohliche Krankheit (gastro)",
#   `Chickenpox` = "Windpocken",
#   `Chickenpox (state)` = "Windpocken (Meldepflicht gemäß Landesmeldeverordnung)",
#   `Yersinia` = "Yersiniose",
#   `Zika` = "Zikavirus-Erkrankung"
# )

#' `SurvStat` age group list
#'
#' * single_year
#' * children_coarse: from 0, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
#' * children_medium: from 0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
#' * children_fine: from 0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
#' * five_year: from 0, 1, 5, 10, 15, 20, … , 75, 80 years
#' * zero_fifteen: from 0, 15+ years
#' * zero_fifteen_sixty: from 0, 15, 60+ years
#' * zero_one_4_20_40_60_80: from 0, 4, 20, 40, 60, 80+ years
#'
#' @name age_groups
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @export
age_groups = list(
  single_year = "[AlterPerson80].[AgeGroupName8]",
  children_coarse = "[AlterPerson80].[AgeGroupName3]", #from 0, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  children_medium = "[AlterPerson80].[AgeGroupName2]", #from 0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  children_fine = "[AlterPerson80].[AgeGroupName1]", #from 0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  five_year = "[AlterPerson80].[AgeGroupName6]", #from 0, 1, 5, 10, 15, 20, … , 75, 80 years
  zero_fifteen = "[AlterPerson80].[AgeGroupName4]", #from 0, 15+
  zero_fifteen_sixty = "[AlterPerson80].[AgeGroupName5]", #from 0, 15, 60+
  zero_one_4_20_40_60_80 = "[AlterPerson80].[AgeGroupName7]" #from 0, 15, 60+
)


#' Data sources in the `SurvStat` service
#'
#' @name cubes
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @keywords internal
cubes = list(
  survstat = "SurvStat",
  survstat73 = "SurvStat73",
  evstat = "EvStat"
)

#' Languages supported by the `SurvStat` service
#'
#' @name languages
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @keywords internal
languages = list(
  german = "German",
  english = "English"
)

#' Commands supported by the `SurvStat` service
#'
#' @name commands
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @keywords internal
commands = list(
  dimensions = "GetAllDimensions",
  hierarchies = "GetAllHierarchies",
  existing_members = "GetAllHierarchyExistsMembers",
  members = "GetAllHierarchyMembers",
  measures = "GetAllMeasures",
  cube_info = "GetCubeInfo",
  olap_data = "GetOlapData",
  olap_result_data = "GetOlapResultData"
)

#' Return measures supported by the `SurvStat` service
#'
#' Not all services support all 3 methods.
#'
#' @name commands
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
#' @keywords internal
return_measures = list(
  count = "Count",
  incidence = "Incidence",
  population = "Population"
)

#' Geographical resolution options
#'
#' The 3 different resolution levels of the geospatial data
#'
#' @name commands
#' @docType data
#' @references \url{https://survstat.rki.de/Content/Query/Create.aspx}
#' @concept data
geography_resolution = list(
  state = "[DeutschlandNodes].[Kreise71Web].[FedStateKey71]",
  nuts = "[DeutschlandNodes].[Kreise71Web].[NutsKey71]",
  county = "[DeutschlandNodes].[Kreise71Web].[CountyKey71]"
)

## BerlinMap definition ----

#' A Berlin outline `sf` map
#'
#' @usage data(BerlinMap)
#'
#' @format
#' A `sf` dataframe containing the following columns:
#'
#' * Name (character) - the Name column
#'
#' 1 rows
#'
#' @docType data
#' @concept data
#' @name BerlinMap
NULL

## BerlinMap definition ends

## FedStateKey71Map definition ----

#' The `FedStateKey71Map` dataset.
#'
#' This matches the `FedStateKey71` dimension in `SurvStat`. This is the 16
#' federal states in Germany.
#'
#' @usage data(FedStateKey71Map)
#'
#' @format
#' A `sf` dataframe containing the following columns:
#'
#' * `Id` - the full `SurvStat` identifier for this region (includes
#'   hierarchical information)
#' * `ComponentId` - the id of the most granular geographical unit (which can be
#'   used to link out to other data sets)
#' * `HierarchyId` - the id of the geographical unit type
#' * `Name` - the name of the region
#'
#' 16 rows
#'
#' @docType data
#' @concept data
#' @name FedStateKey71Map
NULL

## FedStateKey71Map definition ends
## NutsKey71Map definition ----

#' The `NutsKey71Map` dataset
#'
#' This matches the `NutsKey71` dimension in `SurvStat`. This is the 38 `NUTS2`
#' level administrative regions in Germany.
#'
#' @usage data(NutsKey71Map)
#'
#' @format
#' A `sf` dataframe containing the following columns:
#'
#' * `Id` - the full `SurvStat` identifier for this region (includes
#'   hierarchical information)
#' * `ComponentId` - the id of the most granular geographical unit (which can be
#'   used to link out to other data sets)
#' * `HierarchyId` - the id of the geographical unit type
#' * `Name` - the name of the region
#'
#' 38 rows
#'
#' @docType data
#' @concept data
#' @name NutsKey71Map
NULL

## NutsKey71Map definition ends
## CountyKey71Map definition ----

#' The `CountyKey71Map` dataset
#'
#' This matches the `CountyKey71` dimension in `SurvStat`. This is the 400
#' `Stadtkreis` and `Landkreise` administrative regions in Germany, plus 12
#' Berlin boroughs (`Bezirke`) which replace the Berlin `Kriese` (Id: `11000`).
#' The boroughs have sequential `Id`s from `[11001]` to `[11012]`
#'
#' @usage data(CountyKey71Map)
#'
#' @format
#' A `sf` dataframe containing the following columns:
#'
#' * `Id` - the full `SurvStat` identifier for this region (includes
#'   hierarchical information)
#' * `ComponentId` - the id of the most granular geographical unit (which can be
#'   used to link out to other data sets)
#' * `HierarchyId` - the id of the geographical unit type
#' * `Name` - the name of the region
#'
#' Any grouping allowed.
#'
#' 411 rows
#'
#' @docType data
#' @concept data
#' @name CountyKey71Map
NULL

## CountyKey71Map definition ends
