<!-- README.md is generated from README.Rmd. Please edit that file -->

# extractox <img src='man/figures/extractox.png' align='right' height="139px" alt='logo'  style="float:right; height:139px;">

<!-- badges: start -->

[![R-CMD-check](https://github.com/c1au6i0/extractox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/c1au6i0/extractox/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/extractox)](https://CRAN.R-project.org/package=extractox)
[![DEV
version](https://img.shields.io/badge/devel%20version-1.1.0-blue.svg)](https://github.com/c1au6i0/extractox)
<!-- badges: end -->

`extractox` is a comprehensive R package designed to simplify querying
various chemical, toxicological, and biological databases:

-   the Integrated Chemical Environment (ICE) of the National Toxicology
    Program (NTP).
-   the Integrated Risk Information System (IRIS) of the Environmental
    Protection Agency (EPA).
-   the Provisional Peer-Reviewed Toxicity Values (PPRTVs) of EPA.
-   the Computational Toxicology Chemicals Dashboard Resource Hub
    (CompTox) of EPA,
-   the monographs of International Agency for Research on Cancer of the
    World Health Organization (WHO).
-   PubChem of the National Center for Biotechnology
    Information/National Institutes Of Health (NCBI, NIH).
-   the Comparative Toxicogenomics Database (CTP) of the MDI Biological
    Laboratory and NC State University.

The package facilitates interaction with APIs, providing curated and
user-friendly outputs. To communicate with Pubchem, `extractox` relies
on the package`webchem`.

## Installation

Install the package `extractox` from CRAN.

    # From CRAN
    install.packages("extractox")

### Development Version

To get a bug fix or to use a feature from the development version, you
can install the development version of `extractox` from GitHub.

    # Install pak if not already installed
    if (!requireNamespace("pak", quietly = TRUE)) {
      install.packages("pak")
    }

    # Install the package from GitHub
    pak::pkg_install("c1au6i0/extractox")

## Features

### NTP’s ICE

The ICE database provides access to a variety of data related to
chemical toxicity, exposure, and risk assessment. It includes data from
high-throughput screening assays, in vivo studies, and computational
models.

`extr_ice` provides access to NTP’s ICE database for toxicological and
exposure-related data.

    library(extractox)
    # assays is null so all assays are retrieved
    ice_data <- extr_ice(casrn = c("50-00-0"), assays = NULL, verbose = FALSE)
    names(ice_data)
    #>  [1] "assay"                 "endpoint"              "substance_type"       
    #>  [4] "casrn"                 "qsar_ready_id"         "value"                
    #>  [7] "unit"                  "species"               "receptor_species"     
    #> [10] "route"                 "sex"                   "strain"               
    #> [13] "life_stage"            "tissue"                "lesion"               
    #> [16] "location"              "assay_source"          "in_vitro_assay_format"
    #> [19] "reference"             "reference_url"         "dtxsid"               
    #> [22] "substance_name"        "pubmed_id"             "query"

There are more than **2000 possible assays** in ICE. The
`extr_ice_assay_names()` function allows to search for assay names that
match a pattern you’re interested in. Please note that searches are case
sensitive and accept regexp.

    extr_ice_assay_names("Rat Acute", verbose = FALSE) # keep empty to retrieve all
    #> [1] "Rat Acute Oral Toxicity"         "Rat Acute Inhalation Toxicity"  
    #> [3] "Rat Acute Dermal Toxicity"       "CATMoS, Rat Acute Oral Toxicity"

### EPA’s IRIS

The IRIS database contains information on the health effects of exposure
to various substances found in the environment. It provides qualitative
and quantitative health risk information.

`extr_iris` provides access to EPA’s IRIS database and accepts queries
CASRN or IUPAC names of chemicals.

    iris_info <- extr_iris(c("glyphosate", "50-00-0"), verbose = FALSE)
    names(iris_info)
    #> [1] "chemical_name"                 "casrn"                        
    #> [3] "exposure_route"                "assessment_type"              
    #> [5] "critical_effect_or_tumor_type" "woe_characterization"         
    #> [7] "toxicity_value_type"           "toxicity_value"               
    #> [9] "query"

### EPA’s PPRTVs

The `extr_pprtv` function allows you to extract data for specified
identifiers (CASRN or chemical names) from the EPA’s PPRTVs database.
This function retrieves the file containing all the chemicals and
processes it, but if has an argument (`force`) to allow users to use a
cached file or force a fresh download.

    # Example usage to extract data for a specific CASRN
    dat_pprtv_1 <- extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = FALSE)

    # Example usage to extract data for a chemical name
    dat_pprtv_2 <- extr_pprtv(ids = "Acrolein", search_type = "name", verbose = FALSE)

### EPA’s CompTox

The CompTox Chemistry Dashboard provides access to data on chemical
structures, properties, and associated bioactivity data. It
integratesdata from various sources to support chemical safety
assessments.

`extr_comptox` extracts data from CompTox using either CASRN or IUPAC
names of chemicals and returns a **list of dataframes**.

    info_comptox <- extr_comptox(ids = c("Aspirin", "50-00-0"), verbose = FALSE)
    names(info_comptox)
    #> [1] "comptox_cover_sheet"           "comptox_main_data"            
    #> [3] "comptox_abstract_sifter"       "comptox_synonym_identifier"   
    #> [5] "comptox_related_relationships" "comptox_toxcast_assays_ac50"  
    #> [7] "comptox_toxval_details"        "comptox_chemical_properties"

### WHO’s IARC

The IARC Monographs database contains evaluations of the carcinogenic
risks of various substances to humans. It provides detailed information
about Monographs, including publication volumes and years, evaluation
years, and additional relevant details.

The function `extr_monograph` provides access to the WHO IARC Monographs
database and accepts queries using CASRN or the names of chemicals.

    dat <- extr_monograph(
      search_type = "casrn",
      ids = c("105-74-8", "120-58-1"),
      verbose = FALSE
    )
    str(dat)
    #> 'data.frame':    2 obs. of  8 variables:
    #>  $ casrn                  : chr  "105-74-8" "120-58-1"
    #>  $ agent                  : chr  "Lauroyl peroxide" "Isosafrole"
    #>  $ group                  : chr  "3" "3"
    #>  $ volume                 : chr  "36, Sup 7, 71" "10, Sup 7"
    #>  $ volume_publication_year: chr  "1999" "1987"
    #>  $ evaluation_year        : int  1998 1987
    #>  $ additional_information : chr  "" ""
    #>  $ query                  : chr  "105-74-8" "120-58-1"

    # Example usage for name search
    dat2 <- extr_monograph(
      search_type = "name",
      ids = c("Aloe", "Schistosoma", "Styrene")
    )
    #> ℹ Extracting WHO IARC monographs...
    #> Last updated: 2024-11-29 5:08pm (CET)

### NIH’s PubChem

PubChem provides information on chemical structures, identifiers,
chemical and physical properties, biological activities, safety and
toxicity information, patents, literature citations, and more.

A series of functions that rely on the `webchem` package are used to
extract chemical information, Globally Harmonized System (`GHS`)
classification data, or flavor classification (`FEMA`) from PubChem.

The function `extr_chem_info` retrieves chemical information of
IUPAC-named chemicals. A warning is displayed if the chemical is not
found.

    chem_info <- extr_chem_info(
      iupac_names = c("Formaldehyde", "Aflatoxin B1"),
      verbose = FALSE
    )
    names(chem_info)
    #>  [1] "cid"                         "iupac_name"                 
    #>  [3] "casrn"                       "cid_all"                    
    #>  [5] "casrn_all"                   "molecular_formula"          
    #>  [7] "molecular_weight"            "smiles"                     
    #>  [9] "connectivity_smiles"         "inchi"                      
    #> [11] "inchi_key"                   "iupac_name_2"               
    #> [13] "x_log_p"                     "exact_mass"                 
    #> [15] "monoisotopic_mass"           "tpsa"                       
    #> [17] "complexity"                  "charge"                     
    #> [19] "h_bond_donor_count"          "h_bond_acceptor_count"      
    #> [21] "rotatable_bond_count"        "heavy_atom_count"           
    #> [23] "isotope_atom_count"          "atom_stereo_count"          
    #> [25] "defined_atom_stereo_count"   "undefined_atom_stereo_count"
    #> [27] "bond_stereo_count"           "defined_bond_stereo_count"  
    #> [29] "undefined_bond_stereo_count" "covalent_unit_count"        
    #> [31] "volume3d"                    "x_steric_quadrupole3d"      
    #> [33] "y_steric_quadrupole3d"       "z_steric_quadrupole3d"      
    #> [35] "feature_count3d"             "feature_acceptor_count3d"   
    #> [37] "feature_donor_count3d"       "feature_anion_count3d"      
    #> [39] "feature_cation_count3d"      "feature_ring_count3d"       
    #> [41] "feature_hydrophobe_count3d"  "conformer_model_rmsd3d"     
    #> [43] "effective_rotor_count3d"     "conformer_count3d"          
    #> [45] "fingerprint2d"               "title"                      
    #> [47] "patent_count"                "patent_family_count"        
    #> [49] "literature_count"            "annotation_types"           
    #> [51] "annotation_type_count"       "source_categories"          
    #> [53] "query"

Two functions are used to extract specific sections of PubChem chemical
information using CASRN:

-   `extr_pubchem_ghs` extracts GHS codes.
-   `extr_pubchem_fema` extracts FEMA data.

<!-- -->

    ghs_info <- extr_pubchem_ghs(casrn = c("50-00-0", "64-17-5"), verbose = FALSE)
    fema_info <- extr_pubchem_fema(casrn = c("50-00-0", "123-68-2"), verbose = FALSE)

### MDI’s CTD

The CTP provides information about the interactions between chemicals,
genes, and diseases. It helps in understanding the effects of
environmental exposures on human health.

A series of functions interact with the CTP database.

`extr_ctd` extracts information related to chemical-gene or pathway
associations.

    input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")
    ctd_association <- extr_ctd(
      input_terms = input_terms,
      category = "chem",
      report_type = "genes_curated",
      input_term_search_type = "directAssociations",
      action_types = "ANY",
      ontology = c("go_bp", "go_cc"),
      verbose = FALSE
    )

    names(ctd_association)
    #> [1] "chemical_name" "chemical_id"   "casrn"         "gene_symbol"  
    #> [5] "gene_id"       "organism"      "organism_id"   "pubmed_ids"   
    #> [9] "query"

    # Get expresssion data
    ctd_expression <- extr_ctd(
      input_terms = input_terms,
      report_type = "cgixns",
      category = "chem",
      action_types = "expression",
      verbose = FALSE
    )

    names(ctd_expression)
    #>  [1] "chemical_name" "chemical_id"   "casrn"         "gene_symbol"  
    #>  [5] "gene_id"       "organism"      "organism_id"   "pubmed_ids"   
    #>  [9] "query"         NA              NA

Tetramers are computationally generated information units that
interrelate four data types from the CTP: a chemical, gene product,
phenotype, and disease. They help in understanding the complex
relationships between these entities and their combined impact on human
health.

`extr_tetramer` extracts info related to tetramers from CTD.

    tetramer_data <- extr_tetramer(
      chem = c("50-00-0", "ethanol"),
      disease = "",
      gene = "",
      go = "",
      input_term_search_type = "directAssociations",
      qt_match_type = "equals",
      verbose = FALSE
    )

    names(tetramer_data)
    #>  [1] "chemical"                "chemical_id"            
    #>  [3] "gene"                    "gene_id"                
    #>  [5] "phenotype"               "phenotype_id"           
    #>  [7] "disease"                 "disease_id"             
    #>  [9] "evidence_strength_score" "query"

## Important Note regarding OpenSSL

Please note that functions that pull data from EPA servers may encounter
issues on systems. This is because these servers do not accept secure
legacy renegotiation. On Linux system, those functions depend on `curl`
and `OpenSSL`, which have known problems with unsafe legacy
renegotiation in newer versions. By using
[`condathis`](https://github.com/luciorq/condathis), the current package
installs a specific `OpenSSL` version in an isolated environment in
order to comunicate with the EPA servers.
