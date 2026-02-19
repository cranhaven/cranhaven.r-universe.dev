# Written by Rob Young at the University of Guelph in Ontario Canada, May, 2024
# ********************************************Main program section*************
# DBTC Package
#' @import DBTC
#'
# Shiny Packages
#' @import leaflet
#'
# Shiny Functions
#' @importFrom magrittr %>%
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny div
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny h4
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom shiny modalDialog
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny p
#' @importFrom shiny radioButtons
#' @importFrom shiny removeModal
#' @importFrom shiny renderText
#' @importFrom shiny reactiveValues
#' @importFrom shiny shinyApp
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny strong
#' @importFrom shiny tabPanel
#' @importFrom shiny tags
#' @importFrom shiny textInput
#' @importFrom shiny textOutput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny wellPanel

#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabBox
#' @importFrom shinydashboard box
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles parseFilePaths
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom shinycssloaders withSpinner
#' @importFrom leaflet.extras addFullscreenControl

#' @importFrom DT dataTableOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput

#' @importFrom stats reshape
#' @importFrom utils read.delim
#' @importFrom utils read.table

#################### Dash Board Body ########################################
dashBoardBodyComponent <- function() {

  shinydashboard::dashboardBody(

    #Style tag for website name, located in left top corner of page
    shiny::tags$head(shiny::tags$style(shiny::HTML('
      .main-header .logo {
        font-family: Verdana, Geneva, sans-serif;
        font-size: 24px;
      }
    '))),

    shinydashboard::tabItems(

      #Welcome Dashboard
      welcomePage(),

      #DBTC Dashboard
      dbtcTools(),

      #Mapping Dashboard
      mappingDashboard()

    )
  )
}

################### Welcome Page Tab ##########################################

welcomePage <- function() {
  shinydashboard::tabItem(tabName = "welcomePage",
                          shiny::fluidRow(shinydashboard::box(
                            title = shiny::p("Welcome to DBTCShiny", style = "font-size:24px;"),
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            width = 12,
                            shiny::p("Dada-BLAST-Taxon Assign-Condense (DBTC) is an R shiny implementation of a Dada2 based Metabarcode analysis pipeline. This implementation includes: ", style = "font-size:24px;"),
                            shiny::tags$div(
                              shiny::tags$ul(
                                shiny::tags$li(shiny::a("Fastq", href = "https://en.wikipedia.org/wiki/FASTQ_format", target="_blank")," file processing using Dada in R"),
                                shiny::tags$li("Using the Basic Local Alignment Search Tool (",shiny::a("BLAST", href = "https://en.wikipedia.org/wiki/BLAST_(biotechnology)", target="_blank"),") amplicon sequence variants ", shiny::a("ASV", href = "https://en.wikipedia.org/wiki/Amplicon_sequence_variant", target="_blank"), " can be searched against local NCBI or custom sequence databases"),
                                shiny::tags$li("Assign taxa to the unique reads using NCBI taxon database (obtain the database using ",shiny::a("taxonomizr website", href = "https://cran.r-project.org/web/packages/taxonomizr/vignettes/usage.html", target="_blank")),
                                shiny::tags$li("Condense the resulting ", shiny::a("ASV", href = "https://en.wikipedia.org/wiki/Amplicon_sequence_variant", target="_blank"), " taxonomic assignment tables to unique taxa with the ability to combine datasets (using different sequence sequence databases for the same reads, or results from the same samples for different molecular regions) into a combined results table"),
                                shiny::br(),
                                shiny::p("NOTE: While the DBTCShiny package has been built for the analysis of high-throughput sequencing results, the BLAST and taxonomic assignment, taxonomic condense can be utilized with single specimen Sanger sequencing data."),
                              )
                            )
                          )),
                          shiny::fluidRow(shinydashboard::box(
                            title = shiny::p("DBTCShiny Components", style = "font-size:24px;"),
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = 12,
                            shiny::br(),
                            shiny::h4("R Packages"),
                            shiny::p(shiny::a("DBTC", href = "https://github.com/rgyoung6/DBTC", target="_blank")),
                            shiny::p(shiny::a("DBTCShiny", href = "https://github.com/rgyoung6/DBTCShiny", target="_blank")),
                            shiny::br(),
                            shiny::h4("Required elements outside of R"),
                            shiny::p("NCBI ", shiny::a("BLAST+", href = "https://blast.ncbi.nlm.nih.gov/doc/blast-help/downloadblastdata.html#blast-executables", target="_blank"), " local program to run BLAST on local databases"),
                            shiny::tags$div(
                              shiny::tags$ul(
                                shiny::tags$li("Follow the instructions on the NCBI ", shiny::a("BLAST+ executables", href = "https://blast.ncbi.nlm.nih.gov/doc/blast-help/downloadblastdata.html#blast-executables", target="_blank"), " page to install a local version of the BLAST tools."),
                              )
                            ),
                            shiny::br(),
                            shiny::h4("NCBI taxonomic database"),
                            shiny::p("The R package ",  shiny::a("taxonomizr", href = "https://cran.r-project.org/web/packages/taxonomizr/vignettes/usage.html", target="_blank"), " is used to establish a NCBI taxaID database (NOTE: this package is also required when using the taxon assignment elements in the DBTC pipeline)"),
                            shiny::tags$div(
                              shiny::tags$ul(
                                shiny::tags$li("In the 'Preparation' section of the taxonomizr website, use the instructions and the prepareDatabase('accessionTaxa.sql') taxonomizr command establish a local taxonomic database."),
                              )
                            ),
                            shiny::br(),
                            shiny::h4("Sequence Database"),
                            shiny::p("NCBI preformated databases can be established through two methods."),
                            shiny::tags$div(
                              shiny::tags$ul(
                                shiny::tags$li("Download your desired preformatted NCBI database by using the 'update_blastdb.pl' (found in the NCBI BLAST+ local install folder). NOTE: Perl programming langugage needs to be installed on your local machine. Instructions can be found at ", shiny::a("Get NCBI BLAST databases", href = "https://www.ncbi.nlm.nih.gov/books/NBK569850/", target="_blank")),
                                shiny::tags$li("You can download your desired preformatted NCBI database manually at ", shiny::a("NCBI BLAST databases", href = "https://ftp.ncbi.nlm.nih.gov/blast/db/"))
                              )
                            ),
                            shiny::br(),
                            shiny::p("In addition to the NCBI resources, DBTC can also use custom databases. To establish these databases you will requre a fasta file with the desired records with MACER formatted headers. The MACER R package and instructions can be found at either of the two locations:"),
                            shiny::tags$div(
                              shiny::tags$ul(
                                shiny::tags$li(shiny::a("MACER CRAN", href = "https://cran.r-project.org/web/packages/MACER")),
                                shiny::tags$li(shiny::a("MACER GitHub", href = "https://github.com/rgyoung6/MACER"), " (will have the most recent version)")
                              )
                            )
                          )),
                          shiny::fluidRow(shinydashboard::box(
                            title = shiny::p("Contact", style = "font-size:24px;"),
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = 12,
                            shiny::h4("Questions, concerns, suggestions about this pipeline can be sent to rgyoung6[at]gmail[dot]com.")
                          ))
  )
}
################### DBTC Tools Tab ##########################################

dbtcTools <- function() {
  shinydashboard::tabItem(tabName = "dbtcTools",shiny::h1(shiny::strong("DBTC Tools")),
    shinydashboard::tabBox(id = "DBTC_Tools_Box",width = 12,
       shiny::tabPanel("dada_implement()",shiny::div(style = 'overflow-y:scroll;height:400px;',
             shiny::column(width = 4,
                           shiny::p(shiny::tags$b(shiny::tags$u("1. General Information", style = "font-size:14px;"))),
                           shiny::wellPanel(
                             shiny::fluidRow(
                               shiny::strong("Select a fastq file in one of the run folders in the directory of interest (NOTE: all run folders with fastq data in the parent directory will be processed by DBTC. If this is not what you want please rearrange your folder structure. See ", shiny::a("DBTC dada_implement()", href = "https://github.com/rgyoung6/DBTC/blob/main/README.md#input")," for details)."),
                               shiny::br(),
                               #Data file upload
                               shinyFiles::shinyFilesButton("dadaDirectory", "Fastq File",title = "Fastq File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                               shiny::br(),
                               shiny::br(),
                               shiny::column(5,shiny::textOutput("dadaDirectoryDisplay"))
                             ),
                             shiny::br(),
                             shiny::fluidRow(
                               shiny::strong("Select a file with the primers for this analysis (See ", shiny::a("DBTC dada_implement()", href = "https://github.com/rgyoung6/DBTC/blob/main/README.md#input")," for details)."),
                               shiny::br(),
                               #Data file upload
                               shinyFiles::shinyFilesButton("primerFile", "Primer File",title = "Primer File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                               shiny::br(),
                               shiny::br(),
                               shiny::column(5,shiny::textOutput("primerFileDisplay"))
                             ),
                             shiny::br(),
                             #General Processing values
                             shiny::radioButtons("uniOrbidirectional", "Directional processing: Process the samples unidirectionally, bidirectionally, or both (Note: If bidirectinoal is selected the function will not run if only unidirectional data is present).", c("Bidirectional", "Unidirectional", "Both")),
                             shiny::conditionalPanel(
                               condition = "input.uniOrbidirectional != 'Unidirectional'",
                               shiny::textInput("fwdIdent", "Foward identifier naming substring", value = "_R1_001"),
                               shiny::textInput("revIdent", "Reverse identifier naming substring", value = "_R2_001")
                             ),
                             shiny::radioButtons("printQualityPdf", "Print quality plots to pdf:", c("TRUE", "FALSE"))
                           ),
                           shiny::p(shiny::tags$b(shiny::tags$u("2. Pattern Trim", style = "font-size:14px;"))),
                           shiny::wellPanel(
                             #Pattern trim
                             shiny::numericInput("maxPrimeMis", "Maximum number of mismatches allowed when pattern matching trimming the primers from the ends of the reads for the ShortRead trimLRPatterns() function (Default maxPrimeMis = 2)", value = 2, min = 0, max = 50),
                           ),
             ),shiny::column(width = 4,
                             shiny::p(shiny::tags$b(shiny::tags$u("3. Dada filterAndTrim", style = "font-size:14px;"))),
                             shiny::wellPanel(
                               #Dada filterAndTrim values
                               shiny::numericInput("fwdTrimLen", "Select a forward trim length for the Dada filterAndTrim() function (Default fwdTrimLen = 0).", value = 0, min = 0, max = 50),
                               shiny::numericInput("revTrimLen", "Select a reverse trim length for the Dada filterAndTrim() function (Default revTrimLen = 0).", value = 0, min = 0, max = 50),
                               shiny::numericInput("maxEEVal", "Maximum number of expected errors allowed in a read for the Dada filterAndTrim() function (Default maxEEVal = 2)", value = 2, min = 0, max = 250),
                               shiny::numericInput("truncQValue", "Truncation value use to trim ends of reads, nucleotides with quality values less than this value will be used to trim the remainder of the read for the Dada filterAndTrim() function (Default truncQValue = 2).", value = 2, min = 0, max = 42),
                               shiny::numericInput("truncLenValueF", "Dada forward length trim value for the Dada filterAndTrim() function. This function is set to 0 when the pattern matching trim function is enabled (Default truncLenValueF = 0).", value = 0, min = 0, max = 250),
                               shiny::numericInput("truncLenValueR", "Dada reverse length trim value for the Dada filterAndTrim() function. This function is set to 0 when the pattern matching trim function is enabled (Default truncLenValueR = 0).", value = 0, min = 0, max = 250),
                             ),
             ),shiny::column(width = 4,
                             shiny::p(shiny::tags$b(shiny::tags$u("4. Dada learnErrors", style = "font-size:14px;"))),
                             shiny::wellPanel(
                               #Dada error assessment for the fastq files
                               shiny::numericInput("error", "Percent of fastq files used to assess error rates for the Dada learnErrors() function (Default error = 0.1)", value = 0.1, min = 0, max = 1),
                               shiny::numericInput("nbases", "The total number of bases used to assess errors for the Dada learnErrors() function (Default nbases = 1e80) NOTE: this value is set very high to get all nucleotides in the error persent file subset. If the error is to be assessed using total reads and not specific fastq files then set the error to 1 and set this value to the desired number of reads.", value = 1000000000, min = 0, max = 1e80),
                             ),
                             shiny::p(shiny::tags$b(shiny::tags$u("5. Dada mergePairs", style = "font-size:14px;"))),
                             shiny::wellPanel(
                               #Dada mergePairs values
                               shiny::numericInput("maxMismatchValue", "Maximum number of mismatches allowed when merging two reads for the Dada mergePairs() function (Default maxMismatchValue = 2)", value = 2, min = 0, max = 50),
                               shiny::numericInput("minOverlapValue", "Minimum number of overlapping nucleotides for the forward and reverse reads for the Dada mergePairs() function (Default minOverlapValue = 12)", value = 12, min = 0, max = 50),
                               shiny::radioButtons("trimOverhang", "Trim merged reads past the start of the complimentary primer regions for the Dada mergePairs() function (Default trimOverhang = FALSE)", c("FALSE", "TRUE")),

                               #Final length filtering
                               shiny::numericInput("minFinalSeqLen", "The minimum final desired length of the read (Default minFinalSeqLen = 100).", value = 100, min = 0, max = 1000),

                               #Submit button to run the script
                               shiny::actionButton("dadaSubmit","Dada Submit", icon = shiny::icon("play-circle"))

                             ),
             )#Closing out the shiny columns
          )#Closing out the div style
       ),#Tab panel
       shiny::tabPanel("combine_dada_output()",
           shiny::wellPanel(
             shiny::fluidRow(
               shiny::strong("Select a file in the file folder with dada results
                             you would like to combine
                             (YYYY_MM_DD_HHMM_FileName_MergeFwdRev.tsv OR
                             YYYY_MM_DD_HHMM_FileName_Merge.tsv)"),
               shiny::br(),
               #Data file upload
               shinyFiles::shinyFilesButton("dadaCombineFile", "Select a File in the Target Folder",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
               shiny::br(),
               shiny::br(),
               shiny::column(5,shiny::textOutput("dadaCombineDisplay")),
               shiny::br()
             ),
             #Final length filtering
             shiny::numericInput("dadaCombineMinLen", "The minimum final desired length of the read (Default minLen = 100).", value = 100, min = 0, max = 1000),
             #Submit button to run the script
             shiny::actionButton("dadaCombine","Dada Combine Submit", icon = shiny::icon("play-circle"))
           )
       ),
       shiny::tabPanel("make_BLAST_DB()",
         shiny::wellPanel(
           shiny::p(shiny::tags$b(shiny::tags$u("Create a BLAST data base using fasta files (formatted correctly).", style = "font-size:16px;"))),

           #BLAST data base file location
           shiny::fluidRow(
             shiny::strong("Please select the fasta file you would like to use to construct the database."),
             shiny::br(),
             shinyFiles::shinyFilesButton("makeBlastDBFileLoc", "Fasta File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("makeBlastDBFileLocDisplay")),
             shiny::br()
           ),
           #Data file upload
           shiny::fluidRow(
             shiny::strong("Please select the location of the NCBI BLAST programs makeblastdb. If no file location is selected then the program will try to run in the local directory with the default 'makeblastdb'."),
             shiny::br(),
             shinyFiles::shinyFilesButton("makeblastdbPath", "makeblastdb Location",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("makeblastdbPathDisplay")),
             shiny::br()
           ),
           #Data file accessionTaxa.sql
           shiny::fluidRow(
             shiny::strong("Please select the NCBI accessionTaxa.sql data base file to use in constructing the custom database."),
             shiny::br(),
             shinyFiles::shinyFilesButton("makeBlastTaxaDBLoc", "Select the NCBI Taxon Database File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("makeBlastTaxaDBLocDisplay")),
             shiny::br()
           ),

           #Final length filtering
           shiny::numericInput("makeBLASTDBMinLen", "The minimum length of reads in the fasta file to be included in the BLAST database (Default minLen = 100).", value = 100, min = 0, max = 1000),
           shiny::textInput("dbName", "Provide a brief and simple database name with no special characters."),

           #Submit button to run the script
           shiny::actionButton("makeBlastDB","Create BLAST Database Submit", icon = shiny::icon("play-circle"))
         )

       ),
       shiny::tabPanel("seq_BLAST()",
         shiny::wellPanel(
           shiny::p(shiny::tags$b(shiny::tags$u("BLAST a fasta file against an established library.", style = "font-size:16px;"))),

           #Data base location
           shiny::fluidRow(
             shiny::strong("Select a file in the folder with the NCBI database you would like to use."),
             shiny::br(),
             shinyFiles::shinyFilesButton("BLASTDatabasePath", "Database File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("BLASTDatabasePathDisplay")),
             shiny::br()
           ),
           #BLAST program location
           shiny::fluidRow(
             shiny::strong("Select the blastn command. If no file location is selected then the program will try to run in the local directory with the default 'blastn'."),
             shiny::br(),
             shinyFiles::shinyFilesButton("blastnPath", "blastn Location",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("blastnPathDisplay")),
             shiny::br()
           ),
           #query file locations
           shiny::fluidRow(
             shiny::strong("Select a file in the folder with the fasta files you
                           would like to BLAST."),
             shiny::br(),
             shinyFiles::shinyFilesButton("querySeqPath", "Query Fasta File",title = "Fasta File(s):",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
             shiny::br(),
             shiny::br(),
             shiny::column(5,shiny::textOutput("querySeqPathDisplay")),
             shiny::br()
           ),
           shiny::numericInput("BLASTResults", "An integer value for the desired maximum number of BLAST returned results is required.", value = 200, min = 0, max = 1000),
           shiny::numericInput("BLASTminLen", "The minimum length, in nucleotides, of the reads to BLAST. All reads below this value will be removed from furtehr analyses.", value = 100, min = 0, max = 10000),
           shiny::conditionalPanel(
              condition=".Platform$OS.type != 'windows'",
              shiny::numericInput("blastSeqNumCores", "The number of cores used for the analysis. Note: Windows analyses can only use a single core.", value = 1, min = 0, max = 1000),
           ),
           #Submit button to run the script
           shiny::actionButton("blastSequences","Sequence BLAST Submit", icon = shiny::icon("play-circle"))
         )

       ),#Tab panel
       shiny::tabPanel("taxon_assign()",shiny::div(style = 'overflow-y:scroll;height:500px;',
         shiny::column(width = 4,
                       shiny::wellPanel(
                         #Data file upload
                         shiny::fluidRow(
                           shiny::strong("Select a file in the folder with the
                                         BLAST output file(s) and associated
                                         fasta file(s). In addition, if present,
                                         associated Dada ASV output file(s) will
                                         also be used to combine with the
                                         taxonomic results. NOTE: if no paired
                                         ASV file is present, the BLAST results
                                         and sequence records will still be
                                         processed."),
                           shiny::br(),
                           shinyFiles::shinyFilesButton("taxaAssignFileLoc", "Select a file in the location of BLAST and Fasta file",title = "BLAST and Fasta File(s):",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                           shiny::br(),
                           shiny::br(),
                           shiny::column(5,shiny::textOutput("taxaAssignFileLocDisplay")),
                           shiny::br()
                         ),
                         #Data file accessionTaxa.sql
                         shiny::fluidRow(
                           shiny::strong("Please select the NCBI accessionTaxa.sql data base file to use in constructing the custom database."),
                           shiny::br(),
                           shinyFiles::shinyFilesButton("taxaAssignDBLoc", "Select the NCBI Taxon Database File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                           shiny::br(),
                           shiny::br(),
                           shiny::column(5,shiny::textOutput("taxaAssignDBLocDisplay")),
                           shiny::br()
                         ),
                         shiny::numericInput("taxaAssignNumCores", "The number of cores used for the analysis. Note: Windows analyses can only use a single core. (Default numCores = 1).", value = 1, min = 0, max = 1000)
                       )
         ),shiny::column(width = 4,
                         shiny::wellPanel(
                           shiny::br(),
                           shiny::numericInput("coverage", "The percent coverage used for taxonomic assignment for the above threshold results (Default coverage = 95)", value = 95, min = 0, max = 100),
                           shiny::numericInput("ident", "The percent identity used for the taxonomic assignment for above threshold results (Default ident = 95)", value = 95, min = 0, max = 100),
                           shiny::numericInput("propThres", "The proportional threshold flags the final result based on the preponderance of the data. So if the threshold is set to 0.95, results will be flagged if the taxa direclty below the assigned taxa has fewer than 0.95% of the records causing the upward taxonomic placement (Default propThres = 0.95).", value = 0.95, min = 0, max = 1)
                         )
         ),shiny::column(width = 4,
                         shiny::wellPanel(
                           shiny::numericInput("coverReportThresh", "The percent coverage threshold used for reporting flags below this threshold (Default coverReportThresh = 95)", value = 95, min = 0, max = 100),
                           shiny::numericInput("identReportThresh", "The percent identity threshold used for reporting flags below this threshold (Default identReportThresh = 95)", value = 95, min = 0, max = 100),
                           shiny::radioButtons("includeAllDada", "When paired Dada ASV tables are present, when set to FALSE, this will exclude records without taxonomic assignment (Default includeAllDada = TRUE)", c("TRUE", "FALSE")),
                           #Submit button to run the script
                           shiny::actionButton("taxonAssign","Taxon Assign Submit", icon = shiny::icon("play-circle"))
                         )
         ))
       ),#Tab panel
       shiny::tabPanel("combine_assign_output()",
                       shiny::wellPanel(
                         #Combine Taxa Assign file in the target location
                         shiny::fluidRow(
                           shiny::strong("Select a file in the file folder with
                                         the taxa assigned files you would like
                                         to combine (extension
                                         '_taxaAssign_YYYY_MM_DD_HHMM.tsv').
                                         NOTE: all '_taxaAssign_' files in the
                                         folder location should originate from
                                         the same dada output file but have
                                         outputs from different BLAST sequence
                                         libraries and therefore contain the same ASVs."),
                           shiny::br(),
                           shinyFiles::shinyFilesButton("combineTaxaFileLoc", "Taxa Assign File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                           shiny::br(),
                           shiny::br(),
                           shiny::column(5,shiny::textOutput("combineTaxaFileLocDisplay")),
                           shiny::br()
                         ),

                         shiny::numericInput("combineTaxaNumCores", "The number of cores used for the analysis. Note: Windows analyses can only use a single core. (Default numCores = 1).", value = 1, min = 0, max = 1000),
                         #Submit button to run the script
                         shiny::actionButton("combineTaxa","Combine Taxa Assign", icon = shiny::icon("play-circle"))
                       )
       ),#Tab panel
       shiny::tabPanel("reduce_taxa()",
                       shiny::wellPanel(
                         #Combine Taxa Assign file in the target location
                         shiny::fluidRow(
                           shiny::strong("Select a file in the file folder with
                                         taxon assign files you would like to
                                         reduce (extension '_taxaAssign_YYYY_MM_DD_HHMM.tsv'
                                         or 'YYYY_MM_DD_HHMM_taxaAssignCombine.tsv' ).
                                         NOTE: For use in the mapping visualization
                                         component of the DBTCShiny where read numbers
                                         are retained the name of the files should be
                                         'markerName' followed by '_taxaAssign_YYYY_MM_DD_HHMM.tsv'"),
                           shiny::br(),
                           shinyFiles::shinyFilesButton("reduceTaxaFileLoc", "Taxa Assign File Location",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                           shiny::br(),
                           shiny::br(),
                           shiny::column(5,shiny::textOutput("reduceTaxaFileLocDisplay")),
                           shiny::br()
                         ),
                         shiny::numericInput("reduceTaxaNumCores", "The number of cores used for the analysis. Note: Windows analyses can only use a single core. (Default numCores = 1).", value = 1, min = 0, max = 1000),
                         #Submit button to run the script
                         shiny::actionButton("reduceTaxa","Reduce Taxa Assign", icon = shiny::icon("play-circle"))
                       )
       ),#Tab panel
       shiny::tabPanel("combine_reduced_output()",
                       shiny::wellPanel(
                         #Combine Taxa Assign file in the target location
                         shiny::fluidRow(
                           shiny::strong("Select a file in the folder with
                                         the reduced taxa files you would
                                         like to combine (extension
                                         '_taxaReduced_YYYY_MM_DD_HHMM.tsv')."),
                           shiny::br(),
                           shinyFiles::shinyFilesButton("combineReducedTaxaFileLoc", "Reduced Taxa File Location(s)",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL),
                           shiny::br(),
                           shiny::br(),
                           shiny::column(5,shiny::textOutput("combineReducedTaxaFileLocDisplay")),
                           shiny::br(),
                           shiny::br()
                         ),
                         # Get an input to see if the submitted files will be combined by taxa or by taxa
                         # and sample/marker name and then replaced with 1/0 presence absence
                         shiny::radioButtons("presenceAbsence", "When combining the ASV tables there are two choices. Change all positive reads to 1 for presence absence data (TRUE) - used when sample names across markers are the same. Or keep read data values (FALSE) - used when sample names contain marker information (Default presenceAbsence = TRUE)", c("TRUE", "FALSE")),
                         #Submit button to run the script
                         shiny::actionButton("combineReduceTaxa","Combine Reduced Taxa Assign", icon = shiny::icon("play-circle"))
                       )
       )
    )#Closing off the tabBox
  )#Closing the tab item
}#Closing the function

 #############  Mapping Tab ###########################################################
 mappingDashboard <- function() {
   shinydashboard::tabItem(tabName = "mappingDashboard",shiny::h1(shiny::strong("Mapping and Filtering")),
    shiny::p("Perform geospatial analysis based on collection locations.",
    style = "font-size:16px;"),
    shiny::tabsetPanel(id = "map_filter_table_tabbox",
      shiny::tabPanel("Mapping", shinycssloaders::withSpinner(leaflet::leafletOutput("mymap", height = 700))),
      shiny::tabPanel("Data Import",
          shinydashboard::box( width = 12,title = "Filter Options",status = "warning",solidHeader = TRUE,collapsible = T,
          shiny::column(width = 10,
            shiny::p("Select a file of interest in the Reduced Taxa Assign output format ('..._taxaReduced_YYYY_MM_DD_HHMM.tsv' or 'YYYY_MM_DD_HHMM_CombineTaxaReduced.tsv') as input to view the data on the map."),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 2, shinyFiles::shinyFilesButton("ASVFile", "ASV Data File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL)),
              shiny::column(width = 1),
              shiny::column(width = 5, shiny::textOutput("ASVFileOut")),
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 2, shinyFiles::shinyFilesButton("provenanceDataFile", "Associated Provenance Data File",title = "Select File:",icon = shiny::icon("magnifying-glass"), multiple = FALSE, buttonType = "default", class = NULL)),
              shiny::column(width = 1),
              shiny::column(width = 5, shiny::textOutput("provenanceDataFileOut")),
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 2, shiny::actionButton("submitDataImport","Submit", icon = shiny::icon("play-circle"))),
              shiny::column(width = 1),
              shiny::column(width = 5, shiny::actionButton("resetDataImport","Reset", icon = shiny::icon("refresh")))
            )
          ))
      ),
      shiny::tabPanel("Data Filtering",
        shinydashboard::box( width = 12,title = "Filter Options",status = "warning",solidHeader = TRUE,collapsible = T,
          shiny::column(width = 6,
          shiny::p(shiny::tags$b(shiny::tags$u("1. Taxonomic Filtering", style = "font-size:14px;"))),
          shiny::wellPanel(
          #Dropdown menu for SampleID
          shinyWidgets::pickerInput(inputId = "finalRankInput","Final Rank",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "kingdomFilterInput","Kingdom",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "phylumFilterInput","Phylum",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "classFilterInput","Class",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "orderFilterInput","Order",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "familyFilterInput","Family",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "genusFilterInput","Genus",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE),
          shinyWidgets::pickerInput(inputId = "speciesFilterInput","Species",
            choices = "NA",
            selected = "NA",
            options = list('actions-box' = TRUE),
            multiple = TRUE)
        )),
        shiny::column(width = 6,
          shiny::p(shiny::tags$b(shiny::tags$u("2. Provenance Data Filtering", style = "font-size:14px;"))),
          shiny::wellPanel(
            #Dropdown menu for Sample
            shinyWidgets::pickerInput(inputId = "campaignInput","Campaign",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Dropdown menu for Sample
            shinyWidgets::pickerInput(inputId = "sampleFilterInput","Sample",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Dropdown menu for Run
            shinyWidgets::pickerInput(inputId = "runFilterInput","Run",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Dropdown menu for Lab
            shinyWidgets::pickerInput(inputId = "labFilterInput","Laboratory",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Dropdown menu for Type
            shinyWidgets::pickerInput(inputId = "typeFilterInput","Type",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Dropdown menu for the different molecular Markers
            shinyWidgets::pickerInput(inputId = "markerFilterInput","Molecular Markers",
              choices = "NA",
              selected = "NA",
              options = list('actions-box' = TRUE),
              multiple = TRUE),
            #Slider for date
            shiny::sliderInput("dateInput","Select Event Date Range",
                               min =as.Date(Sys.Date(), "%Y-%m-%d"),
                               max = as.Date(Sys.Date(),"%Y-%m-%d"),
                               value = range(c(as.Date(Sys.Date(), "%Y-%m-%d"),
                                               as.Date(Sys.Date(), "%Y-%m-%d"))),
                               timeFormat = "%Y-%m-%d",step = 1),
            shiny::br()
          )
        ),
        shiny::column(width = 12,
          shiny::p(shiny::tags$b(shiny::tags$u("3. Quality Intrepretation", style = "font-size:14px;"))),
          shiny::wellPanel(
            shiny::column(width = 6,
              #Add in option buttons to include or exclude Result_Code
              shiny::numericInput("abundanceLow", "Enter a Lower Value:", value = 0, min = 0, max = 0),
              shiny::radioButtons("SFATButton", "Include saturated filtered taxa above threshold (SFAT):", c("Yes", "No")),
              shiny::radioButtons("BIRTButton", "Include final taxa below identity reporting threshold (BIRT):", c("Yes", "No")),
              shiny::radioButtons("TBATButton", "Include results where records from the taxonomic level below the reported taxon may have a questionable data record (TBAT):", c("Yes", "No")),
            ),
            shiny::column(width = 6,
              #Number of reads inputs
              shiny::numericInput("abundanceHigh", "Enter a Higher Value:", value = 0, min = 0, max = 0),
              shiny::radioButtons("SANFButton", "Include saturated non-filtered (SANF):", c("Yes", "No")),
              shiny::radioButtons("BCRTButton", "Include final taxa below nucleotide coverage reporting threshold (BCRT):", c("Yes", "No")),
              shiny::br(),
              #Data file upload
              shiny::fluidRow(
                shiny::column(6,shiny::actionButton("updateFilterMappingButton", "Update Filtering Options", icon = shiny::icon("play"))),
                shiny::column(6,shiny::actionButton("resetFilterMappingButton", "Reset Filtering Options", icon = shiny::icon("refresh")))
              ),
            ),
            shiny::br()
          )
        )
      )),#End of top box

      shiny::tabPanel("Data Table", DT::DTOutput("dataTable"))
#      shiny::tabPanel("Data Table", shiny::DTOutput("dataTable"))
    )#Tab Box
   )#Tab Item
 }

################## Define UI for application ##################################
shinyAppUI <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title ="DBTCShiny"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tab_being_displayed",
      shinydashboard::menuItem("Welcome", tabName = "welcomePage", icon = shiny::icon("door-open")),
      shinydashboard::menuItem("DBTC Tools", tabName = "dbtcTools", icon = shiny::icon("play-circle")),
      shinydashboard::menuItem("Mapping Dashboard", tabName = "mappingDashboard", icon = shiny::icon("map"))
    )
  ),

  ##Body content
  dashBoardBodyComponent()
)
