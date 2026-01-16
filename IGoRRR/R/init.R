init <- function(envir,
                 examples,
                 language) {
  
  # Every message, every label that is output in national language come from here
  .IGoR$Z <- jsonlite::fromJSON(system.file("text",paste0(language,".json"), package="IGoRRR"))
  
  if ("environment" %not in% class(envir)) stop(.IGoR$Z$init$error.envir)
  
  # Ask the user to load the packages needed by the generated code.
  # For beginners from scratch, we keep the generated code as simple as possible 
  # and the only solution is to alter the search path.
  # As this is prohibited by CRAN policies we need an explicit action of the user.
  packages <- c(
    "dplyr",
    "tidyr",     # in pages 'gather' and 'spread'
    "rio",       # in pages 'import' and 'export'
    "fst",       # in page 'import' only
    "feather",   # in page 'import' only
    "readODS",   # in page 'import' only
    "fuzzyjoin", # in page 'fuzzyjoin' only
    "tables",    # in page 'tabular only
    "skimr",     # in page 'skim' only
    "ggformula", # in graphic pages aand used by IGoR in page 'cut'
    "Hmisc",     # for weighted statistics
    "purrr",     # for 'partial' in 'tabular' page
    "stringr",
    "lubridate"
  )
  l <- search()
  l <- substring(l[grep("package:",l)],9)
  l <- setdiff(packages, l)
  if (length(l)>0) {
    message(.IGoR$Z$init$require.msg)
    print(l)
    if (!..isTRUE(utils::askYesNo(.IGoR$Z$init$require.ask)))
      stop(.IGoR$Z$init$require.err)
    l <- paste(
           unlist(Map(function(x) paste0("library(",x,")"),l)),
           collapse='\n'
         )
    eval(parse(text=l))
  }
  

# Initialize IGoR environment ---------------------------------------------

  .IGoR$config <- jsonlite::fromJSON(system.file("text","config.json", package="IGoRRR"))
  .IGoR$config$volumes <- 
    c("data" = system.file("misc",package="IGoRRR"),
      unlist(.IGoR$config$volumes) # 'shinyFiles' require vectors without message
    )
  
  # The working environment
  .IGoR$env <- envir
  
  # Will contain the log table, initiated by ..writeLog
  .IGoR$log <- list()

  # Used to synchronize page contents to table contents
  .IGoR$sync <- list()

  # Some constants
  .IGoR$MAXROWNAMES = 100              # Maximum rows number for a row.names menu ('browse','view')
  .IGoR$COLORS      = c("black","red","green","blue","white","yellow","pink")
  
  # First item for column selection, when multiple=FALSE
  .IGoR$NONE        = setNames('', .IGoR$Z$any$none)
  .IGoR$TABLE       = setNames('', .IGoR$Z$any$table)
  .IGoR$COLV        = setNames('', .IGoR$Z$any$col)
  .IGoR$CHRCOLV     = setNames('', .IGoR$Z$any$col.chr)
  .IGoR$QALCOLV     = setNames('', .IGoR$Z$any$col.discrete)
  .IGoR$NUMCOLV     = setNames('', .IGoR$Z$any$col.numeric)

  # A recording of every generated code
  ..writeLog("","#Yes master!", append=FALSE)


# Load some example tables ------------------------------------------------

  if (examples) {
    df <- readRDS(system.file("misc","example.RDS",package="IGoRRR"))
    attr(df,'source') <- 'IGoR'
    assign(.IGoR$Z$init$example1, df, envir=.IGoR$env)
    
    df <- data.frame(age=c( 20, 30, 40, 50, 60, 20, 30, 40, 50, 60, NA),
                    sexe=c("M","M","M","M","M","F","F","F","F","F","F"),
                   poids=c(  1,  1,  1,  1,  3,  6,  2,  2,  2,  2,  1))
    attr(df,'source') <- 'IGoR'
    assign(.IGoR$Z$init$example2, df, envir=.IGoR$env)
    
    df <- mapsf::mf_get_mtq()
    attr(df,'source') <- 'mapsf'
    assign(.IGoR$Z$init$example.sf, df, envir=.IGoR$env)
    
    df <- datasets::mtcars
    attr(df,'source') <- 'datasets'
    assign("mtcars", df, envir=.IGoR$env)
  }

}
