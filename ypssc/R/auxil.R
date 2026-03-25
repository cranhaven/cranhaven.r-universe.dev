
####################################################################################################################################
##################################### auxil functions ##############################################################################
####################################################################################################################################

filter <- dplyr::filter

####################################################################################################################################
##################################### checkFileInput() #############################################################################
# >>
checkFileInput <- function( pathFileInput = NULL ){

    # Checking if `pathFileInput` is provided or the file exists >>
    if ( is.null(pathFileInput) ) { isFile = FALSE }
    else                          { isFile = file.exists( pathFileInput ) | startsWith( pathFileInput, "https") }

    # Selecting an input file from a doalog box if `pathFileInput` is not provided or if `pathFileInput` does not exists >>
    if ( !isFile ) {
        msg = paste0( "Argument `pathFileInput` is not provided.\n",
                      "              or \n",
                      "The file does not exists.\n",
                      "\n",
                      "Input csv file generated from MaxQuant is necessary to run this function.\n",
                      "\n",
                      "Do you want to select an input csv file generated from MaxQuant?" )
        Err$warn( 0 )
        Err$warn( msg )
        response = dlgMessage( msg,
                               type = "yesno" )$res

        if ( response == "yes" ) {
            msgSelectFile = "Please select an input csv file generated from MaxQuant."
            Err$note( 0 )
            Err$note( paste0( "A dialog box must have been opened.\n",
                              msgSelectFile ) )
            pathFileInput = dlg_open( title = msgSelectFile )$res
        } else {

            updateUserFailure( "Argument `pathFileInput` is not provided." )
        }
    }

    # Printing `pathFileInput` provided >>
    Err$note( 0 )
    Err$note( paste0( "Provided input csv file generated from MaxQuant:\n",
                      "'", pathFileInput, "'" ) )

    return( pathFileInput )

}
# <<
##################################### checkFileInput() #############################################################################
####################################################################################################################################


####################################################################################################################################
##################################### checkDirOutput() #############################################################################
# >>
checkDirOutput <- function( pathDirOutput = NULL ){

    # Checking if `pathDirOutput` is provided or the dir exists >>
    if ( is.null(pathDirOutput) ) { isDir = FALSE }
    else                          { isDir = dir.exists( pathDirOutput ) }

    # Selecting an output dir from a doalog box if `pathDirOutput` is not provided or if `pathDirOutput` does not exists >>
    if ( !isDir ) {
        msg = paste0( "Argument `pathDirOutput` is not provided.\n",
                      "              or \n",
                      "The directory does not exists.\n",
                      "\n",
                      "Do you want to select an output directory? \n",
                      "'Yes': Then select. \n",
                      "'No' : Then the current working directory will be selected." )
        Err$warn( 0 )
        Err$warn( msg )
        response = dlgMessage( msg,
                               type = "yesno" )$res

        if ( response == "yes" ) {
            msgSelectDir = "Please select an output directory."
            Err$note( 0 )
            Err$note( paste0( "A dialog box must have been opened. \n",
                              msgSelectDir ) )
            pathDirOutput = dlg_dir( title = msgSelectDir )$res
        } else {
            pathDirOutput = getwd()
        }
    }

    # Printing `pathDirOutput` provided >>
    Err$note( 0 )
    Err$note( paste0( "Provided output directory where the output files will be saved:\n",
                      "'", pathDirOutput, "'" ) )

    return( pathDirOutput )

}
# <<
##################################### checkDirOutput() #############################################################################
####################################################################################################################################


####################################################################################################################################
##################################### readFileInput() ##############################################################################
# >>
readFileInput <- function( pathFileInput, isTest ) {

    Err$note(0)
    Err$note( paste0( "Reading input file...." ) )

    # Reading csv file >>

    df = read.csv( pathFileInput )

    # Removing the columns that are not needed and finding the columns containing sample information >>

    df = df[ , -which( names(df) %in% c(    "Sequence","N.term.cleavage.window",
                                            "C.term.cleavage.window","Amino.acid.before",
                                            "First.amino.acid","Second.amino.acid",
                                            "Second.last.amino.acid","Last.amino.acid",
                                            "Amino.acid.after","A.Count","R.Count","N.Count",
                                            "D.Count","C.Count","Q.Count","E.Count",
                                            "G.Count","H.Count","I.Count","L.Count",
                                            "K.Count","M.Count","F.Count","P.Count",
                                            "S.Count","T.Count","W.Count","Y.Count",
                                            "V.Count","U.Count","O.Count","Length",
                                            "Missed.cleavages","Mass",
                                            "Leading.razor.protein","Gene.names",
                                            "Protein.names","Unique..Groups.",
                                            "Unique..Proteins.","Charges","PEP",
                                            "Score","Experiment.ST168.THF.A",
                                            "Experiment.ST168.THF.O",
                                            "Experiment.ST169.DMSO.A","Experiment.ST169.DMSO.O",
                                            "Experiment.ST170.But.A","Experiment.ST170.But.O",
                                            "Intensity.ST168.THF.A",
                                            "Intensity.ST168.THF.O",
                                            "Intensity.ST169.DMSO.A",
                                            "Intensity.ST169.DMSO.O",
                                            "Intensity.ST170.But.A",
                                            "id","Protein.group.IDs","Mod..peptide.IDs",
                                            "Evidence.IDs","MS.MS.IDs","Best.MS.MS",
                                            "Oxidation..M..site.IDs","Taxonomy.IDs",
                                            "MS.MS.Count" ) ) ]

    names = names(df)

    sampleNames         = names[ grepl("Intensity.", names) ]
    sampleNamesUpdate   = gsub( '\\.|Intensity.', ' ', sampleNames )
    names_list          = vector()
    i  = 1
    pb = winProgressBar( title = "progress bar",
                         min   = 0,
                         max   = length(sampleNames),
                         width = 300 )

    for ( i in 1 : length(sampleNames) ) {
        # temp       = paste(sampleNamesUpdate[i],' \n \n ')
        temp       = sampleNamesUpdate[i]
        names_list = c( names_list, temp )
        Sys.sleep(0.1)
        setWinProgressBar( pb, i, title = paste( sampleNamesUpdate[i], '    ', round(i/length(sampleNames)*100, 0), "% done") )
    }

    close(pb)

    # Conformation about sample names from user >>

    if ( !isTest ) {
        sampleNameConfirmation = dlgMessage( c( "Identified sample names in the uploaded file:",
                                                "\n",
                                                names_list,
                                                "\n",
                                                "If it is correct, please enter 'Yes'" ),
                                             type = "yesno" )$res
    } else {
        sampleNameConfirmation = "yes"
    }

    if ( sampleNameConfirmation != "yes" ) {

        msg = paste0( "Wrong sample names.\n",
                      "\n",
                      "Please re-run the program using correct input file." )

        updateUserFailure( msg )
    }

    # Returning multiple variables as a R-list >>

    dataFileInput                   = list()
    dataFileInput$df                = df
    dataFileInput$sampleNames       = sampleNames
    dataFileInput$sampleNamesUpdate = sampleNamesUpdate

    Err$note( "Done." )

    return( dataFileInput )
}
# <<
##################################### readFileInput() ##############################################################################
####################################################################################################################################


####################################################################################################################################
##################################### creatOutputDir() #############################################################################
# >>
creatOutputDir <- function( pathDirOutput, type = "secondary" ) {

    Err$note(0)
    Err$note( paste0( "Creating output directory" ) )

    dateTimeCurrent = format( Sys.time(), "%Y%m%d_%H%M%S" )        # << get current date and time
    nameDirOutput   = paste0( "ypssc_", dateTimeCurrent, "_", type )    # << name of the output folder
    pathDirOutput   = paste0( pathDirOutput, "/", nameDirOutput )  # << path of the output folder
    dir.create( pathDirOutput )                                    # creating new folder for output files
    setwd( pathDirOutput )              # << setting working dir to "pathDirOutput" to write output files
    Err$note( "Done." )
    Err$note( paste0( "Output directory created:\n",
                      "'", getwd(), "'" ) )

    return( dateTimeCurrent )
}
##################################### creatOutputDir() #############################################################################
####################################################################################################################################


####################################################################################################################################
##################################### removeRows() #################################################################################
# >>
removeRows <- function( df, dateTimeCurrent, isTest ) {

    Err$note(0)
    Err$note( paste0( "Filtering input file:" ) )

    if ( !isTest ) {

        # Removing rows containing doubious proteins >>
        removeDoubious = dlgMessage( paste0( "Do you want to remove the rows containing doubious proteins?\n",
                                             "Rows that have 2 or more protiens assigned to one identified peptide are called doubious\n",
                                             "Answer with yes or no" ),
                                     type = "yesno" )$res
        if ( removeDoubious == "yes" ) {
            Err$note( "Removing rows containing doubious proteins..." )
            df = filter( df, !grepl( ';', df$Proteins ) )
        }

        # Removing rows that contains peptides that matched to decoy that has reverse >>
        removeReverse  = dlgMessage( paste0( "Do you want to remove rows that contains peptides that matched to decoy that has reverse ",
                                             "sequnce of real protein?\n",
                                             "Theses proteins are usually removed.\n",
                                             "Answer with yes or no" ),
                                     type = "yesno" )$res
        if ( removeReverse == "yes" ) {
            Err$note( "Removing rows that contains peptides that matched to decoy that has reverse..." )
            df = filter( df, !grepl( '\\+', df$Reverse ) )
        }

        # Removing rows that contains peptides that are showing signs of contamination >>
        removeContaminant = dlgMessage( paste0( "Do you want to remove rows that contains peptides that are showing signs of contamination?\n",
                                                "Theses proteins are usually removed.\n",
                                                "Answer with yes or no" ),
                                        type = "yesno" )$res
        if ( removeContaminant == "yes" ) {
            Err$note( "Removing rows that contains peptides that are showing signs of contamination..." )
            df = filter( df, !grepl( '\\+', df$Potential.contaminant ) )
        }

        # Removing rows that contains peptides that are not showing any intensity >>
        removeNoIntensity  = dlgMessage( paste( "Do you want to remove rows that contains peptides that are not showing any intensity?\n",
                                                "Theses proteins are usually removed.\n",
                                                "Answer with yes or no" ),
                                         type = "yesno" )$res
        if ( removeNoIntensity == "yes" ) {
            Err$note( "Removing rows that contains peptides that are not showing any intensity..." )
            df = filter( df, df$Intensity > 0 )
        }

    } else {

        df = filter( df, !grepl( ';', df$Proteins ) )
        df = filter( df, !grepl( '\\+', df$Reverse ) )
        df = filter( df, !grepl( '\\+', df$Potential.contaminant ) )
        df = filter( df, df$Intensity > 0 )

    }

    # Print Done >>
    Err$note( "Done." )

    # Writing new df to output file in output dir >>
    Err$note(0)
    Err$note( "Writing filtered file to output file in output dir..." )
    nameFile = paste0( dateTimeCurrent, "_", 'df.csv' )
    write.csv( df, nameFile, row.names = FALSE )
    Err$note( "Done." )
    # Err$note( paste0( "Output file created:\n",
    #                   "'", getwd(), "/", nameFile, "'" ) )
    updateUserFileCreated( nameFile )

    return( df )
}
##################################### removeRows() #################################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserFailure ###############################################################################
# >>
updateUserFailure  <- function( msg, isTest = FALSE ) {

    msg = paste0( msg, "\n",
                  "\n",
                  "Analysis failed!" )

    if ( !isTest ) {
        tkmessageBox( title   = "Error",
                      message = msg,
                      icon    = "error",
                      type    = "ok" )
    }

    Err$abort( msg )

}
# <<
################################## updateUserFailure ###############################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserSuccess ###############################################################################
# >>
updateUserSuccess  <- function( isTest = FALSE ) {

    msg = paste0( "Analysis completed successfully! \n",
                  "\n",
                  "Please see output files at: \n",
                  "'", getwd(), "'" )

    if ( !isTest ) {
        tkmessageBox( title   = "Success",
                      message = msg,
                      icon    = "info",
                      type    = "ok" )
    }

    Err$note(0)
    Err$box(msg)
    Err$note(0)

}
# <<
################################## updateUserSuccess ###############################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserFileCreated ###########################################################################
# >>
updateUserFileCreated  <- function( nameFile ) {

    Err$note( paste0( "Output file created:\n",
                      "'", getwd(), "/", nameFile, "'" ) )

}
# <<
################################## updateUserFileCreated ###########################################################################
####################################################################################################################################


####################################################################################################################################
##################################### auxil functions ##############################################################################
####################################################################################################################################
