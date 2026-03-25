
####################################################################################################################################
####################################################################################################################################
# >>
#' @title calculationBetaSheet
#' @param df Dataframe for input file data.
#' @param sampleNames Names of the samples found in the input file.
#' @param sampleNamesUpdate Updated names of the samples found in the input file.
#' @param dateTimeCurrent Date and time at the time the simulation began.
#' @noRd
# <<
####################################################################################################################################
####################################################################################################################################


####################################################################################################################################
##################################### calculationBetaSheet() #######################################################################
# >>
calculationBetaSheet <- function( df, sampleNames, sampleNamesUpdate, dateTimeCurrent ) {

	# Performing Beta-sheet calculation for database ####
	Err$prefix = "Beta"
    Err$note(0)
    Err$note( "Performing Beta-sheet calculation for database..." )

    dataBase_beta    = select( dataBase_beta, c(1,2) )

    num_Pro_baa    = unique( dataBase_beta$id )
    protein        = vector()
    num_baa_pro_DB = vector()

    pb_1 = winProgressBar( title = "progress bar",
                           min   = 0,
                           max   = length(num_Pro_baa),
                           width = 300 )
    i = 1
    for( i in 1 : length(num_Pro_baa) ) {

        item                = num_Pro_baa[i]
        proteins            = filter( dataBase_beta, id == item )
        num_baa_pro_DB_temp = length(proteins$id)
        num_baa_pro_DB      = c( num_baa_pro_DB_temp, num_baa_pro_DB )
        protein             = c( unique(proteins$id), protein )
        proteins            = vector()
        num_baa_pro_DB_temp = vector()

        setWinProgressBar( pb_1, i,
                           title = paste( 'Beta-sheet calculation for database     ',
                                          round( i/length(num_Pro_baa)*100, 0 ),
                                          "% done") )
    }

    Err$note("Done." )
    close(pb_1)

    # Calculating the number of amino acids for beta ####

    baa              = data.frame( id      = protein,
                                   num_baa = num_baa_pro_DB )
    cal_for_database = left_join(  dataBase_numOfAA,
                                   baa,
                                   by = 'id' )

    Sys.sleep(0.5)

    # Samples ####

    i = 1
    for( i in 1 : length(sampleNames) ) {

        Err$note( 0 )
        Err$note( paste0("Working on sample: ", sampleNamesUpdate[i] ) )
        Err$note( 1 )

        # Peptides in the sample >>
        Err$note( "Geting list of peptides..." )
        temp = which( names(df) == sampleNames[i] )
        sample_peptides = filter( df, df[,temp] > 0 )
        Err$note("Done." )
        nameFile = gsub( " ", "_", paste0( dateTimeCurrent, " ", 'list of peptides in', sampleNamesUpdate[i], '.csv' ), fixed = FALSE )
        write.csv( sample_peptides,
                   nameFile,
                   row.names = FALSE )
        updateUserFileCreated( nameFile )
        Err$note( 1 )
        sample = paste( as.character(sampleNamesUpdate[i]), '_ peptides' )
        assign( sample, sample_peptides )

        # Proteins in the sample >>
        Err$note( "Geting list of proteins..." )
        sample_proteins = unique(sample_peptides$Proteins)
        Err$note("Done." )
        nameFile = gsub( " ", "_", paste0( dateTimeCurrent, " ", 'list of proteins in', sampleNamesUpdate[i], '.csv' ), fixed = FALSE )
        write.csv( sample_proteins,
                   nameFile,
                   row.names = FALSE )
        updateUserFileCreated( nameFile )
        Err$note( 1 )
        sample = paste( as.character(sampleNamesUpdate[i]), '_ proteins' )
        assign( sample, sample_proteins )

        # Calculating beta-sheet coverage for samples >>
        Err$note( "Calculating beta-sheet coverage for samples..." )
        proteins_in_s = vector()
        aa_in_s       = vector()
        baa_in_s      = vector()

        pb_2 = winProgressBar( title = "progress bar",
                               min   = 0,
                               max   = length(sample_proteins),
                               width = 300 )

        j = 1
        for( j in 1 : length(sample_proteins) ) {

            item      = sample_proteins[j]
            Pro_chunk = filter( sample_peptides, sample_peptides$Proteins == item )

            k         = 1
            list_aa_s = vector()

            for( k in 1 : length(Pro_chunk$Proteins) ) {

                start          = Pro_chunk$Start.position[k]
                end            = Pro_chunk$End.position[k]
                list_aa_s_temp = seq(start:end)
                list_aa_s_temp = list_aa_s_temp+start-1
                list_aa_s      = c( list_aa_s_temp, list_aa_s )
                list_aa_s_temp = vector()

            }

            proteins_temp = item
            proteins_in_s = c( proteins_temp, proteins_in_s )
            proteins_temp = vector()

            aa_in_s_temp  = length( unique(list_aa_s) )
            aa_in_s       = c( aa_in_s_temp, aa_in_s )
            aa_in_s_temp  = vector()

            protein_chunk_dataBase = filter( dataBase_beta, id == item )

            baa_in_s_temp = unique(list_aa_s)%in%protein_chunk_dataBase$n
            baa_in_s_temp = sum(baa_in_s_temp)
            baa_in_s      = c( baa_in_s_temp, baa_in_s )
            baa_in_s_temp = vector()

            results = data.frame( id                             = proteins_in_s,
                                  num_amino_acids_in_sample      = aa_in_s,
                                  num_beta_amino_acids_in_sample = baa_in_s )

            results = left_join( results, cal_for_database, by = 'id' )

            setWinProgressBar( pb_2, j,
                               title = paste( 'Beta-sheet calculation for ',
                                              sampleNames[i],
                                              '    ',
                                              round( j/length(sample_proteins)*100, 0 ),
                                              "% done") )

        }

        Err$note( "Done" )
        nameFile = gsub( " ", "_", paste0( dateTimeCurrent, " ", 'beta sheet analysis of', sampleNamesUpdate[i], '.csv' ), fixed = FALSE )
        write.csv( results,
                   nameFile,
                   row.names = FALSE )
        updateUserFileCreated( nameFile )

        close(pb_2)

    }

    Err$reset()

    return( invisible(NULL) )
}
# <<
##################################### calculationBetaSheet() #######################################################################
####################################################################################################################################
