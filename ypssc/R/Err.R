
####################################################################################################################################
################################## Err #############################################################################################
# >>

Err            = new.env()

####################################################################################################################################

Err$msg        = ""
Err$prefix     = "ypssc"

Err$tab        = "    "
Err$msgDash    = " - "
Err$fullprefix = ""
Err$funcType   = ""
Err$msgType    = ""
Err$msgColon   = " : "
Err$lenOfLine  = 50
Err$boxLen     = 60

Err$checkAndWriteMsg = function() { # checkAndWriteMsg >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    Err$checkMsg()
    Err$writeMsg()
}

Err$checkMsg = function() { # checkMsg >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    if( is.character(Err$msg) )                         { Err$msgType = "character" }
    else if( is.double(Err$msg) | is.integer(Err$msg) ) { Err$msgType = "double"    }
    else { stop( paste("Please provide either character/string or double/integer message for ", Err$funcType, sep = ""), call. = FALSE) }

}

Err$writeMsg = function() { # writeMsg >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$msg = gsub("\n", " \n ", Err$msg)

    if( Err$prefix == "" ) {
        Err$fullprefix = paste( Err$tab, Err$funcType, Err$msgColon, sep = "" )
    } else {
        Err$fullprefix = paste( Err$tab, Err$prefix, Err$msgDash, Err$funcType, Err$msgColon, sep = "" )
    }

    if( Err$msgType == "character" ) {
        lines = Err$getListOfLines(Err$msg)
        for( i in 1:length(lines) ) {
            msg   = paste(Err$fullprefix, lines[i], sep = "")
            writeLines(msg)
        }
    } else {
        if( Err$msg < 0 ) {
            stop( paste("Please provide non-negetive integer.", funcType, sep = ""), call. = FALSE)
        } else if( Err$msg == 0 ) {
            writeLines("")
        } else {
            for( i in 1:Err$msg ) { writeLines(Err$fullprefix) }
        }
    }
}

Err$getListOfLines = function(text) { # writeMsg >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    words = unlist( strsplit(text, " ") )
    lines = c()
    l = 0; w = 0

    while( w < length(words) ) {
        l = l + 1
        w = w + 1
        if( words[w] == "\n" ) { lines[l] = ""; next}
        else{ lines[l] = words[w]}
        while( nchar(lines[l]) <= Err$lenOfLine & w < length(words) ) {
            w = w + 1
            if( words[w] == "\n" ) { break }
            else{ lines[l] = paste( lines[l], words[w], sep = " " ) }
        }
    }

    return(lines)

}

####################################################################################################################################

Err$help = function() { # note >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    prefixCurrent = Err$prefix

    Err$prefix = "Err"
    tab         = Err$tab

    Err$box( paste("Usage:", "\n",
             "\n",
             tab, "Set Prefix (optional):", "\n",
             "\n",
             tab, tab, "Err$prefix = 'Shashank'", "\n",
             "\n",
             tab, "Methods for displaying Notes, Warnings, Fatal error", "\n",
             tab, tab, "Err$note('your text')", "\n",
             tab, tab, "Err$warn('your text')", "\n",
             tab, tab, "Err$abort('your text')", "\n", sep = "") )

    Err$prefix = prefixCurrent

}

Err$note  = function(msg) { # note >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$msg = msg; Err$funcType = "NOTE";    Err$checkAndWriteMsg()
    msg = 15
}

Err$warn  = function(msg) { # warn >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$msg = msg; Err$funcType = "WARNING"; Err$checkAndWriteMsg()

}

Err$abort = function(msg) { # abort >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$note(0)
    Err$msg = msg; Err$funcType = "FATAL";   Err$checkAndWriteMsg()
    Err$note(0)
    Err$msg = "Aborting..."; Err$funcType = "FATAL"; Err$checkAndWriteMsg()
    Err$note(0)
    Err$note(0)
    exit()

}

Err$box   = function(msg) { # box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$note(0)
    Err$note( paste0( rep(">", Err$boxLen), collapse = '' ) )
    Err$note(1)
    Err$note( msg )
    Err$note(1)
    Err$note( paste0( rep("<", Err$boxLen), collapse = '' ) )
    Err$note(0)

}


Err$reset = function() { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    Err$prefix     = "ypssc"

}

####################################################################################################################################


################################## Err #############################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# Err$note( "dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text " )
# Err$note( "dummy text dummy text \n dummy text dummy text \ndummy text\n dummy text dummy text dummy text\ndummy text dummy text dummy text dummy text dummy text \n dummy text " )
# Err$note( 0 )
# Err$note( 1 )
# Err$note( 2 )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################
