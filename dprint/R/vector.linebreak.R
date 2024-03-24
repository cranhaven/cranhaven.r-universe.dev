#' Vector Line Break
#'
#' To break vectors apart around an escape character [backslash]n that indicates a line break.  
#' This is designed for handling line breaks for the table title (main) and the footnotes
#'
#' @param vctr character vector
#' @export
vector.linebreak <-
function(vctr)
{
  grep.linebreak <- grep("\n",vctr)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak   <- strsplit(vctr[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak          <- length(strsplit.linebreak[[1]]) # store how many line breaks
       v.temp               <- strsplit.linebreak[[1]]
       if (length(vctr)==1) {vctr <- v.temp}
       else
         {
           if (grep.linebreak[1] == 1)
            {  # Insert new expanded rows into orignal object (if occurs at begining of data frame)
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.temp, v.after)
            }
           else if (grep.linebreak[1] == length(vctr))
            {  # Insert new expanded rows into orignal object (if occurs at end of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              vctr      <- c(v.before, v.temp)
            }
           else
            {  # Insert new expanded rows into orignal object (if occurs in middle of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.before, v.temp, v.after)
            }
            grep.linebreak <- grep("\n",vctr)
            # Recursive Calls
            if (length(grep.linebreak) > 0) {vctr <- vector.linebreak(vctr)}
         }
    }
  vctr
}

