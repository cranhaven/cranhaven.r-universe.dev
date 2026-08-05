# Copyright 2024. Jaime Salvador
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this proprietary software and associated documentation files (the "Software"),
# to use, publish, or distribute copies of the Software, and to permit persons to
# whom the Software is furnished to do so.
#
# Any other use, including modifying, adapting, reverse engineering, decompiling,
# or disassembling, is not permitted.
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' @title Execute a Redatam command from text
#' @author Jaime Salvador
#' @description Execute a Redatam command: TABLE or AREALIST.
#' This function removes all the rows that contain total, na or mv values. Additionally, this function removes the "mask" columns.
#' @param dic Dictionary identifier
#' @param spc Program text in SPC format
#' @param tot.omit Omit rows containing total, na y mv values
#' @return {
#' If the program contains more than one table, the method returns the last table (in the SPC program) as a data frame.
#' All the tables are registered (as data frames) in a custom environment called \sQuote{redatam::outputs}.
#' }
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' df<-redatam_query(dic,"freq person.sexo")
#' }

redatam_query<-function( dic, spc, tot.omit=TRUE ) {

  #assign("mi_valor", valor, envir = as.environment("package:redatam"))

  dfs<-redatam_internal_query( dic, spc );
  #dfs<-redatam_query(dic, spc)

  count<-length(dfs)
  index<-0

  for (df in dfs) {

    attrs <- attributes(df)

    dsname<-attr(df,"redatam.table.name")

    attr_type<-attr(df,"redatam.table.type")
    attr_name<-attr(df,"redatam.table.name")
    attr_vars<-attr(df,"redatam.table.vars")

    if(attrs$redatam.table.type=="table"){

      if( tot.omit ){
        cols <- c()

        for (i in 1:length(colnames(df)) ) {
          if( (i %% 3)==0 ) {
            df<-df[df[,i]==0, ]
          }
          else
          {
            cols<-append(cols,i)
          }
        }

        row.names(df)<-c(1:nrow(df))

        df <- df[,cols]

        attr(df,"redatam.table.type")<-attr_type
        attr(df,"redatam.table.name")<-attr_name
        attr(df,"redatam.table.vars")<-attr_vars
      }
    }
    else if(attrs$redatam.table.type=="arealist") {
      # remove empty cols
    }

    #assign( dsname, df, .GlobalEnv )
    assign( dsname, df, envir=outputs )

    if( index==count-1 ) {
      df_last <- df
    }

    index<-index+1
  }

  if(count>0)
    return(df_last)
}


