#' @title Table Dimensions
#' 
#' @description
#' Extract dimensions available
#' for a given 'nomis' table ID. e.g. the dimensions of the table 
#' 'RM011 - Country of birth by age ' are age, country and geography;
#' this function will return all of the available age, country and geography filters available on the table.
#' 
#' @param id A table ID recognised by 'nomis' (e.g "NM_1_1").
#' @param base_url Nomis API base url
#' 
#' @examples get_table_dimensions(id="NM_1240_1")
#' 
#' @returns A tidy dataframe of the dimensions, and available filtering values, of your chosen 'nomis' table.
#' @export

#The dimensions refer to the different columns of the table, i.e. year of interest, total claimants, occupation, etc.
get_table_dimensions <- function(id, base_url = "https://www.nomisweb.co.uk/api/v01") {
  dimensions_overview <- get_overview(id, base_url = base_url)$overview$dimensions$dimension
  assert_function(is.null(dimensions_overview), "Invalid table identifier - use list_tables() to generate a list of available tables")
  output <- c()
  # LOOP OVER EACH DIMENSION

  for (i in seq_along(dimensions_overview)) {
    # EXTRACT DIMENSION NAME
    dimension_name <- dimensions_overview[[i]]$concept
    
    structure_overview<-get_structure(id,dimension_name)

    
    # EXTRACT DIMENSION VALUES
    
        # IF DIMENSION IS A TYPE
        {if ("types" %in% names(dimensions_overview[[i]])) {
          if (is.null(dimensions_overview[[i]]$types$type$value)) {
            d_rows<- data.frame()
            # LOOP ACROSS EACH DIMENSION VALUE
            for (j in c(1:length(dimensions_overview[[i]]$types$type))) {
              
              d_row <- data.frame(dn = dimension_name,
                                 n = dimensions_overview[[i]]$types$type[[j]]$name,
                                 v = dimensions_overview[[i]]$types$type[[j]]$value)
              d_rows <- dplyr::bind_rows(d_rows, d_row)
              
            }
            
          }else{
            
            d_rows <- data.frame(dn = dimension_name,
                                n = dimensions_overview[[i]]$types$type$name,
                                v = dimensions_overview[[i]]$types$type$value)
            
          }
    
        }else{
        #IF DIMENSION IS STORED IN CODES
          
          len_structure <- length(structure_overview$structure$codelists$codelist[[1]]$code)
          #ONLY DO NEXT SECTION IF THE STRUNCTURE HAS LENGTH > 1, AS DIMENSIONS OF LENGTH 1 WILL FAIL HERE
          if (len_structure>1){
            #CHECK WHETHER THE OVERVIEW DATA MATCHES THE STRUCTURE DATA
              if ((structure_overview$structure$codelists$codelist[[1]]$code[[1]]$value == dimensions_overview[[i]]$codes$code[[1]]$value) & (structure_overview$structure$codelists$codelist[[1]]$code[[len_structure]]$value == dimensions_overview[[i]]$codes$code[[len_structure]]$value)) {
                  if (typeof(dimensions_overview[[i]]$codes$code[[1]]$value)=="character") {
                      if (is.null(dimensions_overview[[i]]$codes$code$value)) {
                          d_rows <- data.frame()
            
                          for (j in c(1:length(dimensions_overview[[i]]$codes$code))) {
                              d_row <- data.frame(dn = dimension_name,
                                                 n = dimensions_overview[[i]]$codes$code[[j]]$name,
                                                 v = dimensions_overview[[i]]$codes$code[[j]]$value)
                              d_rows <- dplyr::bind_rows(d_rows, d_row)
                          }
                      } else{
                          d_rows <- data.frame(dn = dimension_name,
                                              n = dimensions_overview[[i]]$codes$code$name,
                                              v = dimensions_overview[[i]]$codes$code$value)
                            }
                  } else{
                      if (is.null(dimensions_overview[[i]]$codes$code$value)) {
                         d_rows <- data.frame()
            
                         for (j in c(1:length(dimensions_overview[[i]]$codes$code))) {
                             d_row <- data.frame(dn = dimension_name,
                                                n = dimensions_overview[[i]]$codes$code[[j]]$name,
                                                v = as.integer(dimensions_overview[[i]]$codes$code[[j]]$value))
                             d_rows <- dplyr::bind_rows(d_rows, d_row)
                         }
                      } else{
                          d_rows <- data.frame(dn = dimension_name,
                                              n = dimensions_overview[[i]]$codes$code$name,
                                              v = dimensions_overview[[i]]$codes$code$value)
                      }
                  }
              } else{
              #IF STRUCTURE AND OVERVIEW DO NOT MATCH, USE THE STRUCTURE INSTEAD OF THE OVERVIEW
              #CHECK WHETHER VARIABLE IS CHARACTER 
                  if (typeof(structure_overview$structure$codelists$codelist[[1]]$code[[1]]$value)=="character") {
                      if (is.null(structure_overview$structure$codelists$codelist[[1]]$code[[1]]$value)) {
                          d_rows <- data.frame()
                
                          for (j in c(1:length(structure_overview$structure$codelists$codelist[[1]]$code))) {
                              d_row <- data.frame(dn = dimension_name,
                                                 n = structure_overview$structure$codelists$codelist[[1]]$code[[j]]$description$value,
                                                 v = structure_overview$structure$codelists$codelist[[1]]$code[[j]]$value)
                              d_rows <- dplyr::bind_rows(d_rows, d_row)
                          }
                      } else{
                
                          d_rows <- data.frame(dn = dimension_name,
                                              n = structure_overview$structure$codelists$codelist[[1]]$code[[j]]$description$value,
                                              v = structure_overview$structure$codelists$codelist[[1]]$code[[j]]$value)
                      }
                  } else{
                  #IF DIMENSION VALUES ARE NOT INTEGERS, CONVERT TO INTEGER
                      d_rows <- data.frame()
                
                      for (j in c(1:length(structure_overview$structure$codelists$codelist[[1]]$code))) {
                          d_row <- data.frame(dn = dimension_name,
                                             n = structure_overview$structure$codelists$codelist[[1]]$code[[j]]$description$value,
                                             v = as.integer(structure_overview$structure$codelists$codelist[[1]]$code[[j]]$value))
                          d_rows <- dplyr::bind_rows(d_rows, d_row)
                      }
                  }
              }
          } else{
              #IF THE LENGTH OF THE STRUCTURE IS 1, IT IS PROCESSED HERE
              if (is.null(dimensions_overview[[i]]$codes$code$value)) {
                  d_rows <- data.frame()
            
                  for (j in c(1:length(dimensions_overview[[i]]$codes$code))) {
                      d_row <- data.frame(dn = dimension_name,
                                         n = dimensions_overview[[i]]$codes$code[[j]]$name,
                                         v = dimensions_overview[[i]]$codes$code[[j]]$value)
                      d_rows <- dplyr::bind_rows(d_rows, d_row)
                  }
               } else{
                   d_rows <- data.frame(dn = dimension_name,
                                       n = dimensions_overview[[i]]$codes$code$name,
                                       v = dimensions_overview[[i]]$codes$code$value)
               }
            }
        }
    }
    
#COMBINE DIMENSION NAMES AND VALUES
    
    d_row_list <- list(d_rows)  
    names(d_row_list) <- dimension_name
    output <- c(output,d_row_list)
  }
  output <- do.call(rbind.data.frame, output)
  rownames(output) <- NULL
  return(output)
}

