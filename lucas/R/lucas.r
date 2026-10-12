utils::globalVariables(c("data_dir","output_dir", "input_dir", "mappings_csv_folder"))

#' @title Conntect to DB
#' @description connect to the db where you want to upload all LUCAS points
#' @param user Character. User of the database
#' @param host Character. Host of the DB
#' @param port Integer. Port to connect to usually 5432
#' @param password Character. Password to access to the DB
#' @param dbname Character
#' @examples \dontrun{
#' con <- Connect_to_db("andrrap", "localhost", 5432,"andrrap","andrrap")
#' con <- Connect_to_db("martlur", "/var/run/postgresql", 5432,"martlur","postgres")
#' con <- Connect_to_db("postgres", "172.15.0.10", 5432,"test","postgres")}
#' @return conection to the db
#' @importFrom DBI dbDriver
#' @import RPostgreSQL
#' @export
Connect_to_db <- function(user, host, port,
                          password, dbname){
  # library(RPostgreSQL)
  # library(rpostgis)
  # credential for connection
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user=user, host=host, port=port, password=password,dbname=dbname)
  return(con)

}

#' @title Assert files
#' @description Check that the user has downoad all the files needed
#' @param data_dir Character. Folder where you saved all the micro data downloaded from EUROSTAT
#' @return Nothing if OK error if failed
#' @export
Assert_files  <- function(data_dir){
  lucas2006 <- c('BE_2006_0.xls', 'CZ_2006_0.xls','DE_2006_0.xls', 'ES_2006_0.xls',
                 'FR_2006_0.xls','HU_2006_0.xls','IT_2006_0.xls','LU_2006_0.xls','NL_2006_0.xls','PL_2006_0.xls','SK_2006_0.xls')
  lucas2009_2018 <- c('EU_2009_20200213.CSV','EU_2012_20200213.CSV','EU_2015_20200225.CSV', 'EU_2018_20200213.CSV','OutScope_2015_20200225.CSV')
  #c('EU-2012-20161019.csv','EU_2018_190611.csv','EU23_2009_20161125.csv','EU28_2015_20180724.csv')

  listcsv <- list.files(path = data_dir, pattern = '.CSV')

  asser = lucas2009_2018 %in% listcsv

  if(length(asser[asser==TRUE]) != length(lucas2009_2018)){
    che= match(FALSE,asser)
    miss = lucas2009_2018[che]
    stop(paste0('You are missing this file: ', miss))
  } else {
    return(TRUE)
  }

  listxls <- list.files(path = data_dir, pattern = '.xls')
  asser = lucas2006 %in% listxls

  if(length(asser[asser==TRUE]) != length(lucas2006) ){
    che= match(FALSE,asser)
    miss = lucas2006[che]
    stop(paste0('You are missing this file: ', miss))
  } else {
    return(TRUE)
  }

}

#' @title Harmonize long values in all tables
#' @describeIn Correct the long values of 2009 data by applying th_ew to th_long and erase this column
#' @param lucas2009 Dataframe with the 2009 data
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Correct_long(con)}
#' @export
Correct_long <- function(lucas2009){
  # library(RPostgreSQL)
  # library(rpostgis)
  #Calculate th_long from th_ew and drop the this last column
  ind_ew <- grep("W", lucas2009$th_ew)
  #if the long value is not already negative add it
  lucas2009[ind_ew, ]$th_long <- sub("^", "-", lucas2009[ind_ew, ]$th_long)
  #changed now drop table
  lucas2009$th_ew <- NULL
  return(lucas2009)

}

#' @title Update csv to database
#' @description Upload to the DB all the 2009-2018 lucas csv downloaded from : \url{https://ec.europa.eu/eurostat/web/lucas/data/primary-data} there should be:
#' EU_2012_20200213.CSV
#' EU_2018_20200213.CSV
#' OutScope_2015_20200225.CSV
#' EU_2009_20200213.CSV
#' EU_2015_20200225.CSV
#' For 2006 it first combines them into one dataset for the entire year comprising of:
#' BE_2006_0.xls
#' CZ_2006_0.xls
#' DE_2006_0.xls
#' ES_2006_0.xls
#' FR_2006_0.xls
#' HU_2006_0.xls
#' IT_2006_0.xls
#' LU_2006_0.xls
#' NL_2006_0.xls
#' PL_2006_0.xls
#' SK_2006_0.xls
#' @param data_dir Character. Folder where you saved all the micro data downloaded from EUROSTAT
#' @param con PosGresSQLConnection Object.
#' @examples \dontrun{
#' Upload_to_db('/data/LUCAS_harmo/data/input', con)}
#' @seealso To create the conection please see
#' lucas]{Connect_to_db}
#' @seealso To assert that you have the files \link[lucas]{Assert_files}
#' @return Boolean. True if the update to the DB worked FALSE otherwise
#' @import RPostgreSQL
#' @import plyr
#' @import utils read.csv
#' @import utils tail
#' @export
Upload_to_db <- function(data_dir, con){
  # library(RPostgreSQL)
  # library(rpostgis)
  # library(plyr)
  if(Assert_files(data_dir)){
    # credential for connection
    listcsv <- list.files(path = data_dir, pattern = '.CSV')
    lucas_2015 <- data.frame(stringsAsFactors=FALSE)

    for (i in listcsv){
      aux = gregexpr(pattern ='20', i)[[1]][1]
      year <- substr(i, aux, aux+3)
      csv <- read.csv(file.path(data_dir,i))
      df <- as.data.frame(csv,stringsAsFactors=FALSE)
      colnames(df) <- tolower(colnames(df))
      df <- as.data.frame(apply(df,2,function(x)gsub(' $', '', x)), stringsAsFactors=FALSE)
      df <- as.data.frame(apply(df,2,function(x)gsub('^ ', '', x)), stringsAsFactors=FALSE)

      if (sum(is.na(tail(df,1))) > 0){
        #erase the last line in the df (error on the data adding char at the end)
        df = df[-nrow(df), ]
      }

      #handle 2015 Outscopefile
      if (year == 2015){
        lucas_2015 <- rbind(lucas_2015, df)
      }else if (year == 2009){
        #handle long values of the 2009 data
        df = Correct_long(df)
        dbWriteTable(con, c("public", paste0('lucas_', year)), value = df, row.names = FALSE)
      }else{
        dbWriteTable(con, c("public", paste0('lucas_', year)), value = df, row.names = FALSE)
      }
    }
    #upload 2015 data on the db
    dbWriteTable(con, c("public", 'lucas_2015'), value = lucas_2015, row.names = FALSE)

    listxls <- list.files(path = data_dir, pattern = '.xls')
    lucas_2006 <- data.frame(stringsAsFactors=FALSE)
    for (j in listxls){
      csv <- read.csv(file.path(data_dir, j))
      lucas_2006 <- rbind(lucas_2006, csv)
    }
    colnames(lucas_2006) <- tolower(colnames(lucas_2006))
    lucas_2006 <- lucas_2006[lucas_2006$surv_date != '88/88/2006',]
    dbWriteTable(con, c("public", "lucas_2006"), value = lucas_2006, row.names = FALSE)}
}

#' @title Upload_exif
#' @describeIn Upload to DB the exif information of LUCAS
#' @param con Connection to database
#' @param exif the actual EXIF cvs located in mappings
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_photo_fields_2006n(con)}
#' @import RPostgreSQL
#' @import utils read.csv
#' @export
Upload_exif <- function(con,exif){
  # library(RPostgreSQL)
  # library(rpostgis)
  message("Uploading EXIF information to the DB")
  exif_info <- read.csv(exif)
  dbWriteTable(con, c("public", "lucas_exif"), value = exif_info, row.names = FALSE)
  message("DONE")

}

#' @title Rename columns to match 2018 survey
#' @description Columns with different names between the surveys must be made to fit the last survey before merge
#' @param con Connection the database
#' @param csv CSV file with the relevant column name mappings
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Rename_cols(con, '/data/LUCAS_harmo/data/mappings/columnRename.csv')}
#' @import RPostgreSQL
#' @import utils read.csv
#' @export
Rename_cols <- function(con, csv){
  # library(RPostgreSQL)
  # library(rpostgis)

  if(!file.exists(csv)){
    stop(message("Mapping CSV to rename columns does not exist in directory."))
  }else{
    colRenameAll <- read.csv(csv)

    for(i in unique(colRenameAll$year)){
      message(i)

      # defaultW <- getOption("warn")
      # options(warn = -1)

      q <-dbSendQuery(con, paste0("SELECT * FROM ", paste0("lucas_", i)))
      lucas <- fetch(q,n = Inf)

      # options(warn = defaultW)

      colsToRename <- colRenameAll[colRenameAll$year == i,]
      colsToRename$old.name <- as.character(colsToRename$old.name)
      colsToRename$new.name <- as.character(colsToRename$new.name)

      for(j in 1:nrow(colsToRename)){
        dbSendQuery(con, paste0("ALTER TABLE ", paste0("lucas_", i), " RENAME COLUMN ", colsToRename$old.name[j], " TO ", colsToRename$new.name[j]))
      }
    }
  }

}


#' @title Add photo fields 2006
#' @describeIn Adds missing columns photo_n/e/s/w in 2006 data from the information of the exif DB
#' @param con Connection to database
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_photo_fields_2006n(con)}
#' @import RPostgreSQL
#' @export
Add_photo_fields_2006 <- function(con){
  # library(RPostgreSQL)
  # library(rpostgis)
  message('Adding photo_field to 2006 data')
  q <-dbSendQuery(con, "create temp table e2006agg as
                        select distinct(pointid), array_agg(ptype) as ptypes from lucas_exif where year = '2006' group by pointid;

                        alter table e2006agg
                        add column photo_north int,
                        add column photo_south int,
                        add column photo_east int,
                        add column photo_west int,
                        add column photo_point int;

                        update e2006agg
                        set photo_north = case when 'N' = any(ptypes) then 1 else 2 end,
                            photo_south = case when 'S' = any(ptypes) then 1 else 2 end,
                        	photo_east = case when 'E' = any(ptypes) then 1 else 2 end,
                        	photo_west = case when 'W' = any(ptypes) then 1 else 2 end,
                        	photo_point = case when 'P' = any(ptypes) then 1 else 2 end;

                        alter table e2006agg drop column ptypes;

                        alter table public.lucas_2006
                        add column photo_north int,
                        add column photo_south int,
                        add column photo_east int,
                        add column photo_west int,
                        add column photo_point int;

                        update public.lucas_2006
                        set photo_north = e.photo_north from e2006agg e where point_id = e.pointid;
                        update public.lucas_2006
                        set photo_south = e.photo_south from e2006agg e where point_id = e.pointid;
                        update public.lucas_2006
                        set photo_east = e.photo_east from e2006agg e where point_id = e.pointid;
                        update public.lucas_2006
                        set photo_west = e.photo_west from e2006agg e where point_id = e.pointid;
                        update public.lucas_2006
                        set photo_point = e.photo_point from e2006agg e where point_id = e.pointid;")
  message('DONE')
}

#' @title Add missing columns
#' @describeIn Adds missing columns to all tables before merge
#' @param con Connection to database
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_missing_cols(con, c(2006, 2009, 2012, 2015, 2018))}
#' @import RPostgreSQL
#' @export
Add_missing_cols <- function(con, years){
  # library(RPostgreSQL)
  # library(rpostgis)

  refYear <- years
  for(i in refYear){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <-dbSendQuery(con, paste0("SELECT * FROM lucas_",i,";"))
    dfRef <- fetch(q,n = Inf)

    # options(warn = defaultW)

    for(l in years){
      if(i != l){

        # defaultW <- getOption("warn")
        # options(warn = -1)

        q <-dbSendQuery(con, paste0("SELECT * FROM ", paste0("lucas_", l)))
        lucas <- fetch(q,n = Inf)

        # options(warn = defaultW)

        for(j in colnames(dfRef)[!colnames(dfRef) %in% colnames(lucas)]){
          dbSendQuery(con, paste('ALTER TABLE', paste0('lucas_', l) ,'ADD COLUMN', j, 'CHARACTER VARYING'))
        }
      }
    }
  }

  #checks
  dfcheck <- c()
  for(ii in refYear){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <-dbSendQuery(con, paste0("select count(*) from information_schema.columns where table_name='lucas_",ii,"'",";"))
    dfcheckInt <- fetch(q,n = Inf)

    # options(warn = defaultW)

    dfcheck <- unlist(c(dfcheck, dfcheckInt))
  }
  if((length(unique(dfcheck))==1)==T){
    message(TRUE)
  }else{
    stop(message('ERROR: Adding missing columns failed. Look at source data!'))
  }
}

#' @title Add new columns to tables
#' @description Adds new columns to all table that will be necessary for when tables are merged. Includes
#' letter group - first level of LUCAS land cover/land use classification system
#' year - year of survey
#' file_path_gisco_n/s/e/w/p - file path to full HD images on ESTAT GISCO cloud service for North, South, East, West, and Point images
#' @param con Connection to db
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_new_cols(con, c(2006, 2009, 2012, 2015, 2018))}
#' @import RPostgreSQL
#' @export
Add_new_cols <- function(con, years){
  # library(RPostgreSQL)
  # library(rpostgis)

  for(i in years){
    dbSendQuery(con, paste0("ALTER TABLE lucas_",i," ADD COLUMN letter_group CHARACTER VARYING;"))
    dbSendQuery(con, paste0("UPDATE lucas_",i," SET letter_group = LEFT(lc1, 1);"))

    dbSendQuery(con, paste0("ALTER TABLE lucas_",i," ADD COLUMN year CHARACTER VARYING;"))
    dbSendQuery(con, paste0("UPDATE lucas_",i," SET year = ", i, ";"))

    cardDir <- c('north', 'south', 'east', 'west', 'point')
    for(j in cardDir){
      dbSendQuery(con, paste0("ALTER TABLE lucas_",i," ADD COLUMN file_path_gisco_", j," CHARACTER VARYING;"))
      dbSendQuery(con, paste0("UPDATE lucas_",i," SET file_path_gisco_",j," = concat('https://gisco-services.ec.europa.eu/lucas/photos/',
                              year, '/' ,
                              nuts0, '/',
                              substring(point_id::VARCHAR from 1 for 3), '/',
                              substring(point_id::VARCHAR from 4 for 3), '/',
                              point_id, '",toupper(substr(j, 1,1)),".jpg' )
                              where photo_",j," = '1';"))
    }
  }
}


#' @title Upper case columns
#' @description Convert values in designated columns (lc1, lc1_spec, lu1, lu1_type, lc2, lc2_spec, lu2, lu2_type, cprn_lc) to uppercase for consistency's sake
#' @param con Connection to db
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Upper_case(con,c(2009, 2012, 2015, 2018))}
#' @import RPostgreSQL
#' @export
Upper_case <- function(con, years){
  # library(RPostgreSQL)
  # library(rpostgis)

  vars <- c("lc1", "lc1_spec", "lu1", "lu1_type", "lc2", "lc2_spec", "lu2", "lu2_type", "cprn_lc")

  for(i in years){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <-dbSendQuery(con, paste0("SELECT * FROM ", paste0("lucas_", i)))
    lucas <- fetch(q,n = Inf)

    # options(warn = defaultW)

    for(j in vars){
      if(j %in% colnames(lucas)){
        dbSendQuery(con, paste0("UPDATE lucas_",i," SET ",j," = UPPER(", j,");"))
      }
    }
  }
}


#' @title Update values to fit 2018
#' @description Updates values in all tables to fit the last survey (2018) in terms of the coding of different variables; update is based on pre-made mappings
#' @param con Connection to db
#' @param csv CSV file that holds the pre-prepared variable mappings
#' @param years Numeric vector of years to be harmonised
#' @param nonHarmonizeableVars Characer vector of variables that cannot be harmonized between the years. These pertain to the variables which have been collected at the earlier stages (before 2018) of survey as ordered categorical variables, and at later stages (at and later than (?) 2018) - as discrete numbers. Such attributes are lc1/2_perc, lu1/2_perc and soil_stones_perc.
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Recode_vars(con, '/data/LUCAS_harmo/data/mappings/RecodeVars.csv', c(2006, 2009, 2012, 2015, 2018))}
#' @import RPostgreSQL
#' @import utils read.csv
#' @export
Recode_vars <- function(con, csv, years, nonHarmonizeableVars){
  # library(RPostgreSQL)
  # library(rpostgis)

  years <- years
  for(year in years){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <- dbSendQuery(con, paste0('SELECT * FROM lucas_', year))
    lucas <- fetch(q, n=Inf)

    # options(warn = defaultW)

    for(i in 1:ncol(lucas)){
      dbSendQuery(con, paste0('ALTER TABLE lucas_',year," ALTER COLUMN ",colnames(lucas)[i]," SET DATA TYPE CHARACTER VARYING USING ",colnames(lucas)[i],"::CHARACTER VARYING;" ))
    }
  }

  updateValsCSV <- read.csv(csv, stringsAsFactors = F)

  for(i in 1:nrow(updateValsCSV)){
    if(updateValsCSV$var[i] %in% nonHarmonizeableVars & updateValsCSV$year[i] == 2018){
      if(is.na(updateValsCSV$whereClause[i])){
        if(updateValsCSV$var[i] == 'lc1_perc'){
          dbSendQuery(con, paste0("UPDATE lucas_",updateValsCSV$year[i],
                                  " SET ",updateValsCSV$var[i],
                                  " = ",updateValsCSV$newVal[i],
                                  " WHERE ",updateValsCSV$var[i],"::int ",
                                  updateValsCSV$oldVal[i],";"))
        }else{
          dbSendQuery(con, paste0("UPDATE lucas_",updateValsCSV$year[i],
                                  " SET ",updateValsCSV$var[i],
                                  " = ",updateValsCSV$newVal[i],
                                  " WHERE ",updateValsCSV$var[i],"::int ",
                                  " = ",updateValsCSV$oldVal[i],";"))
        }

      }else{
        dbSendQuery(con, paste0("UPDATE lucas_",updateValsCSV$year[i],
                                " SET ",updateValsCSV$var[i],
                                " = ",updateValsCSV$newVal[i],
                                " WHERE ",updateValsCSV$var[i],"::int ",
                                updateValsCSV$oldVal[i],
                                " AND ",updateValsCSV$var[i],"::int ",updateValsCSV$whereClause[i],";"))
      }

    }else{
      dbSendQuery(con, paste0("UPDATE lucas_",updateValsCSV$year[i],
                              " SET ",updateValsCSV$var[i],
                              " = ",updateValsCSV$newVal[i],
                              " WHERE ",updateValsCSV$var[i],
                              " = ", updateValsCSV$oldVal[i],";"))
    }
  }
}



#' @title Change column order
#' @description Changes order of columns to fit the last survey (2018) and set all column data type to character varying in order to prepare for merge
#' @param con Connection to db
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Order_cols(con, c(2006, 2009, 2012, 2015))}
#' @import RPostgreSQL
#' @export
Order_cols <- function(con, years){
  # library(RPostgreSQL)
  # library(rpostgis)

  # defaultW <- getOption("warn")
  # options(warn = -1)

  q <-dbSendQuery(con, "SELECT * FROM lucas_2018")
  lucas_ref <- fetch(q,n = Inf)

  # options(warn = defaultW)

  for(i in years){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <-dbSendQuery(con, paste0("SELECT * FROM ", paste0("lucas_", i)))
    lucas <- fetch(q,n = Inf)

    # options(warn = defaultW)

    lucas <- lucas[names(lucas_ref)]

    if(all(colnames(lucas) == colnames(lucas_ref)) & all(colnames(lucas_ref) == colnames(lucas))){
      message(paste("Table lucas_",i, ": all columns match - " ,TRUE, '; \n'))
    }

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <- dbSendQuery(con, paste0("SELECT *
                             FROM information_schema.columns
                     WHERE table_schema = 'public'
                     AND table_name   = 'lucas_",i,"'
                     ;"))
    harmoCols <- fetch(q, n=Inf)

    # options(warn = defaultW)

    for(col in 1:nrow(harmoCols)){
      #message(harmoCols$column_name[col])
      dbSendQuery(con, paste0("UPDATE lucas_",i," SET ",harmoCols$column_name[col]," = Null WHERE ",harmoCols$column_name[col]," = 'NA';"))
      dbSendQuery(con, paste0("UPDATE lucas_",i," SET ",harmoCols$column_name[col]," = Null WHERE ",harmoCols$column_name[col],"= ' ';"))
      dbSendQuery(con, paste0("UPDATE lucas_",i," SET ",harmoCols$column_name[col]," = Null WHERE ",harmoCols$column_name[col],"= '';"))
    }

    dbSendQuery(con, paste0('DROP TABLE lucas_', i, ';'))
    dbWriteTable(con, c("public", paste0("lucas_", i)), value = lucas, row.names = FALSE)
  }
}

#' @title Merge all tables
#' @description Merge all tables into a single harmonized version containing all years and change to relevant data type, as mapped in the record descriptor
#' @param con Connection to db
#' @param rd Record descriptor in CSV format
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Merge_harmo(con, '/data/LUCAS_harmo/data/supportDocs/LUCAS_harmo_RD.csv')}
#' @import RPostgreSQL
#' @import utils read.csv
#' @export
Merge_harmo <- function(con, rd){
  # library(RPostgreSQL)
  # library(rpostgis)

  # defaultW <- getOption("warn")
  # options(warn = -1)

  q <- dbSendQuery(con, "with l2006 as(select count(*) as cols2006 from information_schema.columns where table_name = 'lucas_2006'),
              l2009 as (select count(*) as cols2009 from information_schema.columns where table_name = 'lucas_2009'),
              l2012 as (select count(*) as cols2012 from information_schema.columns where table_name = 'lucas_2012'),
              l2015 as (select count(*) as cols2015 from information_schema.columns where table_name = 'lucas_2015'),
              l2018 as (select count(*) as cols2018 from information_schema.columns where table_name = 'lucas_2018')

              select * from l2006, l2009, l2012, l2015, l2018;")
  df_check <- fetch(q, n = Inf)

  # options(warn = defaultW)

  df_check <- t(df_check)
  if(length(unique(df_check[,1])) == 1){
    message(paste0('All tables have ',unique(df_check[,1]),' columns. Tables ready to be merged. \n'))

    dbSendQuery(con, 'CREATE TABLE lucas_harmo_pack AS
                      SELECT * FROM lucas_2018
                      UNION ALL
                      SELECT * from lucas_2015
                      UNION ALL
                      SELECT * from lucas_2012
                      UNION ALL
                      SELECT * from lucas_2009
                      UNION ALL
                      SELECT * from lucas_2006;')

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <- dbSendQuery(con, 'SELECT * FROM pg_catalog.pg_tables;')
    tables <- fetch(q, n=Inf)

    # options(warn = defaultW)

    if('lucas_harmo_pack' %in% tables$tablename){
      dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack ADD COLUMN id SERIAL;')
      recordDescriptor <- read.csv(rd)
      recordDescriptor$variable <- as.character(recordDescriptor$variable)
      for(varr in 1:nrow(recordDescriptor)){
        if(recordDescriptor$variable[varr] != "survey_date"){
          if(recordDescriptor$variable[varr] != "revisit"){
            #message(recordDescriptor$variable[varr])
            dbSendQuery(con, paste0('ALTER TABLE lucas_harmo_pack ALTER COLUMN ',recordDescriptor$variable[varr],
                                    " SET DATA TYPE ",recordDescriptor$type[varr],
                                    " USING ",paste0(recordDescriptor$variable[varr], '::', recordDescriptor$type[varr], ';')))
          }

          # }else {dbSendQuery(con, 'SET DateStyle TO dmy;')
          #   dbSendQuery(con, "ALTER TABLE lucas_harmo_pack ALTER COLUMN survey_date TYPE DATE USING to_date(survey_date, 'DD/MM/YY');")
        }
      }
      message('Table lucas_harmo_pack has successfully been added. \n')
    }else{
      stop('Error: Table lucas_harmo_pack not created. ')
    }

  } else {
    stop('Tables have an unequal number of columns. Unable to merge. ')
  }
}

#' @title Consistency checks
#' @description Perform consistency checks on newly created tables to ensure conformity in terms of column order and data types
#' @param con Connection to db
#' @param years Numeric vector of years to be harmonised
#' @param manChangedVars File path to csv of attributes and relevant years to which manual manipulation has been done and thus cannot clear a consistency of values check
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Consistency_check(con, c(2006, 2009, 2012, 2015, 2018))}
#' @import DBI dbExecute
#' @import utils read.csv
#' @export
Consistency_check <- function(con, years, manChangedVars){
  # library(RPostgreSQL)
  # library(rpostgis)

  if(file.exists(file.path(mappings_csv_folder, 'manChangedVars.csv'))==FALSE){

    stop('Error: CSV of manually changed variables file is not in input folder. Please place file in input folder!')

  }else{

    manChangedVars <- read.csv(manChangedVars)
    years <- years
    colnames(manChangedVars) <- c('varChanged', years)
    for(i in years){

      #take care of pesky date TODO: has to be diff for 2006 and for the other!!!!
      # dbExecute(con, paste0("ALTER TABLE lucas_",i,"
      #                       ALTER COLUMN survey_date
      #                       SET DATA TYPE DATE
      #                       USING TO_DATE(survey_date, 'DD/MM/YY')::DATE;"))


      # defaultW <- getOption("warn")
      # options(warn = -1)

      q <- dbSendQuery(con, paste0("SELECT table_name, column_name, data_type
                             FROM information_schema.columns
                     WHERE table_schema = 'public'
                     AND table_name   = 'lucas_",i,"'
                     ;"))
      lucasCols_base <- fetch(q, n=Inf)

      q <- dbSendQuery(con, paste0("SELECT table_name, column_name, data_type
                             FROM information_schema.columns
                     WHERE table_schema = 'public'
                     AND table_name   = 'lucas_harmo_pack';"))
      lucasCols_harmo <- fetch(q, n=Inf)

      # options(warn = defaultW)

      lucasCols <- merge(lucasCols_base, lucasCols_harmo, by = 'column_name', all.x = T)

      for(coll in 1:nrow(lucasCols)){
        #Get the table
        if(any(lucasCols$column_name[coll] %in% manChangedVars$varChanged)){
          manChangedVars_int <- manChangedVars[manChangedVars$varChanged == lucasCols$column_name[coll],]
          yearManipulated <- colnames(manChangedVars_int[which(t(manChangedVars_int) == T)])
          if(yearManipulated == i){
            message(paste0("Column ",lucasCols$column_name[coll]," from year ",yearManipulated," has been manually manipulated. Skipping consistency check.", '\n'))
          }
        }else{
          # defaultW <- getOption("warn")
          # options(warn = -1)

          q <-dbSendQuery(con, paste0("select distinct(",
                                      lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],
                                      "), count(*)
                                  from lucas_",i,"
                                  group by ",lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],
                                      " order by ",lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],";"))
          dfSingle <- fetch(q,n = Inf)

          # options(warn = defaultW)

          # defaultW <- getOption("warn")
          # options(warn = -1)

          q <-dbSendQuery(con, paste0("select distinct(",
                                      lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],
                                      "), count(*)
                                  from lucas_harmo_pack
                                  where year = ",i,"
                                  group by ",lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],
                                      " order by ",lucasCols$column_name[coll],"::",lucasCols$data_type.y[coll],";"))
          dfAll <- fetch(q,n = Inf)

          # options(warn = defaultW)

          dfcompare <- merge(dfSingle, dfAll, by = lucasCols$column_name[coll])
          if(nrow(dfcompare) == 0){
            stop(paste0('Error: Table lucas_',i,' is NOT consistent for variable ',lucasCols$column_name[coll]), '\n')
          }else{
            wrongs <- which(dfcompare$count.x != dfcompare$count.y)

            if(length(wrongs) == 0){
              message(paste0('Table lucas_',i,' is consistent for variable ',lucasCols$column_name[coll]), '\n')
            } else{
              stop(paste0('Error: Table lucas_',i,' is NOT consistent for variable ',lucasCols$column_name[coll]), '\n')

            }
          }
        }
      }
    }
  }
}


#' @title Correct theoretical long lat
#' @description Applying a correction of the values of columns th_long and th_lat according to the latest LUCAS grid
#' @param con Connection to db
#' @param lucas_grid File path to the csv file of the latest LUCAS grid
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Correct_th_loc(con, lucas_grid)}
#' @importFrom DBI dbExecute
#' @import utils read.csv
#' @export
Correct_th_loc <- function(con, lucas_grid){

  #upload grid to database
  if(file.exists(file.path(input_dir, 'GRID_CSVEXP_20171113.csv'))==FALSE){
    stop('Error: Grid CSV file is not in input folder. Please place file in input folder!')
  }else{
    l_grid <- read.csv(lucas_grid)
    colnames(l_grid) <- tolower(colnames(l_grid))
    dbWriteTable(con, c("public", "lucas_grid"), value = l_grid, row.names = FALSE)
    message("DONE")

    # library(RPostgreSQL)
    # library(rpostgis)

    dbExecute(con, 'update lucas_harmo_pack set th_lat = y_wgs84 from lucas_grid where lucas_harmo_pack.point_id = lucas_grid.point_id')
    dbExecute(con, 'update lucas_harmo_pack set th_long = x_wgs84 from lucas_grid where lucas_harmo_pack.point_id = lucas_grid.point_id')

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <-dbSendQuery(con, paste0("select lucas_harmo_pack.point_id as h_point_id, th_lat, th_long, lucas_grid.point_id as g_point_id, x_wgs84, y_wgs84 FROM lucas_harmo_pack JOIN lucas_grid on lucas_harmo_pack.point_id = lucas_grid.point_id"))
    dfthLocUpdate <- fetch(q,n = Inf)

    # options(warn = defaultW)

    if(all(dfthLocUpdate$th_lat == dfthLocUpdate$y_wgs84) & all(dfthLocUpdate$th_long == dfthLocUpdate$x_wgs84)){
      message('All values for th_lat and th_long have been successfuly hard-coded to fit the specified LUCAS grid')
    }else{
      stop(message('Error:Recoding failed check source tables'))
    }
  }
}


#' @title Add geometries and calculated distance
#' @description Add geometries to lucas harmonized table:
#' - location of theoretical point(th_geom) from fields th_long, the_lat
#' - location of lucas survey (gps_geom) from fields gps_long, gps_lat
#' - lucas transect geometr (trans_geom) from fields gps_long, gps_lat
#' - distance between theoretical and survey point (th_gps_dist)
#' @param con Connection to db
#' @param save_dir Dirrectory where to save geometries
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_geom(con)}
#' @importFrom DBI dbExecute
#' @export
Add_geom <- function(con, save_dir){
  # library(RPostgreSQL)
  # library(rpostgis)

  #gps_geom

  dbExecute(con, 'ALTER TABLE lucas_harmo_pack ADD COLUMN gps_geom geometry(POINT, 4326);')
  # dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom = st_setsrid(st_point(gps_long::DOUBLE PRECISION ,gps_lat::DOUBLE PRECISION),4326)
  #                   WHERE gps_lat::DOUBLE PRECISION != 88.888888
  #                   AND gps_ew IS NOT NULL
  #                   AND gps_ew != 8;')
  #
  # dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom = st_setsrid(st_point(-gps_long::DOUBLE PRECISION,gps_lat::DOUBLE PRECISION),4326)
  #                   WHERE gps_ew = 2
  #                   AND gps_lat::DOUBLE PRECISION != 88.888888
  #                   AND gps_ew IS NOT NULL
  #                   AND gps_ew != 8;')

  # if gps_ew is not 8 and not null : the longitude (gps_long) could be used as it is
  dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom = st_setsrid(st_point(gps_long::DOUBLE PRECISION ,gps_lat::DOUBLE PRECISION),4326)
                    WHERE gps_lat::DOUBLE PRECISION < 88
                    AND gps_lat::DOUBLE PRECISION > 0
                    AND gps_long::DOUBLE PRECISION < 88
                    AND gps_long::DOUBLE PRECISION > 0
                    AND gps_ew IS NOT NULL
                    AND gps_ew = 1;')
  #TODO: set here the flag for the longs that are negatives!!!
  # if gps_ew is 2 (mean west looking) : the longitude sign (gps_long) should be changed
  dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom = st_setsrid(st_point(-gps_long::DOUBLE PRECISION,gps_lat::DOUBLE PRECISION),4326)
                    WHERE gps_ew = 2
                    AND gps_ew IS NOT NULL
                    AND gps_long::DOUBLE PRECISION > 0
                    AND gps_lat::DOUBLE PRECISION > 0
                    AND gps_lat::DOUBLE PRECISION < 88
                    AND gps_long::DOUBLE PRECISION < 88;')

  # if gps_ew is NULL and the sign of lat and long are the same between th and gps, set gps_geom as gps_long gps_lat
  dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom =st_setsrid(st_point(gps_long::DOUBLE PRECISION ,gps_lat::DOUBLE PRECISION),4326)
                    WHERE gps_ew IS NULL
                    AND gps_lat::DOUBLE PRECISION < 88 AND gps_long::DOUBLE PRECISION < 88
                    AND ((th_long::DOUBLE PRECISION < 0 AND gps_long::DOUBLE PRECISION <0) OR (th_long::DOUBLE PRECISION > 0 AND gps_long::DOUBLE PRECISION >0));')

  # if gps_ew is NULL and the sign of lat and long differs between th and gps, set gps_geom as NULL
  dbExecute(con,'UPDATE lucas_harmo_pack SET gps_geom =NULL
              WHERE gps_ew IS NULL
              AND gps_lat::DOUBLE PRECISION < 88 AND gps_long::DOUBLE PRECISION < 88
              AND ((th_long::DOUBLE PRECISION < 0 AND gps_long::DOUBLE PRECISION >0) OR (th_long::DOUBLE PRECISION > 0 AND gps_long::DOUBLE PRECISION <0) )
              OR gps_long::DOUBLE PRECISION = 8.000000 OR gps_lat::DOUBLE PRECISION = 8.000000;')

  #th_geom
  dbExecute(con, 'ALTER TABLE lucas_harmo_pack ADD COLUMN th_geom geometry(POINT, 4326);')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET th_geom = st_setsrid(st_point(th_long::DOUBLE PRECISION,th_lat::DOUBLE PRECISION),4326) WHERE year::int != 2006;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET th_geom = st_setsrid(st_point(th_long::DOUBLE PRECISION,th_lat::DOUBLE PRECISION), 4326) WHERE year::int = 2006 AND th_lat::decimal % 1 <> 0')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET th_geom = st_transform(st_setsrid(st_point(th_lat::DOUBLE PRECISION,th_long::DOUBLE PRECISION),3035), 4326) WHERE year::int = 2006 AND th_lat::decimal % 1 = 0;')

  #th_gps_dist
  dbExecute(con, 'ALTER TABLE lucas_harmo_pack ADD COLUMN th_gps_dist numeric;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET th_gps_dist = ST_Distance(geography(th_geom), geography(gps_geom));')

  #obs_dist 2006
  dbExecute(con, 'UPDATE lucas_harmo_pack SET obs_dist = 1.5 WHERE year::int = 2006 AND obs_dist = 1;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET obs_dist = 24.5 WHERE year::int = 2006 AND obs_dist = 2;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET obs_dist = 75 WHERE year::int = 2006 AND obs_dist = 3;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET obs_dist = 101 WHERE year::int = 2006 AND obs_dist = 4;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET obs_dist = th_gps_dist WHERE year::int = 2006 AND gps_geom IS NOT NULL;')

  #remove geometries of extreme innacuracy
  dbExecute(con, 'UPDATE lucas_harmo_pack SET gps_geom = NULL WHERE th_gps_dist > 999999;')
  dbExecute(con, 'UPDATE lucas_harmo_pack SET th_gps_dist = NULL WHERE th_gps_dist > 999999;')

  #trans_geom
  dbExecute(con, "SELECT AddGeometryColumn ('lucas_harmo_pack','trans_geom', 3035,'LINESTRING',2);")
  dbExecute(con, "UPDATE lucas_harmo_pack SET trans_geom = st_makeline(ST_Transform(ST_SETSRID(ST_MakePoint(th_long::DOUBLE PRECISION, th_lat::DOUBLE PRECISION), 4326), 3035),
                                               st_translate(ST_Transform(ST_SETSRID(ST_MakePoint(th_long::DOUBLE PRECISION, th_lat::DOUBLE PRECISION), 4326), 3035),
                                                            250.0*sin(radians(90.0)), 250.0*cos(radians(90.0))))
                    WHERE th_lat::DOUBLE PRECISION < 88
                    AND th_long::DOUBLE PRECISION < 88
                    AND transect IS NOT NULL;")

  #system(paste("pgsql2shp -f", file.path(save_dir, "LUCAS_gps_geom.shp"), "-h "172.15.0.10 -u postgres -P test postgres "SELECT id, gps_geom FROM lucas_harmo_pack;"))
  #system(pgsql2shp -f /data/data_vol/LUCAS/LucasPackage/package/geoms/LUCAS_th_geom.shp -h 172.15.0.10 -u postgres -P test postgres "SELECT id, th_geom FROM lucas_harmo_pack;")
  #system(pgsql2shp -f /data/data_vol/LUCAS/LucasPackage/package/geoms/LUCAS_trans_geom.shp -h 172.15.0.10 -u postgres -P test postgres "SELECT id, transect, trans_geom FROM lucas_harmo_pack;")
}


#' @title Create tags for harmonized table
#' @description Create database tags (primary key), index, and spatial index and a new id column for the harmonized table
#' @param con Connection to db
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Create_tags(con)}de
#' @import RPostgreSQL
#' @export
Create_tags <- function(con){
  # library(RPostgreSQL)
  # library(rpostgis)

  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack DROP COLUMN id;')
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack ADD COLUMN id SERIAL;')
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack ADD CONSTRAINT pointid_year_key UNIQUE (point_id, year);')
  dbSendQuery(con, 'CREATE UNIQUE INDEX lucas_harmo_pack_idx ON lucas_harmo_pack (id);')
  dbSendQuery(con, 'CREATE INDEX lucas_harmo_pack_sp_idx_gps ON lucas_harmo_pack USING GIST (gps_geom);')
  dbSendQuery(con, 'CREATE INDEX lucas_harmo_pack_sp_idx_th ON lucas_harmo_pack USING GIST (th_geom);')
  dbSendQuery(con, 'CREATE INDEX lucas_harmo_pack_sp_idx_trans ON lucas_harmo_pack USING GIST (gps_geom);')
  dbSendQuery(con, 'VACUUM ANALYZE;')
}



#' @title Add revisit column
#' @description Adds revisit column to lucas harmonized table to show the number of times between the years when the point was revisited.
#' @param con Connection to db
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Add_revisit(con)}
#' @import RPostgreSQL
#' @export
Add_revisit <- function(con){
  # library(RPostgreSQL)
  # library(rpostgis)

  #nice INSERT INTO WAY
  # dbSendQuery(con, 'CREATE TABLE revisit AS SELECT DISTINCT(point_id)::VARCHAR as idd, COUNT(DISTINCT(year)) as revisitR FROM lucas_harmo_pack GROUP BY (point_id);')
  # dbSendQuery(con, "ALTER TABLE lucas_harmo_pack ADD COLUMN revisit INTEGER;")
  # dbSendQuery(con, "INSERT INTO lucas_harmo_pack_t (revisit) (SELECT revisitR FROM revisit, lucas_harmo_pack_t WHERE lucas_harmo_pack_t.point_id::VARCHAR = revisit.idd);")
  # dbSendQuery(con, "DROP TABLE revisit;")
  #

  #stupid Momo way
  dbSendQuery(con, 'CREATE TABLE revisit AS SELECT DISTINCT(point_id)::VARCHAR as idd, COUNT(DISTINCT(year)) as revisitR FROM lucas_harmo_pack GROUP BY (point_id);')
  dbSendQuery(con, 'create table lucas_harmo_pack_r as select * from lucas_harmo_pack join revisit on lucas_harmo_pack.point_id::VARCHAR = revisit.idd;')
  dbSendQuery(con, 'drop table revisit;')
  dbSendQuery(con, 'drop table lucas_harmo_pack;')
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack_r RENAME TO lucas_harmo_pack;')
  dbSendQuery(con, 'alter table lucas_harmo_pack drop column idd;')
  dbSendQuery(con, 'alter table lucas_harmo_pack rename column revisitr to revisit;')
}





#' @title Align mapping CSVs
#' @description Corrects any typo, spelling mistake, or spelling difference in the user-created mapping CSVs, used to generate labels in subsequent User_friendly() function by alligning them to the mapping CSV of the latest survey.
#' @param mapp_csv_folder Directory where mapping files are stored
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @import utils read.csv
#' @import utils write.csv
#' @examples \dontrun{
#' Align_Map_CSVs('/data/LUCAS_harmo/data/mappings', c(2006, 2009, 2012, 2015, 2018))}
#' @export
Align_Map_CSVs <- function(mapp_csv_folder, years){

  labelCols <- c('lc1', 'lc2', 'lc1_spec', 'lc2_spec', 'lu1', 'lu2', 'lu1_type', 'lu2_type', 'cprn_lc')
  listfiles <- list.files(mapp_csv_folder)
  listfiles <- listfiles[grep('20', listfiles)]

  if(length(listfiles == length(years))){
    for(i in years){
      if(i != max(years)){
        ufcsvREF <- read.csv(file.path(mapp_csv_folder, listfiles[grep(as.character(max(years)), listfiles)]), stringsAsFactors = F)

        message(paste0("Mapping csv ", listfiles[grep(as.character(i), listfiles)]," is getting corrected. Aligning to ",listfiles[grep(as.character(max(years)),listfiles)],"."))
        ufcsv <- read.csv(file.path(mapp_csv_folder,listfiles[grep(as.character(i), listfiles)]), stringsAsFactors = F)
        ufcsv$X = NULL

        #change column type to character
        for(ufcol in colnames(ufcsv)){
          ufcsv[[ufcol]] <- as.character(ufcsv[[ufcol]])
        }


        #change LABEL of uf(mapping) csv to fit that of the reference (latest, in this case - 2018) csv
        codesNotToChange4LabelRecode <- ufcsv$code[! ufcsv$code %in% ufcsvREF$code]
        for(ii in 1:nrow(ufcsv)){
          #message(paste(ufcsv$var[ii], ufcsv$code[ii]))
          if(ufcsv$var[ii] %in% labelCols){
            if(! ufcsv$code[ii] %in% codesNotToChange4LabelRecode){
              ufcsvREFInt <- ufcsvREF[ufcsv$var[ii] == ufcsvREF$var & ufcsvREF$code == ufcsv$code[ii],]
              #message(paste(ufcsv$code[ii], ufcsv$label[ii], ufcsvREFInt$code, ufcsvREFInt$label))
              if(nrow(ufcsvREFInt) > 1){
                message(paste("ERROR: variable here specified more than once in mappings CSV. Edit mapping CSV for these lines:",ufcsvREFInt))
              }else if(nrow(ufcsvREFInt) > 0){
                ufcsv$label[ii] <- ufcsvREFInt$label
              }
            }
          }
        }

        #change CODE of uf(mapping) csv to fit that of the reference (latest, in this case - 2018) csv
        codesNotToChange4CodeRecode <- ufcsv$code[! ufcsv$label %in% ufcsvREF$label]
        for(ii in 1:nrow(ufcsv)){
          #message(paste(ufcsv$var[ii], ufcsv$code[ii]))
          if(ufcsv$var[ii] %in% labelCols){
            if(! ufcsv$code[ii] %in% codesNotToChange4CodeRecode){
              ufcsvREFInt <- ufcsvREF[ufcsv$var[ii] == ufcsvREF$var & ufcsvREF$label == ufcsv$label[ii],]
              if(nrow(ufcsvREFInt) > 1){
                message(paste("ERROR: variable here specified more than once in mappings CSV. Edit mapping CSV for these lines:",ufcsvREFInt))
              }else if(nrow(ufcsvREFInt) > 0){
                ufcsv$code[ii] <- ufcsvREFInt$code
              }
            }
          }
        }

        #AGAIN change LABEL of uf(mapping) csv to fit that of the reference (latest, in this case - 2018) csv
        codesNotToChange4LabelRecode <- ufcsv$code[! ufcsv$code %in% ufcsvREF$code]
        for(ii in 1:nrow(ufcsv)){
          #message(paste(ufcsv$var[ii], ufcsv$code[ii]))
          if(ufcsv$var[ii] %in% labelCols){
            if(! ufcsv$code[ii] %in% codesNotToChange4LabelRecode){
              ufcsvREFInt <- ufcsvREF[ufcsv$var[ii] == ufcsvREF$var & ufcsvREF$code == ufcsv$code[ii],]
              #message(paste(ufcsv$code[ii], ufcsv$label[ii], ufcsvREFInt$code, ufcsvREFInt$label))
              if(nrow(ufcsvREFInt) > 1){
                message(paste("ERROR: variable here specified more than once in mappings CSV. Edit mapping CSV for these lines:",ufcsvREFInt))
              }else if(nrow(ufcsvREFInt) > 0){
                ufcsv$label[ii] <- ufcsvREFInt$label
              }
            }
          }
        }

        write.csv(ufcsv, file.path(mapp_csv_folder,listfiles[grep(as.character(i), listfiles)]))
        message('Done.')
      }
    }
  }else{
    stop(message(paste('Number of mapping CSVs does not match number of years specified - check for missing files in',data_dir,'Note: Make sure mapping CSV containes the year of survey in numeric: e.g. 20##')))}

}



#' @title Check mapping CSVs
#' @description Consistency check for Allign_map_CSVs function and creation of a new document with the explicit legends documented in document C3 (Classification) from LUCAS surveys.
#' @param mapp_csv_folder Directory where mapping files are stored
#' @param years Numeric vector of years to be harmonised
#' @param save_dir Directory where new C3 legends will be saved. Idealy (for consistency's sake) this directory should be the support_dir from main.R script.
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @import utils read.csv
#' @import utils write.csv
#' @examples \dontrun{
#' Check_Map_CSVs('/data/LUCAS_harmo/data/mappings',c(2006, 2009, 2012, 2015, 2018),'/data/LUCAS_harmo/data/supportDocs/C3_legends_new.csv')
#' @export

Check_Map_CSVs <- function(mapp_csv_folder, years, save_dir){

  labelCols <- c('lc1', 'lc2', 'lc1_spec', 'lc2_spec', 'lu1', 'lu2', 'lu1_type', 'lu2_type', 'cprn_lc')
  listfiles <- list.files(mapp_csv_folder)
  listfiles <- listfiles[grep('20', listfiles)]

  if(length(listfiles == length(years))){

    newC3doc <- data.frame()
    for(i in years){
      ufcsvREF <- read.csv(file.path(mapp_csv_folder, listfiles[grep(as.character(max(years)), listfiles)]), stringsAsFactors = F)
      ufcsvREF$X <- NULL

      message(paste0("Checking mapping csv ", listfiles[grep(as.character(i), listfiles)]))
      ufcsv <- read.csv(file.path(mapp_csv_folder,listfiles[grep(as.character(i), listfiles)]), stringsAsFactors = F)
      ufcsv$X = NULL

      #change column type to character
      for(ufcol in colnames(ufcsv)){
        ufcsv[[ufcol]] <- as.character(ufcsv[[ufcol]])
      }

      #take only stuff that is going to be in the new C3 doc
      ufcsvc3 <- ufcsv[ufcsv$var %in% labelCols,]
      ufcsvc3$year <- i
      ufcsvREFc3 <- ufcsvREF[ufcsvREF$var %in% labelCols,]
      ufcsvREFc3$year <- max(years)

      ufcsvboth <- rbind(ufcsvc3, ufcsvREFc3)
      ufcsvbothOrd <- ufcsvboth[order(ufcsvboth$var, ufcsvboth$code), ]
      ufcsvbothOrdSingle <- ufcsvbothOrd[!duplicated(ufcsvbothOrd[c('var', 'code')]),]

      if(i == min(years)){
        newC3doc <- ufcsvbothOrdSingle
      }else{
        newC3doc <- rbind(newC3doc, ufcsvbothOrdSingle)
        newC3doc <- newC3doc[order(newC3doc$var, newC3doc$code), ]
        newC3doc <- newC3doc[!duplicated(newC3doc[c('var', 'code')]),]
      }
    }

    message('New C3 document with explicit harmonized legends created. Proceeding with consistency check.')

    newC3doc$year <- NULL
    for(i in years){
      ufcsv <- read.csv(file.path(mapp_csv_folder,listfiles[grep(as.character(i), listfiles)]), stringsAsFactors = F)
      ufcsv$X = NULL

      ufcsv <- ufcsv[ufcsv$var %in% labelCols,]
      if(length(ufcsv$code[!ufcsv$code %in% newC3doc$code])>0){
        stop(message(paste('Mapping CSV for year',i,'is not consistent for variables',paste(unique(ufcsv$var[!ufcsv$code %in% newC3doc$code]), collapse = ', '))))
      }else{
        message(paste('All code-label legend combinations for year', i, 'are consistent with the new C3 document with explicit harmonized legends', '\n'))
      }
    }

    rownames(newC3doc) <- NULL
    message('Saving new C3 document.')
    message('\n')
    write.csv(newC3doc, save_dir)
    message('Saved.')

  }else{
    stop(message(paste('Number of mapping CSVs does not match number of years specified - check for missing files in',data_dir,'Note: Make sure mapping CSV containes the year of survey in numeric: e.g. 20##')))}

}

#' @title User-friendly LUCAS harmonized
#' @description Creates columns with labels for coded variables and decodes all variables where possible to explicit labels
#' @param con Connection to db
#' @param data_dir Directory where files are stored
#' @param years Numeric vector of years to be harmonised
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' User_friendly(con, '/data/LUCAS_harmo/data/mappings', c(2006, 2009, 2012, 2015, 2018))}
#' @import RPostgreSQL
#' @import utils read.csv
#' @export
User_friendly <- function(con, data_dir, years){
  # library(RPostgreSQL)
  # library(rpostgis)

  dbSendQuery(con, paste0("CREATE TABLE lucas_harmo_pack_uf AS SELECT * FROM lucas_harmo_pack;"))
  labelCols <- c('lc1_label', 'lc2_label', 'lc1_spec_label', 'lc2_spec_label', 'lu1_label', 'lu2_label', 'lu1_type_label', 'lu2_type_label', 'cprn_lc_label')
  for(lc in labelCols){
    dbSendQuery(con, paste0("ALTER TABLE lucas_harmo_pack_uf ADD COLUMN ",lc," TEXT;"))
  }

  listfiles <- list.files(data_dir)
  years <- years

  for(i in years){
    message(paste0("the year working on: ", i))
    ufcsv <- read.csv(file.path(data_dir,listfiles[grep(as.character(i), listfiles)]))
    ufcsv$X = NULL

    #change column type to character
    for(ufcol in colnames(ufcsv)){
      ufcsv[[ufcol]] <- as.character(ufcsv[[ufcol]])
    }

    #convert all values in csv to the same case - first letter upper case, rest - lower case
    firstup <- function(x) {
      x <- tolower(x)
      substr(x, 2, 2) <- toupper(substr(x, 2, 2))
      x
    }

    varsToChangeCase <- c('lc1', 'lc2', 'lc1_spec', 'lc2_spec', 'lu1', 'lu2', 'lu1_type', 'lu2_type', 'cprn_lc')

    for(labell in 1:nrow(ufcsv)){
      if(ufcsv$var[labell] %in% varsToChangeCase){ # ufcsv$label[labell] != 'Null' & ufcsv$label[labell] != 'WGS84'
        ufcsv$label[labell] <- firstup(ufcsv$label[labell])
      }
    }

    #message(paste0('lucas_',i))
    for(j in 1:nrow(ufcsv)){
      if(ufcsv$var[j] %in% c(gsub("_label","",labelCols))){
        if(is.na(ufcsv$whereClause[j])){
          #message(paste0('1if', ufcsv$var[j], ", ", ufcsv$label[j]))
          dbSendQuery(con, paste0("UPDATE lucas_harmo_pack_uf SET ",ufcsv$var[j] ,"_label = ",ufcsv$label[j]," WHERE ",ufcsv$var[j]," = ",ufcsv$code[j]," AND year = ",i,";"))
        }else{
          #message(paste0('1else', ufcsv$var[j], ", ", ufcsv$label[j], ", ",ufcsv$whereClause[j]))
          dbSendQuery(con, paste0("UPDATE lucas_harmo_pack_uf SET ",ufcsv$var[j] ,"_label = ",ufcsv$label[j]," WHERE ",ufcsv$var[j]," = ",ufcsv$code[j]," " ,as.character(ufcsv$whereClause[j])," AND year = ",i,";"))
        }
      }else{
        if (ufcsv$var[j] == "lc1_perc"){
          #message("Im in the rror!")

        }
        dbSendQuery(con, paste0('ALTER TABLE lucas_harmo_pack_uf ALTER COLUMN ',ufcsv$var[j]," SET DATA TYPE TEXT USING ",ufcsv$var[j],"::TEXT;"))
        if(is.na(ufcsv$whereClause[j])){
          #message(paste0('2if',ufcsv$var[j], ", ", ufcsv$label[j]))
          dbSendQuery(con, paste0("UPDATE lucas_harmo_pack_uf SET ",ufcsv$var[j] ," = ",ufcsv$label[j]," WHERE ",ufcsv$var[j]," = ",ufcsv$code[j],"AND year = ",i,";"))
        }else{
          #message(paste0('2else',ufcsv$var[j], ", ", ufcsv$label[j], ", ",ufcsv$whereClause[j]))
          dbSendQuery(con, paste0("UPDATE lucas_harmo_pack_uf SET ",ufcsv$var[j] ," = ",ufcsv$label[j]," WHERE ",ufcsv$var[j]," = ",ufcsv$code[j], " " ,as.character(ufcsv$whereClause[j])," AND year = ",i,";"))
        }
      }
    }
  }
}

#' @title User-friendly consistency check
#' @description Perform consistency checks on newly created UF fields to ensure conformity in terms of column order and data types
#' @param con Connection to db
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' UF_Consistency_check(con)}
#' @import RPostgreSQL
#' @export
UF_Consistency_check <- function(con){
  # library(RPostgreSQL)
  # library(rpostgis)

  colsToCheck <- c('lc1', 'lc2', 'lc1_spec', 'lc2_spec', 'lu1', 'lu2', 'lc1_perc', 'lc2_perc', 'lu1_type', 'lu2_type', 'obs_dist', 'gps_proj', 'obs_direct',
                   'parcel_area_ha', 'tree_height_maturity','tree_height_survey', 'feature_width', 'lc_lu_special_remark', 'grazing','special_status', 'wm', 'wm_type', 'wm_delivery', 'soil_taken', 'lndmng_plough', 'soil_crop',
                   'soil_stones_perc', 'photo_point', 'photo_east', 'photo_west', 'photo_north', 'photo_south', 'cprn_lc', 'office_pi', 'ex_ante', 'car_ew', 'gps_ew', 'lm_plough_slope', 'lm_plough_direct',
                   'lm_stone_walls', 'lm_grass_margins', 'cprn_urban', 'eunis_complex', 'grassland_sample', 'erosion_cando', 'bio_sample', 'soil_bio_taken', 'bulk0_10_sample', 'soil_blk_0_10_taken',
                   'bulk10_20_sample', 'soil_blk_10_20_taken', 'bulk20_30_sample', 'soil_blk_20_30_taken', 'standard_sample', 'soil_std_taken', 'organic_sample', 'soil_org_depth_cando')

  lcols <- c('lc1','lc2','lc1_spec','lc2_spec','lu1','lu2','lu1_type','lu2_type','cprn_lc')

  df.compare <- data.frame(matrix(ncol = 2, nrow = length(colsToCheck)))
  x <- c("variable", "t/f")
  colnames(df.compare) <- x

  valsWrongdf <- data.frame(matrix(NA, nrow = 1000, ncol = 2))
  colnames(valsWrongdf) <- c('variable', 'value')
  valsWrong <- c()

  for(i in 1:length(colsToCheck)){
    # if (colsToCheck[i]=='lc1_perc'){
    #   message('cacaaaaaaa')
    # }
    if(colsToCheck[i] %in% lcols){
      #message(paste0('if ', colsToCheck[i]))
      #before label

      # defaultW <- getOption("warn")
      # options(warn = -1)

      q1 <- dbSendQuery(con, paste("select distinct(", colsToCheck[i], "), count(*) from lucas_harmo_pack group by", colsToCheck[i], " order by", colsToCheck[i] ,";"))
      dfREF <- fetch(q1, n = Inf)

      # options(warn = defaultW)

      #after label

      # defaultW <- getOption("warn")
      # options(warn = -1)

      q2 <- dbSendQuery(con, paste("select distinct(",colsToCheck[i] ,",", paste0(colsToCheck[i],"_label),"), colsToCheck[i], ",  count(*) from lucas_harmo_pack_uf group by",colsToCheck[i],",", paste0(colsToCheck[i],"_label;")))
      dfLUCAS <- fetch(q2, n = Inf)

      # options(warn = defaultW)

      #check values
      allVarlsInVar <- merge(dfREF, dfLUCAS, by = c(colsToCheck[i]))
      colnames(allVarlsInVar) <- c(colsToCheck[i], 'countMerge', paste0(colsToCheck[i], '_label'), 'countUF')
      allVarlsInVar$TF <- ifelse(allVarlsInVar$countMerge == allVarlsInVar$countUF, TRUE, FALSE)
      allVarlsInVarWrong <- allVarlsInVar[allVarlsInVar$TF == FALSE,]
      valsWrongdf$variable[i] <- colsToCheck[i]
      valsWrongdf$value[i] <- paste(allVarlsInVarWrong[[1]], collapse = ',')
      valsWrong <- c(valsWrong, allVarlsInVarWrong[[1]])

      #assign to compare
      df.compare$variable[i] <- colsToCheck[i]
      df.compare$`t/f`[i] <- ifelse(all(dfLUCAS$count == dfREF$count), TRUE, FALSE)
      #message(df.compare$variable[i])

    }else{
      #message(paste0('else ', colsToCheck[i]))
      #before label

      # defaultW <- getOption("warn")
      # options(warn = -1)

      q1 <- dbSendQuery(con, paste("select distinct(", colsToCheck[i], "), count(*) from lucas_harmo_pack group by", colsToCheck[i], " order by", colsToCheck[i] ,";"))
      dfREF <- fetch(q1, n = Inf)

      # options(warn = defaultW)

      #after label

      # defaultW <- getOption("warn")
      # options(warn = -1)

      q1 <- dbSendQuery(con, paste("select distinct(", colsToCheck[i], "), count(*) from lucas_harmo_pack_uf group by", colsToCheck[i], " order by", colsToCheck[i] ,";"))
      dfLUCAS <- fetch(q1, n = Inf)

      # options(warn = defaultW)

      #check values
      allVarlsInVar <- merge(dfREF, dfLUCAS, by = c(colsToCheck[i]))
      colnames(allVarlsInVar) <- c(colsToCheck[i], 'countMerge', 'countUF')
      #message(allVarlsInVar)
      allVarlsInVar$TF <- ifelse(allVarlsInVar$countMerge == allVarlsInVar$countUF, TRUE, FALSE)
      allVarlsInVarWrong <- allVarlsInVar[allVarlsInVar$TF == FALSE,]
      valsWrongdf$variable[i] <- colsToCheck[i]
      valsWrongdf$value[i] <- paste(allVarlsInVarWrong[[1]], collapse = ',')
      #as.list(c(colsToCheck[i], allVarlsInVarWrong[[1]]))
      valsWrong <- c(valsWrong, allVarlsInVarWrong[[1]])

      #assign to compare
      df.compare$variable[i] <- colsToCheck[i]
      df.compare$`t/f`[i] <- ifelse(all(dfREF$count %in% dfLUCAS$count), TRUE, FALSE)
      #message(df.compare$variable[i])
    }
  }
  valsWrongdf <- valsWrongdf[!is.na(valsWrongdf),]
  dfCheck <- df.compare$variable[df.compare$`t/f` == FALSE]

  if(length(dfCheck)==0){
    message('Table lucas_harmo_pack_uf is consistent for all variables.')
  }else{
    stop(message(paste0('Table lucas_harmo_pack_uf is not consistent for variables: ', dfCheck, '.\n')))# More specifically for values : ',valsWrong,'\n')))
  }
}

#' @title Final column order
#' @description Re-order columns of final tables
#' @param con Connection to db
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Final_order_cols(con)}
#' @import RPostgreSQL
#' @export
Final_order_cols <- function(con){
  # library(RPostgreSQL)
  # library(rpostgis)

  # defaultW <- getOption("warn")
  # options(warn = -1)

  q <-dbSendQuery(con, "SELECT * FROM lucas_harmo_pack")
  lucas_harmo_pack <- fetch(q,n = Inf)

  # options(warn = defaultW)

  colOrder <- c('id', 'point_id', 'year', 'nuts0', 'nuts1','nuts2', 'nuts3', 'th_lat', 'th_long', 'office_pi', 'ex_ante', 'survey_date', 'car_latitude',
                'car_ew', 'car_longitude', 'gps_proj', 'gps_prec', 'gps_altitude', 'gps_lat', 'gps_ew', 'gps_long', 'obs_dist', 'obs_direct','obs_type', 'obs_radius',
                'letter_group', 'lc1', 'lc1_spec', 'lc1_perc', 'lc2', 'lc2_spec', 'lc2_perc', 'lu1', 'lu1_type', 'lu1_perc', 'lu2', 'lu2_type',
                'lu2_perc', 'parcel_area_ha', 'tree_height_maturity', 'tree_height_survey', 'feature_width', 'lndmng_plough', 'lm_plough_slope',
                'lm_plough_direct', 'lm_stone_walls', 'crop_residues', 'lm_grass_margins', 'grazing', 'special_status', 'lc_lu_special_remark',
                'cprn_cando', 'cprn_lc', 'cprn_lc1n', 'cprnc_lc1e', 'cprnc_lc1s', 'cprnc_lc1w', 'cprn_lc1n_brdth', 'cprn_lc1e_brdth', 'cprn_lc1s_brdth',
                'cprn_lc1w_brdth', 'cprn_lc1n_next', 'cprn_lc1s_next', 'cprn_lc1e_next', 'cprn_lc1w_next', 'cprn_urban', 'cprn_impervious_perc', 'inspire_plcc1',
                'inspire_plcc2', 'inspire_plcc3', 'inspire_plcc4', 'inspire_plcc5', 'inspire_plcc6', 'inspire_plcc7', 'inspire_plcc8', 'eunis_complex', 'grassland_sample',
                'grass_cando', 'wm', 'wm_source', 'wm_type', 'wm_delivery', 'erosion_cando', 'soil_stones_perc', 'bio_sample', 'soil_bio_taken', 'bulk0_10_sample',
                'soil_blk_0_10_taken', 'bulk10_20_sample', 'soil_blk_10_20_taken', 'bulk20_30_sample', 'soil_blk_20_30_taken', 'standard_sample', 'soil_std_taken',
                'organic_sample', 'soil_org_depth_cando', 'soil_taken', 'soil_crop', 'photo_point', 'photo_north', 'photo_south', 'photo_east', 'photo_west',
                'transect', 'revisit', 'th_gps_dist', 'file_path_gisco_north', 'file_path_gisco_south', 'file_path_gisco_east', 'file_path_gisco_west', 'file_path_gisco_point',
                'gps_geom', 'th_geom', 'trans_geom')
  length(colOrder)==ncol(lucas_harmo_pack)
  lucas_harmo_pack <- lucas_harmo_pack[colOrder]

  #dbSendQuery(con, 'DROP TABLE lucas_harmo_pack;')
  dbWriteTable(con, c("public", "lucas_harmo_pack_final"), value = lucas_harmo_pack, row.names = FALSE)
  rm(lucas_harmo_pack)

  #UF
  # defaultW <- getOption("warn")
  # options(warn = -1)

  q <-dbSendQuery(con, "SELECT * FROM lucas_harmo_pack_uf")
  lucas_harmo_pack_uf <- fetch(q,n = Inf)

  # options(warn = defaultW)

  colOrder_uf <- c('id', 'point_id', 'year', 'nuts0', 'nuts1','nuts2', 'nuts3', 'th_lat', 'th_long', 'office_pi', 'ex_ante', 'survey_date', 'car_latitude',
                   'car_ew', 'car_longitude', 'gps_proj', 'gps_prec', 'gps_altitude', 'gps_lat', 'gps_ew', 'gps_long', 'obs_dist', 'obs_direct','obs_type', 'obs_radius',
                   'letter_group', 'lc1', 'lc1_label' ,'lc1_spec','lc1_spec_label', 'lc1_perc', 'lc2','lc2_label', 'lc2_spec','lc2_spec_label' ,'lc2_perc', 'lu1', 'lu1_label', 'lu1_type','lu1_type_label' , 'lu1_perc', 'lu2','lu2_label' ,'lu2_type','lu2_type_label',
                   'lu2_perc', 'parcel_area_ha', 'tree_height_maturity', 'tree_height_survey', 'feature_width', 'lndmng_plough', 'lm_plough_slope',
                   'lm_plough_direct', 'lm_stone_walls', 'crop_residues', 'lm_grass_margins', 'grazing', 'special_status', 'lc_lu_special_remark',
                   'cprn_cando', 'cprn_lc', 'cprn_lc_label','cprn_lc1n', 'cprnc_lc1e', 'cprnc_lc1s', 'cprnc_lc1w', 'cprn_lc1n_brdth', 'cprn_lc1e_brdth', 'cprn_lc1s_brdth',
                   'cprn_lc1w_brdth', 'cprn_lc1n_next', 'cprn_lc1s_next', 'cprn_lc1e_next', 'cprn_lc1w_next', 'cprn_urban', 'cprn_impervious_perc', 'inspire_plcc1',
                   'inspire_plcc2', 'inspire_plcc3', 'inspire_plcc4', 'inspire_plcc5', 'inspire_plcc6', 'inspire_plcc7', 'inspire_plcc8', 'eunis_complex', 'grassland_sample',
                   'grass_cando', 'wm', 'wm_source', 'wm_type', 'wm_delivery', 'erosion_cando', 'soil_stones_perc', 'bio_sample', 'soil_bio_taken', 'bulk0_10_sample',
                   'soil_blk_0_10_taken', 'bulk10_20_sample', 'soil_blk_10_20_taken', 'bulk20_30_sample', 'soil_blk_20_30_taken', 'standard_sample', 'soil_std_taken',
                   'organic_sample', 'soil_org_depth_cando', 'soil_taken', 'soil_crop', 'photo_point', 'photo_north', 'photo_south', 'photo_east', 'photo_west',
                   'transect', 'revisit', 'th_gps_dist', 'file_path_gisco_north', 'file_path_gisco_south', 'file_path_gisco_east', 'file_path_gisco_west', 'file_path_gisco_point',
                   'gps_geom', 'th_geom', 'trans_geom')
  length(colOrder_uf)==ncol(lucas_harmo_pack_uf)
  colnames(lucas_harmo_pack_uf)[!colnames(lucas_harmo_pack_uf) %in% colOrder_uf]
  lucas_harmo_pack_uf <- lucas_harmo_pack_uf[colOrder_uf]

  #dbSendQuery(con, 'DROP TABLE lucas_harmo_pack_uf;')
  dbWriteTable(con, c("public", "lucas_harmo_pack_uf_final"), value = lucas_harmo_pack_uf, row.names = FALSE)
  rm(lucas_harmo_pack_uf)

  #set the geometry type for the points
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack_uf_final ALTER COLUMN gps_geom type geometry(Point, 4326);')
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack_uf_final ALTER COLUMN th_geom type geometry(Point, 4326);')
  dbSendQuery(con, 'ALTER TABLE lucas_harmo_pack_uf_final ALTER COLUMN trans_geom type geometry(LineString, 3035);')

}

#' @title Remove unwanted columns
#' @description Removes unwanted columns as specified by user
#' @param con Connection to db
#' @param vars Character vector of variables, specified by name to remove
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Remove_vars(con, vars)}
#' @import RPostgreSQL
#' @export
Remove_vars <- function(con, vars){
  # library(RPostgreSQL)
  # library(rpostgis)

  for(i in vars){
    dbSendQuery(con, paste0('ALTER TABLE lucas_harmo_pack_uf_final DROP COLUMN ', i ,';'))
    message(paste('Column ',i,' from table lucas_harmo_pack_uf_final is removed.'))
  }
}


#' @title Update Record descriptor
#' @description Updates Record descriptor by adding a field (year) showing the year for which the variable exists and removing variables listed in Remove_vars function from RD
#' @param con Connection to db
#' @param rd Path to record descriptor csv
#' @param years Character vector of the years of survey
#' @seealso To create the conection please see \link[lucas]{Connect_to_db}
#' @examples \dontrun{
#' Update_rd(con, rd, years)}
#' @import RPostgreSQL
#' @import plyr
#' @import utils read.csv
#' @import utils write.csv
#' @import utils savehistory
#' @import utils glob2rx
#' @export
#update record rescriptor
Update_rd <- function(con, rd, years){
  rd <- read.csv(rd)

  # library(RPostgreSQL)
  # library(rpostgis)
  # library(plyr)

  #get all functions list
  # defaultW <- getOption("warn")
  # options(warn = -1)

  q <- dbSendQuery(con, "select p.oid::regprocedure
              from pg_proc p
                   join pg_namespace n
                   on p.pronamespace = n.oid
             where n.nspname not in ('pg_catalog', 'information_schema');")
  funList <- fetch(q, n=Inf)

  # options(warn = defaultW)

  #check if function has_nonnulls exists, if not - create it
  if(length(funList$oid[grep('has_nonnulls', funList$oid)]) == 0){
    #create function to check if column is only nulls
    dbSendQuery(con, "create function has_nonnulls(p_schema in text, p_table in text, p_column in text)
  returns boolean language plpgsql as $$
declare
  b boolean;
begin
  execute 'select exists(select * from '||
          p_table||' where '||p_column||' is not null)' into b;
  return b;
end;$$;")

    ### FALSE -> All of the values in the column are Null
    ### TRUE -> Not all of the values in the column are Null
  }


  df_check_nulls <- data.frame(matrix(NA, nrow = nrow(rd), ncol = 0))

  for(i in years){

    # defaultW <- getOption("warn")
    # options(warn = -1)

    q <- dbSendQuery(con, paste0("select column_name,
      has_nonnulls(table_schema, table_name, column_name) as ",paste0('lucas_',i),"
 from information_schema.columns
where table_schema='public'
and table_name='lucas_",i,"'
       ;"))
    nulls <- fetch(q, n = Inf)

    # options(warn = defaultW)

    df_check_nulls <- cbind(df_check_nulls, nulls)
  }

  df_check_nulls <- df_check_nulls[,-c(3,5,7,9)]
  #head(df_check_nulls)

  for(i in 1:nrow(df_check_nulls)){
    #if(c(df_check_nulls[i,2:ncol(df_check_nulls)]) == TRUE){
    if(all(as.character(c(df_check_nulls[i,2:(ncol(df_check_nulls)-1)])) == TRUE)){
      df_check_nulls$year[i] <- 'all'
    }else{
      matOfLooseCols <- as.matrix(c(df_check_nulls[i,2:ncol(df_check_nulls)]) == TRUE)
      df_check_nulls$year[i] <- paste(substr(rownames(matOfLooseCols)[which(matOfLooseCols == TRUE)], 7, 11), collapse = ' ')
    }
  }

  colnames(df_check_nulls) <- c('variable', 'lucas_2006', 'lucas_2009', 'lucas_2012', 'lucas_2015', 'lucas_2018', 'collection year')
  rd_new <- merge(rd, df_check_nulls, by = 'variable')
  rd_new <- rd_new[,-c(grep('lucas', colnames(rd_new)))]

  #remove all variables listed in the Remove_vars() function
  savehistory()
  Remove_vars_input <- readLines(".Rhistory")[grepl(glob2rx('Remove_vars*'), readLines(".Rhistory"))][grepl(glob2rx('Remove_vars(con,*'),readLines(".Rhistory")[grepl(glob2rx('Remove_vars*'), readLines(".Rhistory"))])]
  #readLines(".Rhistory")[grepl(glob2rx('Remove_vars*'), readLines(".Rhistory"))]
  vars_ugly <- strsplit(Remove_vars_input, "c\\(")[[1]][2]
  vars_clean <- gsub("'", "",unlist(regmatches(vars_ugly,gregexpr("'[^']*[^']*'",vars_ugly))))

  rd_new <- rd_new[!(rd_new$variable %in% vars_clean), ]
  rd_new <- rd_new[,-which(names(rd_new) %in% c("X"))]

  #for all manually added variables - add here
  #defaultW <- getOption("warn")
  #options(warn = -1)

  q <- dbSendQuery(con, paste0("select table_schema, table_name, column_name
  from information_schema.columns
  where table_schema='public'
  and table_name='lucas_harmo_pack_uf_final';"))
  colsLucasHarmoPackUfFinal <- fetch(q, n = Inf)

  #options(warn = defaultW)

  colsToAddToRd <- as.data.frame(colsLucasHarmoPackUfFinal$column_name[!colsLucasHarmoPackUfFinal$column_name %in% rd_new$variable])
  colnames(colsToAddToRd) <- c('variable')

  colsrd <- c('type', 'NR', 'values', 'documentation', 'Description', 'Comments', 'collection year')
  colsToAddToRd$type <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$NR <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$values <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$documentation <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$Description <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$Comments <- rep(NA, nrow(colsToAddToRd))
  colsToAddToRd$'collection year' <- rep(NA, nrow(colsToAddToRd))

  rd_new <- rbind(rd_new, colsToAddToRd)

  colOrder_uf <- c('id', 'point_id', 'year', 'nuts0', 'nuts1','nuts2', 'nuts3', 'th_lat', 'th_long', 'office_pi', 'ex_ante', 'survey_date', 'car_latitude',
                   'car_ew', 'car_longitude', 'gps_proj', 'gps_prec', 'gps_altitude', 'gps_lat', 'gps_ew', 'gps_long', 'obs_dist', 'obs_direct','obs_type', 'obs_radius',
                   'letter_group', 'lc1', 'lc1_label' ,'lc1_spec','lc1_spec_label', 'lc1_perc', 'lc2','lc2_label', 'lc2_spec','lc2_spec_label' ,'lc2_perc', 'lu1', 'lu1_label', 'lu1_type','lu1_type_label' , 'lu1_perc', 'lu2','lu2_label' ,'lu2_type','lu2_type_label',
                   'lu2_perc', 'parcel_area_ha', 'tree_height_maturity', 'tree_height_survey', 'feature_width', 'lndmng_plough', 'lm_plough_slope',
                   'lm_plough_direct', 'lm_stone_walls', 'crop_residues', 'lm_grass_margins', 'grazing', 'special_status', 'lc_lu_special_remark',
                   'cprn_cando', 'cprn_lc', 'cprn_lc_label','cprn_lc1n', 'cprnc_lc1e', 'cprnc_lc1s', 'cprnc_lc1w', 'cprn_lc1n_brdth', 'cprn_lc1e_brdth', 'cprn_lc1s_brdth',
                   'cprn_lc1w_brdth', 'cprn_lc1n_next', 'cprn_lc1s_next', 'cprn_lc1e_next', 'cprn_lc1w_next', 'cprn_urban', 'cprn_impervious_perc', 'inspire_plcc1',
                   'inspire_plcc2', 'inspire_plcc3', 'inspire_plcc4', 'inspire_plcc5', 'inspire_plcc6', 'inspire_plcc7', 'inspire_plcc8', 'eunis_complex', 'grassland_sample',
                   'grass_cando', 'wm', 'wm_source', 'wm_type', 'wm_delivery', 'erosion_cando', 'soil_stones_perc', 'bio_sample', 'soil_bio_taken', 'bulk0_10_sample',
                   'soil_blk_0_10_taken', 'bulk10_20_sample', 'soil_blk_10_20_taken', 'bulk20_30_sample', 'soil_blk_20_30_taken', 'standard_sample', 'soil_std_taken',
                   'organic_sample', 'soil_org_depth_cando', 'soil_taken', 'soil_crop', 'photo_point', 'photo_north', 'photo_south', 'photo_east', 'photo_west',
                   'transect', 'revisit', 'th_gps_dist', 'file_path_gisco_north', 'file_path_gisco_south', 'file_path_gisco_east', 'file_path_gisco_west', 'file_path_gisco_point')
  colOrder_uf <- colOrder_uf[!colOrder_uf %in% vars_clean]

  if(nrow(rd_new) == length(colOrder_uf)){
    rd_new <- rd_new[match(colOrder_uf, rd_new$variable),]
  }else{
    varsInRDnotInNewColOrder <- as.character(rd_new$variable[! rd_new$variable %in% colOrder_uf])
    varsInNewColOrdernotInRD <- as.character(colOrder_uf[! colOrder_uf %in% rd_new$variable])
    stop(message('Error: number of columns according to Record Descriptor and Database are different. Variables that exist in RD and not in DB are:',varsInRDnotInNewColOrder,". Variables that exist in DB and not in RD are:",varsInNewColOrdernotInRD))
  }

  rd_new <- rd_new[!is.na(rd_new$variable),]
  row.names(rd_new) <- rep(1:nrow(rd_new))
  write.csv(rd_new, file.path(mappings_csv_folder,'LUCAS_harmo_RD_new.csv'))

  message(paste(c('New record descriptor created at path',file.path(mappings_csv_folder,'LUCAS_harmo_RD_new.csv'),'.
            IMPORTANT NOTE: Make sure to populate newly added fields in Record descriptor.
            Newly added fields are:',as.character(colsToAddToRd$variable))))
}
