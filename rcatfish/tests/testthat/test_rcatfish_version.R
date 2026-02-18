#Test the version of the catalog can be retrieved and parsed


test_that("#Test the version of the catalog can be retrieved and parsed",{
  GetVersion <-try(rcatfish_version())
    if ("try-error"%in%class(GetVersion)) {
      skip("could not connect to remote database")
      }else{
        expect_equal(grep(pattern = "Continuously updated since the early 1980s, around the turn of the month.", GetVersion), 1)
        if(lubridate::month(lubridate::now())== 1){#If January, as update is mid-month, check if the version is either the current year or past year 
          expect_in(as.numeric(stringr::str_extract(GetVersion, "\\d{4}")), c(lubridate::year(lubridate::now()),lubridate::year(lubridate::now())-1))
        }else{#else, check current year
          expect_equal(lubridate::year(lubridate::now()), as.numeric(stringr::str_extract(GetVersion, "\\d{4}")))
        }
    }
  }
)
